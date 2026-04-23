server <- function(input, output, session) {
    paste0(
        "This app shows preliminary results ",
        "for demonstration purposes only."
    ) |>
        showNotification(duration = Inf, type = "warning")

    observeEvent(
        list(input$x_sphere, input$y_sphere),
        {
            if (input$x_sphere == input$y_sphere) {
                showNotification("Please select different spheres!", type = "error")
            }
        }
    )

    current_url <- reactiveVal()
    onBookmarked(function(state) {
        updateQueryString(state)
        state |> current_url()
    })

    hidden_features <- c("q", "ST")


    # features of the selected spheres
    possible_features <- reactive({
        features |>
            filter(sphere %in% c(input$x_sphere, input$y_sphere)) |>
            pull(label) |>
            setdiff(hidden_features)
    })

    # usful subset of uncorrelated possible features
    preselected_features <- reactive({
        features |>
            filter(var_id %in% all_preselected_features) |>
            pull(label) |>
            intersect(possible_features())
    })

    # Set feature choices to possible ones
    observeEvent(
        eventExpr = list(input$x_sphere, input$y_sphere),
        handlerExpr = {
            updateSelectInput(
                session,
                "used_features",
                choices = possible_features(),
                selected = preselected_features()
            )

            updateSelectInput(
                session,
                "detrended_features",
                choices = possible_features(),
                selected = preselected_features()
            )
        }
    )

    # allow to only visualize features used in actual analysis
    observeEvent(
        input$used_features,
        {
            updateSelectInput(
                session,
                "detrended_features",
                choices = input$used_features,
                selected = input$used_features
            )

            updateSelectInput(
                session,
                "selected_feature",
                choices = c("fwd_CCA1", "rev_CCA1", "fwd_CCA2", "rev_CCA2") |> append(input$used_features),
                selected = "fwd_CCA1"
            )

            updateSelectInput(
                session,
                "selected_feature_for_timeseries",
                choices = c("fwd_CCA1", "rev_CCA1", "fwd_CCA2", "rev_CCA2") |> append(input$used_features),
                selected = "fwd_CCA1"
            )
        }
    )

    highlighted_data <- reactive({
        if (input$highlight_str != "") {
            ~ filter(.x, str_detect(name, input$highlight_str))
        } else {
            # highlight nothing
            ~ filter(.x, FALSE)
        }
    })

    x_features <- reactive({
        features |>
            filter(sphere == input$x_sphere & label %in% input$used_features) |>
            pull(var_id)
    })

    y_features <- reactive({
        features |>
            filter(sphere == input$y_sphere & label %in% input$used_features) |>
            pull(var_id)
    })

    processed_cube <- reactive({
        detrended_features <-
            features |>
            filter(label %in% input$used_features & label %in% input$detrended_features) |>
            pull(var_id)

        other_features <-
            features |>
            filter(label %in% input$used_features & !label %in% input$detrended_features) |>
            pull(var_id)

        normalize_cube(
            cube_tbl, global_stats, annual_stats, quarterly_stats, geo_stats,
            detrended_features, other_features, input$detrend_methods, input$scaling_grouping
        )
    }) |> bindCache(input$used_features, input$detrended_features, input$detrend_methods, input$scaling_grouping)

    cca_fwd <- reactive(calculate_cca(processed_cube(), x_features(), y_features())) |>
        bindCache(input$x_sphere, input$y_sphere, input$used_features, input$detrended_features, input$detrend_methods, input$scaling_grouping)
    cca_rev <- reactive(calculate_cca(processed_cube(), y_features(), x_features())) |>
        bindCache(input$x_sphere, input$y_sphere, input$used_features, input$detrended_features, input$detrend_methods, input$scaling_grouping)

    output$features_table <- renderTable({
        features |>
            filter(!var_id %in% hidden_features) |>
            select(sphere, label, source, temporal_resolution, spatial_resolution, description) |>
            arrange(sphere, label)
    }) |> bindCache(1)

    scores_plt <- reactive({
        data <-
            inner_join(
                cca_fwd()$scores |> select(fwd = CCA1, geo, time),
                cca_rev()$scores |> select(rev = CCA1, geo, time)
            ) |>
            unite("name", geo, time)

        r <- cor.test(data$fwd, data$rev, method = "pearson")$estimate

        data |>
            ggplot(aes(fwd, rev)) +
            geom_abline(color = dark_gray_color) +
            geom_point(
                data = highlighted_data(),
                color = primary_color,
                alpha = 0.3,
                size = 1
            ) +
            geom_density_2d(
                data = highlighted_data(),
                mapping = aes(color = "highlighted"),
            ) +
            stat_density_2d(contour = TRUE, mapping = aes(color = "all")) +
            scale_color_manual(values = c("all" = "darkgrey", "highlighted" = primary_color)) +
            coord_fixed() +
            labs(
                x = paste0(input$x_sphere, "-", input$y_sphere),
                y = paste0(input$y_sphere, "-", input$x_sphere),
                color = "Sample group",
                subtitle = paste0("Pearson r=", round(r, 2) |> abs())
            )
    })

    output$scores_plt <- renderPlot(
        bg = "transparent",
        scores_plt()
    ) |> bindCache(input$x_sphere, input$y_sphere, input$used_features, input$detrended_features, input$highlight_str, input$detrend_methods, input$scaling_grouping)

    loadings_cca1_fwd_plt <- reactive(plot_loadings(cca_fwd()$loadings, "CCA1", "FWD CCA1 loading"))
    output$loadings_cca1_fwd_plt <- renderPlot(loadings_cca1_fwd_plt())

    loadings_cca2_fwd_plt <- reactive(plot_loadings(cca_fwd()$loadings, "CCA2", "FWD CCA2 loading"))
    output$loadings_cca2_fwd_plt <- renderPlot(loadings_cca2_fwd_plt())

    loadings_cca1_rev_plt <- reactive(plot_loadings(cca_rev()$loadings, "CCA1", "REV CCA1 loading"))
    output$loadings_cca1_rev_plt <- renderPlot(loadings_cca1_rev_plt())

    loadings_cca2_rev_plt <- reactive(plot_loadings(cca_rev()$loadings, "CCA2", "REV CCA2 loading"))
    output$loadings_cca2_rev_plt <- renderPlot(loadings_cca2_rev_plt())

    trajectories_fwd_plt <- reactive({
        cca_fwd()$scores |>
            left_join(nuts3_regions |> rename(geo_label = label)) |>
            arrange(geo, time) |>
            distinct(geo, CCA1, CCA2, .keep_all = TRUE) |>
            ggplot(aes(CCA1, CCA2)) +
            geom_path(
                data = ~ filter(.x, geo_label %in% input$selected_geo),
                mapping = aes(group = geo, color = geo_label),
                arrow = arrow(ends = "last")
            ) +
            coord_fixed() +
            guides(fill = "none") +
            labs(title = paste0(input$x_sphere, "-", input$y_sphere), color = "Region")
    })

    output$trajectories_fwd_plt <- renderPlot(trajectories_fwd_plt()) |> bindCache(
        input$x_sphere, input$y_sphere, input$used_features, input$detrended_features,
        input$selected_geo, input$detrend_methods, input$scaling_grouping
    )

    trajectories_rev_plt <- reactive({
        cca_rev()$scores |>
            left_join(nuts3_regions |> rename(geo_label = label), by = join_by(geo)) |>
            arrange(geo, time) |>
            distinct(geo, CCA1, CCA2, .keep_all = TRUE) |>
            ggplot(aes(CCA1, CCA2)) +
            geom_path(
                data = ~ filter(.x, geo_label %in% input$selected_geo),
                mapping = aes(group = geo, color = geo_label),
                arrow = arrow(ends = "last")
            ) +
            coord_fixed() +
            guides(fill = "none") +
            labs(title = paste0(input$y_sphere, "-", input$x_sphere), color = "Region")
    })

    output$trajectories_rev_plt <- renderPlot(trajectories_rev_plt()) |> bindCache(
        input$x_sphere, input$y_sphere, input$used_features, input$detrended_features,
        input$selected_geo, input$detrend_methods, input$scaling_grouping
    )


    output$map_plt <- renderPlot({
        cur_time <- paste0(input$selected_year, "-", input$selected_quarter)

        # get data: either feature or CCA scores
        if (input$selected_feature %in% features$label) {
            cur_feature <-
                features |>
                filter(label == input$selected_feature) |>
                pull(var_id) |>
                first()

            cur_data <-
                tibble(
                    space_time = rownames(processed_cube()),
                    value = processed_cube()[, cur_feature]
                ) |>
                separate(space_time, c("geo", "time"), sep = "_") |>
                filter(time == cur_time)
        } else {
            cur_data <-
                inner_join(
                    cca_fwd()$scores |> select(fwd_CCA1 = CCA1, fwd_CCA2 = CCA2, geo, time),
                    cca_rev()$scores |> select(rev_CCA1 = CCA1, rev_CCA2 = CCA2, geo, time)
                ) |>
                filter(time == cur_time) |>
                pivot_longer(cols = -c(geo, time), names_to = "feature", values_to = "value") |>
                filter(feature == input$selected_feature)
        }

        max_val <-
            cur_data$value |>
            abs() |>
            max()

        nuts3_sf |>
            left_join(cur_data, by = join_by(geo)) |>
            ggplot() +
            geom_sf(data = land_sf, fill = light_gray_color, color = light_gray_color) +
            geom_sf(aes(fill = value), color = dark_gray_color) +
            scale_fill_gradientn(
                colours = color("vik")(10),
                na.value = light_gray_color,
                limits = c(-max_val, max_val)
            ) +
            coord_sf(
                xlim = c(2377294, 7453440),
                ylim = c(1313597, 5628510),
                crs = 3035
            ) +
            theme(
                axis.line = element_blank(),
                axis.text = element_blank(),
                axis.ticks = element_blank(),
                legend.key.width = unit(3, "cm")
            ) +
            labs(fill = input$selected_feature)
    })

    output$timeseries_plt <- renderPlot({
        selected_geos <-
            nuts3_regions |>
            filter(label %in% input$selected_geo) |>
            pull(geo)

        cur_feature_data <-
            processed_cube() |>
            as_tibble(rownames = "space_time") |>
            separate(space_time, c("geo", "time"), sep = "_") |>
            filter(geo %in% selected_geos) |>
            pivot_longer(-c(geo, time), names_to = "var_id", values_to = "value")

        cur_cca_data <-
            inner_join(
                cca_fwd()$scores |> select(fwd_CCA1 = CCA1, fwd_CCA2 = CCA2, geo, time),
                cca_rev()$scores |> select(rev_CCA1 = CCA1, rev_CCA2 = CCA2, geo, time),
                by = join_by(geo, time)
            ) |>
            filter(geo %in% selected_geos) |>
            pivot_longer(cols = -c(geo, time), names_to = "var_id", values_to = "value")

        cca_features <- tibble(
            var_id = c("fwd_CCA1", "fwd_CCA2", "rev_CCA1", "rev_CCA2"),
            label = c("fwd_CCA1", "fwd_CCA2", "rev_CCA1", "rev_CCA2")
        )

        cur_data <-
            bind_rows(cur_feature_data, cur_cca_data) |>
            left_join(features |> bind_rows(cca_features), by = join_by(var_id)) |>
            left_join(nuts3_regions |> rename(geo_label = label), by = join_by(geo)) |>
            filter(label %in% input$selected_feature_for_timeseries) |>
            mutate(time = yq(time))

        cur_data |>
            ggplot(aes(time, value, color = geo_label, linetype = label)) +
            geom_line() +
            theme(legend.position = "bottom", legend.direction = "vertical") +
            labs(y = "z-score", linetype = "Feature", color = "Region")
    })

    output$download_plots <- downloadHandler(
        filename = "planetary-health-index.zip",
        content = function(zip_path) {
            session$doBookmark() # save state to go back afetr download

            tmp_dir <- tempfile("bundle_")
            dir.create(tmp_dir)
            on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

            inputs_file <- file.path(tmp_dir, "inputs.yml")
            inputs <- reactiveValuesToList(input)
            inputs[["url"]] <- current_url()
            yaml::write_yaml(inputs, inputs_file)

            scores_file <- file.path(tmp_dir, "scores.png")
            ggsave(scores_file, plot = scores_plt())


            loadings_cca1_fwd_file <- file.path(tmp_dir, "loadings_cca1_fwd.png")
            ggsave(loadings_cca1_fwd_file, plot = loadings_cca1_fwd_plt(), width = 18)

            loadings_cca2_fwd_file <- file.path(tmp_dir, "loadings_cca2_fwd.png")
            ggsave(loadings_cca2_fwd_file, plot = loadings_cca2_fwd_plt(), width = 18)

            loadings_cca1_rev_file <- file.path(tmp_dir, "loadings_cca1_rev.png")
            ggsave(loadings_cca1_rev_file, plot = loadings_cca1_rev_plt(), width = 18)

            loadings_cca2_rev_file <- file.path(tmp_dir, "loadings_cca2_rev.png")
            ggsave(loadings_cca2_rev_file, plot = loadings_cca2_rev_plt(), width = 18)


            trajectories_fwd_file <- file.path(tmp_dir, "trajectories_fwd.png")
            ggsave(trajectories_fwd_file, plot = trajectories_fwd_plt())

            trajectories_rev_file <- file.path(tmp_dir, "trajectories_rev.png")
            ggsave(trajectories_rev_file, plot = trajectories_rev_plt())

            utils::zip(
                zipfile = zip_path,
                files = c(
                    loadings_cca1_fwd_file, loadings_cca2_fwd_file, loadings_cca1_rev_file, loadings_cca2_rev_file,
                    inputs_file, trajectories_fwd_file, trajectories_rev_file, scores_file
                ),
                flags = "-j" # removes directory paths inside the zip
            )
        }
    )
}
