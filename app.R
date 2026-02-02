library(shiny)
library(tidyverse)
library(bslib)
library(plotly)
library(ggsci)
library(targets)
library(shinycssloaders)
library(ncdf4)
library(lubridate)
library(eurostat)
library(rnaturalearth)
library(khroma)
source("lib.R")

shinyOptions(cache = cachem::cache_mem(max_size = 1e9))

nuts3_regions <- read_csv("data/nuts3_regions.csv")

nuts3_sf <- get_eurostat_geospatial(
  output_class = "sf",
  resolution = "20",
  nuts_level = "3",
  year = "2024"
)

tar_load(cube)
tar_load(detrended_cube)
tar_load(features)
withSpinner <- partial(shinycssloaders::withSpinner, color = primary_color, type = 8)
theme_set(
  theme_classic(base_size = 18) + theme(
    legend.position = "bottom"
  )
)
quarters <- seq(
  from = ymd("2001-01-01"),
  to   = ymd("2025-12-31"),
  by   = "3 months"
)

ui <- page_navbar(
  title = "Planetary Health Index φ",
  theme = bs_theme(
    bootswatch = "minty",
    navbar_bg = primary_color,
    primary = primary_color,
    fg = "black",
    bg = "white"
  ),
  tags$head(
    tags$style(HTML("
      h3 {
        color: #006c66
      }
      .tab-content.html-fill-container, .navbar-header {
        padding-left: 1.5em;
        padding-right: 1.5em;
      }
    "))
  ),
  nav_panel(
    title = "Input",
    div(paste0(
      "The Planetary Health Index φ is a concept to explain linear relationships of a set of features or spheres using another one, ",
      "e.g., to model socioeconomic features using biological measurements. Hereby, Canonical Correlation Analysis is used ",
      "to model a set of related features holistically, whereas traditional Pearson Correlation focuses on the relationship ",
      "between two individual features. Data was collected from Eurostat, ERA5, and FluxCom."
    )),
    h3("Input"),
    fluidRow(
      radioButtons(
        "x_sphere", "Source sphere",
        choices = spheres, selected = "bio"
      ),
      radioButtons(
        "y_sphere", "Target sphere",
        choices = spheres, selected = "socio"
      ),
      selectInput(
        "used_features", "Use features",
        choices = features$label, selected = features$label, multiple = TRUE
      ),
      selectInput(
        "detrended_features", "Detrend features",
        choices = features$label, selected = features$label, multiple = TRUE
      )
    ),
    h3("Features"),
    tableOutput("features_table")
  ),
  nav_panel(
    title = "Output",
    h3("Sphere relationships"),
    fluidRow(
      textInput(
        "highlight_str", "Highlight NUTS region or year",
        value = "BE"
      )
    ),
    fluidRow(
      splitLayout(
        cellWidths = c("33%", "66%"),
        withSpinner(plotOutput("scores_plt")),
        withSpinner(plotOutput("trajectories_plt"))
      )
    ),
    fluidRow(
      withSpinner(plotOutput("loadings_plt"))
    ),
    h3("Spatial distribution"),
    fluidRow(
      selectInput("selected_feature", "Feature:", choices = features$label),
      sliderInput("selected_year", "Year:", min = 2001, max = 2021, value = 2021),
      selectInput("selected_quarter", "Quarter:", choices = c("Q1", "Q2", "Q3", "Q4"))
    ),
    withSpinner(plotOutput("map_plt", height = "1000px")),
    h3("Temporal distribution"),
    selectInput("selected_geo", "Region:", choices = nuts3_regions$geo3),
    withSpinner(plotOutput("timeseries_plt")),
  )
)

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

      features <-
        features |>
        filter(sphere %in% c(input$x_sphere, input$y_sphere)) |>
        pull(label)

      updateSelectInput(
        session,
        "used_features",
        choices = features,
        selected = features
      )

      updateSelectInput(
        session,
        "selected_feature",
        choices = c("fwd_CCA1", "rev_CCA1", "fwd_CCA2", "rev_CCA2") |> append(features),
        selected = "fwd_CCA1"
      )
    }
  )

  observeEvent(
    input$used_features,
    {
      updateSelectInput(
        session,
        "detrended_features",
        choices = input$used_features,
        selected = intersect(input$detrended_features, input$used_features)
      )
    }
  )

  highlighted_data <- reactive(
    ~ filter(.x, str_detect(name, input$highlight_str))
  )

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

    cbind(detrended_cube[, detrended_features], cube[, other_features])
  }) |> bindCache(input$used_features, input$detrended_features)

  cca_fwd <- reactive(calculate_cca(processed_cube(), x_features(), y_features())) |>
    bindCache(input$x_sphere, input$y_sphere, input$used_features, input$detrended_features)
  cca_rev <- reactive(calculate_cca(processed_cube(), y_features(), x_features())) |>
    bindCache(input$x_sphere, input$y_sphere, input$used_features, input$detrended_features)

  output$features_table <- features |>
    select(-var_id) |>
    renderTable() |>
    bindCache(1)

  output$scores_plt <- renderPlot(
    bg = "transparent",
    {
      inner_join(
        cca_fwd()$scores |> select(fwd = CCA1, geo, time),
        cca_rev()$scores |> select(rev = CCA1, geo, time)
      ) |>
        unite("name", geo, time) |>
        ggplot(aes(fwd, rev)) +
        geom_density2d_filled() +
        scale_fill_grey(start = 1, end = 0, t) +
        geom_abline(color = dark_gray_color) +
        geom_point(
          data = highlighted_data(),
          color = primary_color,
          alpha = 0.3,
          size = 1
        ) +
        geom_density_2d(
          data = highlighted_data(),
          color = primary_color
        ) +
        coord_fixed() +
        guides(fill = "none") +
        labs(x = paste0(input$x_sphere, "-", input$y_sphere), y = paste0(input$y_sphere, "-", input$x_sphere))
    }
  ) |> bindCache(input$x_sphere, input$y_sphere, input$used_features, input$detrended_features, input$highlight_str)

  output$loadings_plt <- renderPlot({
    bind_rows(
      cca_fwd()$loadings,
      cca_rev()$loadings
    ) |>
      left_join(features) |>
      ggplot(aes(label, CCA1)) +
      geom_bar(stat = "identity") +
      geom_hline(yintercept = 0) +
      coord_flip() +
      labs(x = "Feature")
  }) |> bindCache(input$x_sphere, input$y_sphere, input$used_features, input$detrended_features, input$highlight_str)

  output$trajectories_plt <- renderPlot({
    bind_rows(
      cca_fwd()$scores |> mutate(direction = paste0(input$x_sphere, "-", input$y_sphere)),
      cca_rev()$scores |> mutate(direction = paste0(input$y_sphere, "-", input$x_sphere))
    ) |>
      mutate(name = paste0(geo, time)) |>
      ggplot(aes(CCA1, CCA2)) +
      geom_density2d_filled() +
      scale_fill_grey(start = 1, end = 0) +
      geom_line(
        data = highlighted_data(),
        mapping = aes(group = geo),
        color = dark_gray_color
      ) +
      geom_density_2d(
        data = highlighted_data(),
        color = primary_color
      ) +
      geom_density_2d(color = dark_gray_color) +
      scale_color_hue(l = 40) +
      coord_fixed() +
      facet_wrap(~direction) +
      guides(fill = "none") +
      labs(color = "NUTS region")
  }) |> bindCache(
    input$x_sphere, input$y_sphere, input$used_features, input$detrended_features,
    input$highlight_str, input$highlight_str
  )

  # Load land and ocean data
  ocean_sf <- ne_download(scale = 50, type = "ocean", category = "physical", returnclass = "sf")
  land_sf <- ne_countries(scale = "medium", returnclass = "sf")

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

    nuts3_sf |>
      left_join(cur_data) |>
      ggplot() +
      geom_sf(data = land_sf, fill = light_gray_color, color = light_gray_color) +
      geom_sf(aes(fill = value), color = dark_gray_color) +
      scale_fill_gradientn(colours = color("vik")(10), na.value = light_gray_color) +
      coord_sf(
        xlim = c(2377294, 7453440),
        ylim = c(1313597, 5628510),
        crs = 3035
      ) +
      theme(
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()
      ) +
      labs(fill = input$selected_feature)
  })

  output$timeseries_plt <- renderPlot({
    cur_feature_data <-
      processed_cube() |>
      as_tibble(rownames = "space_time") |>
      separate(space_time, c("geo", "time"), sep = "_") |>
      filter(geo == input$selected_geo) |>
      pivot_longer(-c(geo, time), names_to = "feature", values_to = "value")

    cur_cca_data <-
      inner_join(
        cca_fwd()$scores |> select(fwd_CCA1 = CCA1, fwd_CCA2 = CCA2, geo, time),
        cca_rev()$scores |> select(rev_CCA1 = CCA1, rev_CCA2 = CCA2, geo, time)
      ) |>
      filter(geo == input$selected_geo) |>
      pivot_longer(cols = -c(geo, time), names_to = "feature", values_to = "value")

    cur_data <-
      bind_rows(cur_feature_data, cur_cca_data) |>
      mutate(time = yq(time))

    cur_data |>
      ggplot(aes(time, value, color = feature)) +
      geom_line() +
      scale_color_hue(l = 40)
  })
}

shinyApp(ui = ui, server = server)
