ui <- function(request) {
    page_navbar(
        title = "Planet Health Index φ",
        theme = bs_theme(
            bootswatch = "minty",
            navbar_bg = primary_color,
            primary = primary_color,
            secondary = secondary_color,
            fg = "black",
            bg = "white"
        ),
        tags$head(
            tags$link(rel = "stylesheet", type = "text/css", href = "main.css"),
            tags$script(HTML("
      var downloadInProgress = false;
      $(document).on('click', '#download_plots', function(ev) {
        if (downloadInProgress) {
          ev.preventDefault();
          return false;
        }
        downloadInProgress = true;
        ev.preventDefault(); // take over the download to detect when the zip is ready
        $('#download-spinner-overlay').addClass('show');

        fetch(this.href)
          .then(function(res) {
            if (!res.ok) {
              throw new Error('Download failed: server returned ' + res.status);
            }
            return res.blob();
          })
          .then(function(blob) {
            // zip is ready on the server: register the wheel and start the browser download
            var a = document.createElement('a');
            a.href = URL.createObjectURL(blob);
            a.download = 'planetary-health-index.zip';
            document.body.appendChild(a);
            a.click();
            setTimeout(function() { URL.revokeObjectURL(a.href); a.remove(); }, 1000);
          })
          .catch(function(err) {
            console.error(err);
          })
          .finally(function() {
            downloadInProgress = false;
            $('#download-spinner-overlay').removeClass('show');
          });

        return false;
      });
    ")),
        ),
        sidebar = sidebar(
            radioButtons(
                "x_sphere", "Source sphere",
                choiceNames = lapply(spheres, function(s) tags$span(s, class = s)),
                choiceValues = spheres,
                selected = "bio"
            ),
            radioButtons(
                "y_sphere", "Target sphere",
                choiceNames = lapply(spheres, function(s) tags$span(s, class = s)),
                choiceValues = spheres,
                selected = "socio"
            ),
            checkboxGroupInput(
                "detrend_methods", "Detrend methods",
                choices = c(
                    "Remove quarterly effect" = "quarterly",
                    "Remove annual effect" = "annual",
                    "Remove spatial effect" = "spatial"
                ),
                selected = c("quarterly", "annual")
            ),
            selectInput(
                "scaling_grouping", "z-scaling grouping",
                choices = c("feature", "feature and region"),
                selected = "feature"
            ),
            p("Two CCAs will be performed: from the source sphere to the target sphere and from the target sphere to the source sphere.")
        ),
        nav_panel(
            title = "Home",
            includeHTML("www/home.html")
        ),
        nav_panel(
            title = "Features",
            h3("Used features"),
            p("Click and start typing to add new features to be used in the analysis. Those features will be detrended by default. Click on a feature item and press the delete key to remove it."),
            fluidRow(
                column(6, selectizeInput(
                    "used_features", "Use features",
                    choices = features$label, selected = all_preselected_features, multiple = TRUE,
                    width = "100%",
                    options = list(render = feature_color_render())
                )),
                column(6, selectizeInput(
                    "detrended_features", "Detrend features",
                    choices = features$label, selected = all_preselected_features, multiple = TRUE,
                    width = "100%",
                    options = list(render = feature_color_render())
                ))
            ),
            h3("Available features"),
            tableOutput("features_table")
        ),
        nav_panel(
            title = "Spheres",
            h3("Scores between spheres"),
            h4("CCA1"),
            fluidRow(
                column(
                    6,
                    withSpinner(plotOutput("scores_plt", height = "500px"))
                ),
                column(
                    6,
                    withSpinner(plotOutput("loadings_cca1_plt", height = "500px"))
                )
            ),
            h4("CCA2"),
            fluidRow(
                column(
                    6,
                    withSpinner(plotOutput("scores_cca2_plt", height = "500px"))
                ),
                column(
                    6,
                    withSpinner(plotOutput("loadings_cca2_plt", height = "500px"))
                )
            ),
            fluidRow(
                textInput(
                    "highlight_str", "Highlight NUTS region or year",
                    value = ""
                )
            )
        ),
        nav_panel(
            title = "Spatial",
            h3("Spatial distribution"),
            fluidRow(
                selectizeInput("selected_feature", "Feature:", choices = features$label, options = list(render = feature_color_render())),
                sliderInput("selected_year", "Year:", min = 2001, max = 2021, value = 2021, sep = ""),
                selectInput("selected_quarter", "Quarter:", choices = c("Q1", "Q2", "Q3", "Q4"))
            ),
            plotOutput("map_plt", height = "1000px")
        ),
        nav_panel(
            title = "Temporal",
            h3("Temporal distribution"),
            fluidRow(
                column(
                    3,
                    selectInput("selected_geo", "Regions:", choices = nuts3_regions$label, selected = c("Berlin", "Paris"), multiple = TRUE)
                ),
                column(
                    3,
                    selectizeInput(
                        "selected_feature_for_timeseries", "Features:",
                        choices = features$label, multiple = TRUE,
                        options = list(render = feature_color_render())
                    )
                ),
                column(
                    3,
                    sliderInput(
                        "highlight_year_range", "Highlight year range:",
                        min = 2001, max = 2021, value = c(2001, 2021), sep = ""
                    )
                ),
                column(
                    1,
                    selectInput(
                        "highlight_start_quarter", "Start Q",
                        choices = c("Q1", "Q2", "Q3", "Q4"), selected = "Q1"
                    )
                ),
                column(
                    1,
                    selectInput(
                        "highlight_end_quarter", "End Q",
                        choices = c("Q1", "Q2", "Q3", "Q4"), selected = "Q4"
                    )
                )
            ),
            withSpinner(plotOutput("timeseries_plt")),
            fluidRow(
                column(
                    6,
                    withSpinner(plotOutput("trajectories_fwd_plt", height = "800px"))
                ),
                column(
                    6,
                    withSpinner(plotOutput("trajectories_rev_plt", height = "800px"))
                )
            )
        ),
        nav_panel(
            title = "Save",
            h3("Save"),
            p("Save inputs by updating the state in the URL:"),
            bookmarkButton(),
            p("Download inputs and most important plots. May take a minute to process results."),
            downloadButton("download_plots", "Download"),
            div(
                id = "download-spinner-overlay",
                div(class = "spinner"),
                div(class = "spinner-label", "Preparing download...")
            )
        ),
        nav_item(
            tags$a(
                href = "https://www.bgc-jena.mpg.de/2299/imprint",
                "Imprint",
                target = "_blank",
                class = "nav-link",
                rel = "noopener noreferrer"
            )
        )
    )
}
