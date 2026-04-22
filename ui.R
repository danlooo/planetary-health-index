ui <- function(request) {
    page_navbar(
        title = "Planetary Health Index φ",
        theme = bs_theme(
            bootswatch = "minty",
            navbar_bg = primary_color,
            primary = primary_color,
            secondary = secondary_color,
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
      .btn {
        max-width: 500px
      }
    "))
        ),
        sidebar = sidebar(
            radioButtons(
                "x_sphere", "Source sphere",
                choices = spheres, selected = "bio"
            ),
            radioButtons(
                "y_sphere", "Target sphere",
                choices = spheres, selected = "socio"
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
            selectInput("scaling_grouping", "z-scaling grouping", choices = c("feature", "feature and region"), selected = "feature")
        ),
        nav_panel(
            title = "Home",
            div(paste0(
                "The Planetary Health Index φ is a concept to explain linear relationships of a set of features or spheres using another one, ",
                "e.g., to model socioeconomic features using biological measurements. Hereby, Canonical Correlation Analysis is used ",
                "to model a set of related features holistically, whereas traditional Pearson Correlation focuses on the relationship ",
                "between two individual features. Data was collected from Eurostat, ERA5, and FluxCom."
            ))
        ),
        nav_panel(
          title = "Features",
          h3("Used Features"),
          fluidRow(
            column(6, selectInput(
              "used_features", "Use features",
              choices = features$label, selected = all_preselected_features, multiple = TRUE,
              width = "100%"
            )),
            column(6, selectInput(
              "detrended_features", "Detrend features",
              choices = features$label, selected = all_preselected_features, multiple = TRUE,
              width = "100%"
            ))
          ),
          h3("Available features"),
          tableOutput("features_table")
        ),
        nav_panel(
            title = "Spheres",
            h3("Sphere relationships"),
            fluidRow(
                textInput(
                    "highlight_str", "Highlight NUTS region or year",
                    value = "BE"
                )
            ),
            withSpinner(plotOutput("scores_plt")),
            withSpinner(plotOutput("loadings_plt"))
        ),
        nav_panel(
            title = "Spatial",
            h3("Spatial distribution"),
            fluidRow(
                selectInput("selected_feature", "Feature:", choices = features$label),
                sliderInput("selected_year", "Year:", min = 2001, max = 2021, value = 2021),
                selectInput("selected_quarter", "Quarter:", choices = c("Q1", "Q2", "Q3", "Q4"))
            ),
            plotOutput("map_plt", height = "1000px")
        ),
        nav_panel(
            title = "Temporal",
            h3("Temporal distribution"),
            fluidRow(
                selectInput("selected_geo", "Regions:", choices = nuts3_regions$label, selected = c("Berlin", "Paris"), multiple = TRUE),
                selectInput("selected_feature_for_timeseries", "Features:", choices = features$label, multiple = TRUE)
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
            downloadButton("download_plots", "Download")
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
