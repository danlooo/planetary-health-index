library(shiny)
library(tidyverse)
library(bslib)
library(plotly)
library(ggsci)
library(targets)
library(shinycssloaders)
library(ncdf4)

source("lib.R")

nuts3_regions <- read_csv("data/nuts3_regions.csv")
tar_load(cube)
withSpinner <- partial(shinycssloaders::withSpinner, color = primary_color, type = 8)
theme_set(
  theme_classic(base_size = 18) + theme(
    legend.position = "bottom"
  )
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
  sidebar = sidebar(
    radioButtons("x_sphere", "Source sphere", choices = spheres, selected = "bio"),
    radioButtons("y_sphere", "Target sphere", choices = spheres, selected = "socio"),
    textInput("highlight_str", "Highlight NUTS region or year", value = "BE"),
    selectInput("used_features", "Use features", features$label, selected = features$label, multiple = TRUE)
  ),
  nav_panel(
    title = "Overview",
    div(paste0(
      "The Planetary Health Index φ is a concept to explain linear relationships of a set of features or spheres using another one, ",
      "e.g., to model socioeconomic features using biological measurements. Hereby, Canonical Correlation Analysis is used ",
      "to model a set of related features holistically, whereas traditional Pearson Correlation focuses on the relationship ",
      "between two individual features. Data was collected from Eurostat, ERA5, and FluxCom."
    )),
    h3("Features"),
    tableOutput("features_table")
  ),
  nav_panel(
    title = "Scores",
    div(paste0(
      "One feature sphere is used to predict another one, and vice versa. ",
      "Each point represents the first CCA component of a NUTS3 region at a given quarter. ",
      "Many points close to the grey identity line indicate that the feature spheres are highly linearly related."
    )),
    withSpinner(
      plotOutput("scores_plt", height = "80vh")
    )
  ),
  nav_panel(
    title = "Loadings",
    div(paste0(
      "One feature sphere is used to predict another one, and vice versa. ",
      "Each bar represents the importance of that feature in the CCA."
    )),
    withSpinner(
      plotOutput("loadings_plt")
    )
  ),
  nav_panel(
    title = "Trajectories",
    div(paste0(
      "One feature sphere is used to predict another one, and vice versa. ",
      "Each point represents the first and second CCA component of a NUTS3 region at a given quarter. "
    )),
    withSpinner(
      plotOutput("trajectories_plt", height = "80vh")
    )
  )
)

server <- function(input, output, session) {
  paste0(
    "This app shows preliminary results ",
    "for demonstration purposes only."
  ) |>
    showNotification(duration = Inf, type = "warning")

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
  
  cca_fwd <- reactive(calculate_cca(cube, x_features(), y_features()))
  cca_rev <- reactive(calculate_cca(cube, y_features(), x_features()))
  
  output$features_table <- renderTable(features |> select(sphere, var_id, label))

  output$scores_plt <- renderPlot(
    bg = "transparent",
    {
      inner_join(
        cca_fwd()$scores |> select(fwd=CCA1, geo, time),
        cca_rev()$scores |> select(rev=CCA1, geo, time)
      ) |>
        unite("name", geo,time) |>
        ggplot(aes(fwd, rev)) +
        geom_abline(color = "darkgrey") +
        geom_point(color = "darkgrey", alpha = 0.03) +
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
        geom_density_2d(color = "#333333") +
        coord_fixed() +
        guides(fill = "none") +
        labs(x = paste0(input$x_sphere, "-", input$y_sphere), y = paste0(input$y_sphere, "-", input$x_sphere))
    }
  )

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
  })

  output$trajectories_plt <- renderPlot({
    bind_rows(
      cca_fwd()$scores |> mutate(direction= paste0(input$x_sphere, "-", input$y_sphere)),
      cca_rev()$scores |> mutate(direction= paste0(input$y_sphere, "-", input$x_sphere))
    ) |>
      mutate(name = paste0(geo, time)) |>
      ggplot(aes(CCA1, CCA2)) +
      geom_point(
        color = "darkgrey",
        alpha = 0.03
      ) +
      geom_line(
        data = highlighted_data(),
        mapping = aes(color = geo),
      ) +
      geom_density_2d(
        data = highlighted_data(),
        color = primary_color
      ) +
      geom_density_2d(color = "#333333") +
      scale_color_hue(l = 40) +
      coord_fixed() +
      facet_wrap(~direction) +
      labs(color = "NUTS region")
  })
}

shinyApp(ui = ui, server = server)
