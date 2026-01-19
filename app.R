library(shiny)
library(tidyverse)
library(bslib)
library(plotly)
library(ggsci)
library(targets)
library(shinycssloaders)
library(ncdf4)

source("lib.R")

tar_load(nuts3_regions)

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
      plotOutput("loadings_plt", height = "80vh")
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
  ccas <- reactive({
    calculate_ccas(input$used_features)
  })

  highlighted_data <- reactive(
    ~ filter(.x, str_detect(name, input$highlight_str))
  )

  output$scores_plt <- renderPlot(
    bg = "transparent",
    {
      ccas() |>
        filter((X == input$x_sphere & Y == input$y_sphere) | (X == input$y_sphere & Y == input$x_sphere)) |>
        select(X, Y, scores) |>
        unnest(scores) |>
        unite(comp, X, Y) |>
        select(comp, CCA1, geo, time) |>
        pivot_wider(names_from = comp, values_from = CCA1) |>
        left_join(nuts3_regions, by = c("geo" = "geo3")) |>
        unite("name", geo, time, sep = "-") |>
        ggplot(aes_string(paste(input$x_sphere, input$y_sphere, sep = "_"), paste(input$y_sphere, input$x_sphere, sep = "_"))) +
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
        guides(fill = "none")
    }
  )

  output$loadings_plt <- renderPlot({
    ccas() |>
      filter((X == input$x_sphere & Y == input$y_sphere) | (X == input$y_sphere & Y == input$x_sphere)) |>
      select(X, Y, loadings) |>
      unnest(loadings) |>
      unite(comp, X, Y) |>
      left_join(features, by = c("var" = "var_id")) |>
      ggplot(aes(label, CCA1)) +
      geom_hline(yintercept = 0) +
      geom_bar(stat = "identity") +
      coord_flip() +
      facet_grid(comp ~ ., scales = "free_y", space = "free_y") +
      labs(x = "Feature") +
      theme(panel.grid.major.y = element_line(colour = "grey"))
  })

  output$trajectories_plt <- renderPlot({
    ccas() |>
      filter((X == input$x_sphere & Y == input$y_sphere) | (X == input$y_sphere & Y == input$x_sphere)) |>
      select(X, Y, scores) |>
      unnest(scores) |>
      unite(comp, X, Y) |>
      group_by(geo) |>
      arrange(time) |>
      mutate(name = paste(geo, time, sep = "-")) |>
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
      coord_fixed() +
      facet_wrap(~comp) +
      labs(color = "NUTS region")
  })
}

shinyApp(ui = ui, server = server)
