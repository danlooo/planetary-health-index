#
# R Shiny app to visualize planet health index trajectories
#

library(shiny)
library(tidyverse)
library(gg3D)
library(gganimate)
library(patchwork)

events <- tribble(
  ~from, ~to, ~event,
  2003, 2012, "one of the fastest-growing major economies in the world",
  2012, 2012, "Forest conservation code",
  2014, 2014, "Brazilian economic crisis",
  2015, 2016, "Warmest EI Nino-induced severe drought",
  2018, 2024, "increased deforestation alerts",
  2020, 2021, "COVID-19"
) |>
  mutate(country = "BRA") |>
  mutate(year = map2(from, to, ~ seq(.x, .y))) |>
  unnest_longer(year) |>
  select(-from, -to)

data <- 
  list.files("data", full.names = TRUE) |>
  map(read_csv) |>
  bind_rows() |>
  pivot_longer(starts_with("CC")) |>
  mutate(
    index_type = name |> str_extract("[XYZ]") |> recode("X" = "Biosphere", "Y" = "Atmosphere", Z = "Socioeconomics"),
    cca_axis = name |> str_extract("[0-9]") |> map_chr(~ str_glue("CCA{.x}"))
  ) |>
  select(-name) |>
  rename(country = `Country Code`) |>
  left_join(events) |>
  mutate(during_event = ! is.na(event)) |>
  arrange(country, year)


ui <- fluidPage(
  titlePanel("Planetary Health Index"),
  sidebarLayout(
    sidebarPanel(
      selectInput("country", "Country", data$country |> unique()),
      sliderInput("year", "Year", min = 2000, max = 2024, value = 2012)
    ),
    mainPanel(
      textOutput("events"),
      plotOutput("plt1"),
      plotOutput("plt2")
    )
  )
)

server <- server <- function(input, output) {
  output$events <- renderText({
    data |>
      filter(year == input$year & country == input$country) |>
      pull(event) |>
      unique() |>
      paste(collapse = "\n")
  })
  
  output$plt1 <- renderPlot({
    data |>
      pivot_wider(names_from = index_type, values_from = value) |>
      filter(cca_axis == "CCA1") |>
      mutate(
        point_group = case_when(
          year == input$year & country == input$country  ~ "current_point",
          country == current_country ~ "normal",
          year == current_year ~ "current_year",
          TRUE ~ "normal"
        ) |> factor(levels = c("normal", "current_country", "current_year", "current_point")), # plotting order
        path_group = case_when(
          country == input$country & year <= input$year ~ "current_country",
          TRUE ~ NA
        ),
        country_label = ifelse(year == current_year, country, NA),
        point_size = ifelse(point_group == "current_point", 2.5, 1)
      ) |>
      arrange(point_group, year, country) |>
      ggplot(aes(x = Biosphere, y = Atmosphere, z = Socioeconomics)) +
      axes_3D() +
      stat_3D(geom = "point", mapping = aes(color = point_group, size = point_size), size = 2.5) +
      stat_3D(geom = "path", mapping = aes(color = path_group)) +
      stat_3D(geom = "text", mapping = aes(label = country_label, color = point_group), hjust = 1.2) +
      labs_3D(label = c("Bio"," Atmo", "Soc")) +
      guides(color = "none") +
      scale_color_manual(values = c("normal" = "grey", "current_point" = "darkred", "current_country" = "darkred", "current_year" = "darkblue"), na.value = "#00000000") +
      theme_void()
  })
  
  output$plt2 <- renderPlot({
    data |>
      pivot_wider(names_from = cca_axis, values_from = value) |>
      mutate(
        point_group = case_when(
          year == input$year & country == input$country  ~ "current_point",
          country == input$country ~ "normal",
          year == input$year ~ "current_year",
          TRUE ~ "normal"
        ) |> factor(levels = c("normal", "current_country", "current_year", "current_point")), # plotting order
        path_group = case_when(
          country == input$country & year <= input$year ~ "current_country",
          TRUE ~ NA
        ),
        country_label = ifelse(year == input$year, country, NA),
        point_size = ifelse(point_group == "current_point", 2.5, 1)
      ) |>
      arrange(point_group, year, country) |> # plot order
      ggplot(aes(CCA1, CCA2)) +
      geom_point(color = "grey", size = 2) +
      geom_point(mapping = aes(color = point_group), size = 1.5) +
      geom_path(mapping = aes(color = path_group)) +
      geom_text(mapping = aes(label = country_label, color = point_group), hjust = 1.2)+
      scale_color_manual(values = c("normal" = "grey", "current_point" = "darkred", "current_country" = "darkred", "current_year" = "darkblue"), na.value = "#00000000") +
      theme_classic() +
      facet_wrap(~index_type)+
      guides(color = "none") +
      theme(
        strip.background = element_blank()
      ) +
      labs(
        title = str_glue("Brazil {current_year}"),
        subtitle = data2 |> filter(year == current_year) |> pull(event) |> unique() |> paste(collapse = "\n")
      )
  })
}

shinyApp(ui = ui, server = server)