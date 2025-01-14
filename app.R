library(shiny)
library(tidyverse)
library(bslib)
library(plotly)
library(ggnewscale)
library(ggsci)

theme_set(
  theme_classic(base_size = 14) + theme(legend.position="bottom")
)

data <-
  list.files("data", full.names = TRUE) |>
  map(read_csv) |>
  bind_rows() |>
  pivot_longer(starts_with("CC")) |>
  mutate(
    domain = name |> str_extract("[XYZ]") |> recode("X" = "Biosphere", "Y" = "Atmosphere", Z = "Socioeconomics"),
    cca_axis = name |> str_extract("[0-9]") |> map_chr(~ str_glue("CCA{.x}"))
  ) |>
  select(-name) |>
  rename(country = `Country Code`) |>
  left_join(events) |>
  mutate(during_event = !is.na(event)) |>
  arrange(country, year)

ui <- page_navbar(
  title = "Planetary Health Index φ",
  theme = bs_theme(
    bootswatch = "minty",
    base_font = font_google("Inter"),
    navbar_bg = "#006c66",
    primary = "#006c66",
    fg = "black",
    bg = "white"
  ),
  sidebar = sidebar(
    selectInput("countries", "Countries", data$country |> unique(), multiple = TRUE, selected = c("BRA", "CHN")),
    sliderInput("years", "Years", min = 2000, max = 2024, value = c(2000, 2024))
  ),
  nav_panel(
    title = "Overview",
    div("The Planetary Health Index uses three-way Canonical Correlation Analysis (CCA) to explain the variance of one domain using the other domains."),
    
    navset_card_tab( 
      nav_panel("Domains", plotlyOutput("plt_3d_domains") ), 
      nav_panel("CCA axes",
                selectInput("domain", "Domain", c("Atmosphere", "Biosphere", "Socioeconomics"), selected = c("Atmosphere")),
                plotlyOutput("plt_3d_axes")
          )
    )
  ),
  nav_panel(
    title = "Details",
      plotOutput("plt_cca"),
      plotOutput("plt_time_country"),
      plotOutput("plt_time_domain")
  )
)

server <- server <- function(input, output, session) {
  updateSelectInput(session, "countries", selected = data |> pull(country) |> unique() |> head(2))

  current_data <- reactive({
    data |>
      filter(
        country %in% input$countries &
        min(input$years) <= year &
        year <= max(input$years)
      ) |>
      arrange(cca_axis, country, year, domain)
  })
  
  output$plt_3d_domains <- renderPlotly({
    data <-
      current_data() |>
        filter(cca_axis == "CCA1") |>
        pivot_wider(names_from = domain, values_from = value)
    
    plot_ly(
      data = data,
      x = ~ Atmosphere,
      y = ~ Biosphere,
      z = ~ Socioeconomics,
      color = ~ country,
      colors = pal_npg()(length(input$countries)),
      text = ~ str_glue("year: {year}\ncountry: {country}"),
      
      type = "scatter3d",
      mode = "lines"
    )
  })
  
  output$plt_3d_axes <- renderPlotly({
    data <-
      current_data() |>
      filter(domain == input$domain) |>
      pivot_wider(names_from = cca_axis, values_from = value)
    
    plot_ly(
      data = data,
      x = ~ CCA1,
      y = ~ CCA2,
      z = ~ CCA3,
      color = ~ country,
      colors = pal_npg()(length(input$countries)),
      text = ~ str_glue("year: {year}\ncountry: {country}"),
      
      type = "scatter3d",
      mode = "lines"
    )
  })

  output$plt_cca <- renderPlot({
    current_data() |>
      pivot_wider(names_from = cca_axis, values_from = value) |>
      ggplot(aes(CCA1, CCA2, color = country)) +
      geom_point(size = 4) +
      facet_wrap(~domain) +
      guides(alpha = "none") +
      scale_color_npg()
  })

  output$plt_time_country <- renderPlot({
    current_data() |>
      filter(cca_axis == "CCA1") |>
      ggplot(aes(year, value, color = domain)) +
      geom_line() +
      facet_wrap(~country, dir = "v", strip.position = "right", ncol = 1) +
      scale_x_continuous(expand = c(0,0.2))+
      scale_color_manual(values = c(Atmosphere = "darkblue", Biosphere = "darkgreen", Socioeconomics = "darkorange")) +
      labs(y = "CCA1") 
  })

  output$plt_time_domain <- renderPlot({
    current_data() |>
      filter(cca_axis == "CCA1") |>
      ggplot(aes(year, value, color = country)) +
      geom_line() +
      facet_wrap(~domain, dir = "v", strip.position = "right") +
      scale_x_continuous(expand = c(0,0.2))+
      labs(y = "CCA1") +
      scale_color_npg()
  })
}

shinyApp(ui = ui, server = server)
