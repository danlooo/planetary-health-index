#
# R Shiny app to visualize planet health index trajectories
#

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

current_year <- 2012
current_country <- "BRA"

plot_frame <- function(current_year = 2012, current_country = "BRA") {
  data2 <-
    data |>
    pivot_wider(names_from = index_type, values_from = value) |>
    filter(cca_axis == "CCA1") |>
    mutate(
      # need to plot future points with transparency to fix camera position
      state = ifelse(country == current_country & year <= current_year, "current", NA)
    )
  
  current_events <- events |> filter(country == "BRA" & year == current_year)
  current_events_text <- current_events$event |> paste(collapse = ", ")
  
  data2 |>
    ggplot(aes(x = Biosphere, y = Atmosphere, z = Socioeconomics, color = state)) +
    axes_3D() +
    stat_3D(geom = "point", size = 2) +
    stat_3D(geom = "path") +
    labs_3D(label = c("Bio"," Atmo", "Soc"), hjust = 1.2) +
    theme_void() +
    theme(plot.subtitle = element_text(size = 6)) +
    coord_fixed(xlim = 0.5*c(-1,1), ylim = 0.5*c(-1,1)) +
    scale_color_manual(values = c("current" = "darkred"), na.value = "#00000000") +
    guides(color = "none") +
    labs(
      title = str_glue("Brazil {current_year}"),
      subtitle = current_events_text
    )
  
  str_glue("out/{current_country}-{current_year}.jpg") |>
    ggsave(width = 5, height = 5)
}

walk(seq(2003, 2022), plot_frame)

seq(1,20) |> walk(~ str_glue("convert -crop 1200x1050+0+0 out/a{.x}.jpg out/cropped{.x}.jpg") |> system())

# files must be like 001.jpg with leading zeros to be in order
system("convert -delay 80 -coalesce -repage 0x0 -loop 0 out/cropped*.jpg bra.gif")
