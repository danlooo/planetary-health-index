library(tidyverse)
library(gg3D)
library(gganimate)
library(patchwork)

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
  arrange(country, year)


plot <- function(current_cca_axis = "CCA2", current_country = "BRA") {
    data |>
    pivot_wider(names_from = index_type, values_from = value) |>
    filter(cca_axis == current_cca_axis & country == current_country ) |>
    arrange(year) |>
    ggplot(aes(x = Biosphere, y = Atmosphere, z = Socioeconomics, alpha = year)) +
    axes_3D() +
    stat_3D(geom = "point", size = 2) +
    stat_3D(geom = "path") +
    labs_3D(label = c("Bio"," Atmo", "Soc"), hjust = 1.2) +
    theme_void() +
    theme(plot.subtitle = element_text(size = 6)) +
    coord_fixed(xlim = 0.5*c(-1,1), ylim = 0.5*c(-1,1)) +
    guides(color = "none") +
    labs(
      title = str_glue("{current_country} {current_cca_axis}")
    )
}

expand_grid(
  cca_axis = c("CCA1", "CCA2", "CCA3"),
  country = c("BRA", "CHN", "NER")
) |>
  mutate(plt = map2(cca_axis, country, ~ plot(.x, .y))) |>
  pull(plt) |>
  wrap_plots()

ggsave("out/cca-country.png", width = 10, height = 10)
