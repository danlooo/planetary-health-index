devtools::install_github("AckerDWM/gg3D")

# X: Biosphere, Y: Atmosphere, Z: Socioeconomics; we can use dim 1 first, and ignore 2 and 3 in the first try

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

plot_frame <- function(current_year = 2021, current_country = "BRA") {
  data1 <-
    data |>
    pivot_wider(names_from = index_type, values_from = value) |>
    filter(cca_axis == "CCA1")
  
  plt1 <-
    data1 |>
    mutate(
      point_group = case_when(
        year == current_year & country == current_country  ~ "current_point",
        country == current_country ~ "normal",
        year == current_year ~ "current_year",
        TRUE ~ "normal"
      ) |> factor(levels = c("normal", "current_country", "current_year", "current_point")), # plotting order
      path_group = case_when(
        country == current_country & year <= current_year ~ "current_country",
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
  
  plt2 <-
    data |>
    pivot_wider(names_from = cca_axis, values_from = value) |>
    mutate(
      point_group = case_when(
        year == current_year & country == current_country  ~ "current_point",
        country == current_country ~ "normal",
        year == current_year ~ "current_year",
        TRUE ~ "normal"
      ) |> factor(levels = c("normal", "current_country", "current_year", "current_point")), # plotting order
      path_group = case_when(
        country == current_country & year <= current_year ~ "current_country",
        TRUE ~ NA
      ),
      country_label = ifelse(year == current_year, country, NA),
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
  
  
  plt <- (plt2 | plt1) + plot_layout(guides = "collect", widths = c(3,1))
  plt
}

plot_frame(2010, "BRA")

expand_grid(country = "BRA", year = seq(2003, 2021)) |>
  mutate(plt = map2(year, country, function(year, country) {
    plt <- plot_frame(year, country)
    dir.create("out")
    ggsave(str_glue("out/{country}-{year}.png"), width = 11)
  }))
