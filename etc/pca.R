#
# R Shiny app to visualize planet health index trajectories
#

library(tidyverse)
library(gg3D)
library(gganimate)
library(patchwork)
library(ggrepel)

events <- tribble(
  ~from, ~to, ~event,
  2003, 2012, "Emerging economic growth",
  2012, 2012, "Forest conservation code",
  2014, 2014, "Brazilian economic crisis",
  2015, 2016, "Warmest EI Nino & drought",
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
  mutate(during_event = !is.na(event)) |>
  arrange(country, year) |>
  pivot_wider(names_from = index_type, values_from = value) |>
  filter(country == "BRA") |>
  distinct(year, .keep_all = TRUE) |>
  arrange(country, year)

pca <-
  data |>
  select(Biosphere, Atmosphere, Socioeconomics) |>
  prcomp(scale. = TRUE)


plot_frame <- function(current_year = 2012) {
  tibble() |>
    ggplot(aes(x = PC1, y = PC2)) +
    geom_path(
      data = pca$x |> as.data.frame() |> tibble() |> bind_cols(data) |> filter(year <= current_year) |> arrange(year),
      color = "darkred"
    ) +
    geom_point(
      data = pca$x |> as.data.frame() |> tibble() |> bind_cols(data) |> filter(year <= current_year) |> arrange(year),
      size = 1.5,
      color = "darkred"
    ) +
    geom_segment(
      data = pca$rotation |> as.data.frame() |> as_tibble(rownames = "index_type"),
      mapping = aes(x = 0, y = 0, xend = 1.5 * PC1, yend = 1.5 * PC2, color = index_type),
      arrow = arrow(length = unit(2, "mm"))
    ) +
    coord_fixed(xlim = c(-2.5,2.5), ylim = c(-2,2)) +
    scale_color_manual(values = c("Atmosphere" = "#1854B1", "Biosphere" = "#238D23", "Socioeconomics" = "orange")) +
    theme_classic() +
    # theme(plot.subtitle = element_text(size = 8.5)) +
    labs(
      title = str_glue("Brazil {current_year}"),
      subtitle = events |> filter(country == "BRA" & year == current_year) |> pull(event) |> unique() |> paste0(collapse = ", "),
      color = "Index type"
    )

  str_glue("out/{current_year}.jpg") |>
    ggsave(width = 5, height = 3.5)
}

walk(seq(2003, 2022), plot_frame)
system("convert -delay 80 -coalesce -repage 0x0 -loop 0 out/*.jpg bra.gif")
