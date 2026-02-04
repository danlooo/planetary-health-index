#
# Map NUTS regions across versions
#
# Statistical regions may change over time. We need to map those sets of geospatial regions
# to create time series data. Spatial join by highest overlap is used to create a harnomized space_id.
#

library(eurostat)
library(tidyverse)
library(sf)

nuts_sf <-
  c(
    "NUTS2003" = "data/nuts-regions/NUTS_RG_20M_2003_4326.geojson",
    "NUTS2021" = "data/nuts-regions/NUTS_RG_20M_2021_4326.geojson",
    "NUTS2024" = "data/nuts-regions/NUTS_RG_20M_2024_4326.geojson"
  ) |>
  enframe(value = "path") |>
  mutate(
    data = map2(name, path, function(name, path) {
      path |>
        read_sf() |>
        filter(LEVL_CODE == 3) |>
        filter(str_starts(NUTS_ID, "DE93") | CNTR_CODE == "PT") |>
        select(NUTS_ID, NUTS_NAME, geometry) |>
        rename_with(~ paste0(.x, "-", name), c(NUTS_ID, NUTS_NAME))
    })
  ) |>
  select(name, data) |>
  pull(data) |>
  reduce(~ st_join(.x, .y, join = st_overlaps, largest = TRUE)) |>
  mutate(space_id = row_number()) |>
  pivot_longer(starts_with("NUTS")) |>
  separate(name, into = c("name", "NUTS_VERSION"), sep = "-") |>
  pivot_wider()

View(nuts_sf)


# test NUTS region that changed name but not id
nuts_sf |>
  filter(NUTS_NAME %in% c("Heidekreis", "Soltau-Fallingbostel")) |>
  nrow() ==
  3

nuts_sf |>
  filter(NUTS_NAME %in% c("Heidekreis", "Soltau-Fallingbostel")) |>
  pull(NUTS_ID) |>
  unique() |>
  length() ==
  1

# test NUTS region that changed ID but not name

pt_nuts_sf <-
  bind_rows(
    read_sf("data/nuts-regions/NUTS_RG_20M_2024_4326.geojson") |>
      filter(CNTR_CODE == "PT") |>
      mutate(NUTS_VERSION = "NUTS2024"),
    read_sf("data/nuts-regions/NUTS_RG_20M_2021_4326.geojson") |>
      filter(CNTR_CODE == "PT") |>
      mutate(NUTS_VERSION = "NUTS2021")
  )

pt_nuts_sf |>
  filter(NUTS_NAME == "Baixo Alentejo") |>
  pull(NUTS_ID) |>
  length() ==
  2

pt_nuts_sf |>
  ggplot(aes(geometry = geometry, color = NUTS_VERSION)) +
  geom_sf()
