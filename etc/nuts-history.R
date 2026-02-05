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

#
# Use official excel spreadsheets for NUT chcnages instead
# instead of infering from geojson
# data: see https://ec.europa.eu/eurostat/web/nuts/history
library(readxl)

get_nuts_codes <- function(data) {
  data |>
    mutate(across(everything(), ~ .x |> str_extract("[A-Z]{2}[A-Z0-9]*"))) |>
    filter(across(everything(), ~ !is.na(.x)))
}
a <-
  read_excel("data/nuts-regions/1995-1999.xls", skip = 1, n_max = 1502) |>
  select(NUTS1995 = `Code 1995`, NUTS1999 = `Code 1999`) |>
  get_nuts_codes()
b <-
  read_excel("data/nuts-regions/1999-2003.xls", skip = 1, n_max = 1462) |>
  select(NUTS1999 = `Code 1999`, NUTS2003 = `Code 2003`) |>
  get_nuts_codes()
c <-
  read_excel("data/nuts-regions/2003-2006.xls", skip = 1, n_max = 1870) |>
  select(NUTS2003 = `Code 2003`, NUTS2006 = `Code 2006`) |>
  get_nuts_codes()
d <-
  read_excel("data/nuts-regions/2006-2010.xls", skip = 1, n_max = 1830) |>
  select(NUTS2006 = `Code 2006`, NUTS2010 = `Code 2010`) |>
  get_nuts_codes()
e <-
  read_excel(
    "data/nuts-regions/NUTS 2010 - NUTS 2013.xls",
    sheet = 2,
    skip = 1,
    n_max = 1916
  ) |>
  select(NUTS2010 = `Code 2010`, NUTS2013 = `Code 2013`) |>
  get_nuts_codes()
f <-
  read_excel(
    "data/nuts-regions/NUTS2013-NUTS2016.xlsx",
    sheet = 2,
    skip = 1,
    n_max = 1866
  ) |>
  select(NUTS2013 = `Code 2013`, NUTS2016 = `Code 2016`) |>
  get_nuts_codes()
g <-
  read_excel(
    "data/nuts-regions/NUTS2021.xlsx",
    sheet = 4,
    skip = 0,
    n_max = 2150
  ) |>
  select(NUTS2016 = `Code 2016`, NUTS2021 = `Code 2021`) |>
  get_nuts_codes()
h <-
  read_excel(
    "data/nuts-regions/NUTS2021-NUTS2024.xlsx",
    sheet = 3,
    skip = 0,
    n_max = 1648
  ) |>
  select(NUTS2021 = `Code 2021`, NUTS2024 = `Code 2024`) |>
  get_nuts_codes()


nuts_history <-
  a |>
  full_join(b) |>
  full_join(c) |>
  full_join(d) |>
  full_join(e) |>
  full_join(f) |>
  full_join(g) |>
  full_join(h) |>
  arrange(nchar(NUTS2024), NUTS2024) |>
  select(
    NUTS2024,
    NUTS2021,
    NUTS2016,
    NUTS2013,
    NUTS2010,
    NUTS2006,
    NUTS2003,
    NUTS1999
  )

write_csv(nuts_history, "data/nuts_history.csv")

View(nuts_history)
nuts_history <- read_csv("data/nuts_history.csv")

# Baixo Alentejo changed NUTS 3 code from PT184 to PT1C2 in NUTS 2024,
# bilategral agreement signed in 27/07/2022
# issue: different code of same region
get_eurostat("demo_r_find3") |>
  filter(geo %in% c("PT184", "PT1C2")) |>
  count(geo)

# map boundaries of NUT regions

tar_load(eurostat_regions)

eurostat_regions |>
  filter(nut_level == 3) |>
  st_as_sf() |>
  st_transform(3035) |>
  ggplot(aes(geometry = geometry, color = nuts_version)) +
  geom_sf(linewidth = 0.5, fill = "transparent") +
  ggsci::scale_color_npg() +
  coord_sf(
    xlim = c(2377294, 7453440),
    ylim = c(1313597, 5628510),
    crs = 3035
  )

# its not just code changes. what about (partial) splits?
# R package nuts does (dasymetric) spatial interpolation
library(nuts)

get_eurostat("demo_r_find3") |>
  filter(geo %in% c("PT184", "PT1C2")) |>
  nuts_classify(
    nuts_code = "geo",
    group_vars = c("freq", "indic_de", "unit", "TIME_PERIOD")
  ) |>
  nuts_convert_version(
    to_version = "2024",
    variables = c("values" = "absolute")
  ) |>
  filter(to_code %in% c("PT184", "PT1C2"))
