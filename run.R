library(tidyverse)
library(tidync)
library(eurostat)

tidync("data/level_3_quarter.nc") |> hyper_tibble(select_var = c("v10"))
tidync("data/eurostat-nc/demo_r_d3dens.nc") |> hyper_tibble()


data <- get_eurostat("demo_r_d3dens")
nut_level_count <-
  data |>
  head(100) |>
  distinct(geo) |>
  inner_join(eurostat_regions, by = "geo") |>
  count(nut_level) |>
  arrange(-n)

transmute(
  code,
  colnames = map(data, ~ .x |> colnames()),
  freq = map_chr(data, ~ .x$freq[[1]]),
  nut_level = map_dbl(data, ~ {
    # infer nut_level by looking at first few rows
    nut_level_count <-
      .x |>
      head(100) |>
      distinct(geo) |>
      inner_join(eurostat_regions, by = "geo") |>
      count(nut_level) |>
      arrange(-n)

    if (nrow(nut_level_count) == 1) {
      nut_level_count$nut_level[[1]]
    } else {
      # some datasets may have multiple nut levels
      max(nut_level_count$nut_level)
    }
  }),
  vars = map2_dbl(data, code, ~ {
    var_columns <- setdiff(names(.x), stable_columns)

    .x |>
      mutate(code = .y) |>
      unite(var, all_of(c("code", var_columns)), sep = "_", remove = TRUE) |>
      group_by(var) |>
      count() |>
      nrow()
  })
)
