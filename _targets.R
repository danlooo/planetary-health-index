library(tidyverse)
library(targets)
library(crew)
library(eurostat)
library(arrow)

# load libs for cdf4
# dyn.load("/opt/ohpc/pub/libs/hwloc/lib/libhwloc.so.15")
# dyn.load("/opt/ohpc/pub/libs/gnu9/openmpi4/hdf5/1.10.8/lib/libhdf5_hl.so.100")
library(RNetCDF)

library(rrcov3way)

source("lib.R")

set.seed(1337)

tar_option_set(
  controller = crew_controller_local(workers = 1),
  error = "continue"
)

list(
  tar_target(
    name = eurostat_datasets,
    command = {
      get_eurostat_toc() |>
        mutate(last.update.of.data = as.Date(last.update.of.data, format = "%d.%m.%Y")) %>%
        filter(
          type != "folder" &
            last.update.of.data > as.Date("2020-01-01") &
            values > 500 &
            code %in% selected_codes
        ) |>
        # enforce code to be unique
        select(-hierarchy) |>
        distinct(code, .keep_all = TRUE)
    }
  ),
  tar_target(
    name = eurostat_files,
    pattern = map(eurostat_datasets),
    command = {
      eurostat_datasets |>
        transmute(
          code,
          path = map_chr(code, ~ {
            # prevent data race on eurostat cache metadata json
            data <- get_eurostat(.x, cache = FALSE)
            path <- str_glue("out/eurostat-raw/{.x}.parquet")
            write_parquet(data, path)
            path
          })
        )
    }
  ),
  tar_target(eurostat_regions, tibble(
    geo = eurostat_geodata_60_2016$geo,
    nut_level = eurostat_geodata_60_2016$LEVL_CODE
  )),
  tar_target(
    name = nuts3_regions,
    command = {
      eurostat_regions |>
        filter(nut_level == 3) |>
        transmute(
          geo3 = geo,
          geo2 = map_chr(geo3, ~ str_sub(.x, 1, 4)),
          geo1 = map_chr(geo3, ~ str_sub(.x, 1, 3)),
          geo0 = map_chr(geo3, ~ str_sub(.x, 1, 2)),
        )
    }
  ),
  tar_target(
    name = eurostat_metadata,
    pattern = map(eurostat_files),
    command = {
      eurostat_files |>
        mutate(data = map(path, ~ .x |> read_parquet())) |>
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
    }
  ),
  tar_target(
    name = eurostat_cube_files,
    pattern = map(eurostat_files),
    command = {
      eurostat_files |>
        mutate(
          data = map2(path, code, ~ {
            read_parquet(.x) |>
              # discard aggregated data
              filter(!str_starts(geo, "EU|EA|EEA")) |> # e.g euro area
              group_by(nut_level = nchar(geo) - 2) |>
              nest() |>
              arrange(-nut_level) |>
              pull(data) |>
              first() |>
              # create variable
              mutate(code = .y) |>
              select(code, everything()) |>
              unite(var, -any_of(setdiff(stable_columns, "code")), sep = "_") |>
              # re-scale
              resample_time_to_quarter() |>
              resample_space_to_nuts3(nuts3_regions, eurostat_regions)
          }),
          path = str_glue("out/eurostat-nc/{code}.nc"),
          out = map2_chr(data, path, write_nc_tibble)
        ) |>
        select(-out)
    }
  )
)
