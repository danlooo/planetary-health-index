# load libs for cdf4
# dyn.load("/opt/ohpc/pub/libs/hwloc/lib/libhwloc.so.15")
# dyn.load("/opt/ohpc/pub/libs/gnu9/openmpi4/hdf5/1.10.8/lib/libhdf5_hl.so.100")

library(tidyverse)
library(targets)
library(tarchetypes)
library(crew)
library(eurostat)
library(arrow)
library(RNetCDF)
library(ncdf4)
library(khroma)
library(sf)

source("lib.R")

tar_option_set(
  controller = crew_controller_local(workers = 1),
  error = "continue"
)

list(
  tar_target(times, read_lines("data/quarters.txt")),
  tar_target(regions, read_lines("data/geo3.txt")),
  tar_target(n_expected_rows, length(regions) * length(times)),
  tar_target(max_na_frac, 0.1),
  tar_target(
    space_times,
    expand_grid(space = regions, time = times) |>
      unite("space_time", everything()) |>
      pull(space_time)
  ),
  tar_target(features_csv, read_csv("data/features.csv")),
  tar_target(
    name = features,
    command = {
      tibble(var_id = colnames(raw_cube)) |>
        left_join(features_csv) |>
        mutate(
          sphere = replace_na(sphere, "socio"),
          label = ifelse(is.na(label), var_id, label),
          description = ifelse(is.na(description), label, description),
        ) |>
        arrange(sphere, var_id)
    }
  ),
  tar_target(
    nuts3_sf,
    get_eurostat_geospatial(
      output_class = "sf",
      resolution = "20",
      nuts_level = "3",
      year = "2024"
    )
  ),
  tar_target(
    name = eurostat_regions,
    command = {
      list(
        "data/nuts-regions/NUTS_RG_20M_2003_4326.geojson",
        "data/nuts-regions/NUTS_RG_20M_2006_4326.geojson",
        "data/nuts-regions/NUTS_RG_20M_2010_4326.geojson",
        "data/nuts-regions/NUTS_RG_20M_2013_4326.geojson",
        "data/nuts-regions/NUTS_RG_20M_2016_4326.geojson",
        "data/nuts-regions/NUTS_RG_20M_2021_4326.geojson",
        "data/nuts-regions/NUTS_RG_20M_2024_4326.geojson"
      ) |>
        map(read_sf) |>
        map(as_tibble) |>
        bind_rows() |>
        select(geo = NUTS_ID, nut_level = LEVL_CODE) |>
        distinct(geo, nut_level)
    }
  ),
  tar_target(
    name = nuts3_regions,
    command = {
      eurostat_regions |>
        filter(nut_level == 3) |>
        transmute(
          geo3 = geo,
          geo2 = map_chr(geo3, ~ str_sub(.x, 1, 4)),
          geo1 = map_chr(geo3, ~ str_sub(.x, 1, 3)),
          geo0 = map_chr(geo3, ~ str_sub(.x, 1, 2))
        )
    }
  ),
  tar_target(
    name = eurostat_metadata,
    command = {
      eurostat_data |>
        transmute(
          code = NA,
          colnames = map(data, ~ .x |> colnames()),
          freq = map_chr(data, ~ .x$freq[[1]]),
          nut_level = map_dbl(
            data,
            ~ {
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
            }
          ),
          vars = map2_dbl(
            data,
            code,
            ~ {
              var_columns <- setdiff(names(.x), stable_columns)

              .x |>
                mutate(code = .y) |>
                unite(
                  var,
                  all_of(c("code", var_columns)),
                  sep = "_",
                  remove = TRUE
                ) |>
                group_by(var) |>
                count() |>
                nrow()
            }
          )
        )
    }
  ),
  tar_target(
    name = bioatmo_data,
    command = {
      nc <- nc_open("data/level_3_quarter.nc")

      tibble(var_id = names(nc$var)) |>
        mutate(
          data = map(
            var_id,
            ~ {
              mat <- ncvar_get(nc, .x)
              rownames(mat) <- nc$dim$time$vals
              colnames(mat) <- nc$dim$country$vals

              as_tibble(mat) |>
                mutate(TIME_PERIOD = nc$dim$time$vals) |>
                pivot_longer(-TIME_PERIOD, names_to = "geo", values_to = .x) |>
                unite("space_time", geo, TIME_PERIOD)
            }
          )
        ) |>
        deframe()
    }
  ),
  tar_download(
    name = eurostat_file,
    urls = c("https://zenodo.org/records/18682075/files/eurostat-datacube.nc?download=1"),
    paths = c("data/eurostat-datacube.nc")
  ),
  tar_target(
    name = eurostat_data,
    command = {
      nc <- open.nc(eurostat_file)

      res <- list()
      for (grp in grp.inq.nc(nc)$grp) {
        for (var_id in grp.inq.nc(grp)$varids) {
          mat <- var.get.nc(grp, var_id)
          rownames(mat) <- var.get.nc(nc, "time")
          colnames(mat) <- var.get.nc(nc, "geo")
          var_name <- var.inq.nc(grp, var_id)$name

          cur_res <-
            as_tibble(mat, rownames = "time") |>
            pivot_longer(-time, names_to = "geo", values_to = var_name) |>
            unite("space_time", geo, time)
          res[[var_name]] <- cur_res
        }
      }
      reduce(res, ~ full_join(.x, .y, by = join_by(space_time)))
    }
  ),
  tar_target(
    name = raw_cube,
    command = {
      full_join(
        reduce(bioatmo_data, ~ full_join(.x, .y, by = join_by(space_time))),
        eurostat_data,
        by = join_by(space_time)
      ) |>
        column_to_rownames("space_time")
    }
  ),
  tar_target(
    name = cube,
    command = {
      raw_cube |>
        # handle power law distributed vars like GDP
        mutate(across(starts_with("nama_"), log)) |>
        # z score scaling
        mutate(across(where(is.numeric), scale)) |>
        mutate(across(everything(), ~ replace_na(.x, 0))) |>
        as.matrix()
    }
  ),
  tar_target(
    name = detrended_cube,
    command = {
      raw_cube |>
        as_tibble(rownames = "space_time") |>
        pivot_longer(
          -space_time,
          names_to = "var_id",
          values_to = "pre_value"
        ) |>
        separate(space_time, c("geo", "year", "quarter")) |>
        # detrend
        group_by(var_id, geo, year) |>
        mutate(value = pre_value - mean(pre_value, na.rm = TRUE)) |>
        group_by(var_id, geo, quarter) |>
        mutate(value = pre_value - mean(pre_value, na.rm = TRUE)) |>
        select(-pre_value) |>
        ungroup() |>
        # pivot to matrix
        transmute(
          space_time = paste0(geo, "_", year, "-", quarter),
          var_id,
          value
        ) |>
        pivot_wider(names_from = var_id, values_from = value) |>
        column_to_rownames("space_time") |>
        # z score scaling
        mutate(across(where(is.numeric), scale)) |>
        mutate(across(everything(), ~ replace_na(.x, 0)))
    }
  ),
  tar_target(
    name = detrended_data,
    command = {
      detrended_cube |>
        as_tibble(rownames = "space_time") |>
        pivot_longer(-space_time, names_to = "var_id", values_to = "value") |>
        left_join(features) |>
        mutate(sphere = sphere |> replace_na("socio")) |>
        separate(space_time, into = c("geo", "time"), sep = "_") |>
        mutate(time = yq(time))
    }
  )
)
