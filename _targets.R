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
      eurostat_metadata |>
        bind_rows(features_csv) |>
        mutate(
          sphere = replace_na(sphere, "socio"),
          label = ifelse(is.na(label), var_id, label)
        ) |>
        filter(var_id %in% colnames(cube)) |>
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
  tar_target(nuts3_regions, read_csv("data/nuts3_regions.csv")),
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
    name = eurostat_metadata,
    command = {
      nc <- open.nc(eurostat_file)

      res <- list()
      for (grp in grp.inq.nc(nc)$grp) {
        for (var_id in grp.inq.nc(grp)$varids) {
          cur_res <- tibble(
            sphere = "socio",
            var_id = var.inq.nc(grp, var_id)$name,
            unit = att.get.nc(grp, var_id, "unit"),
            label = paste0(att.get.nc(grp, var_id, "long_name"), " (", unit, ")"),
            code = grp.inq.nc(grp)$name
          )

          res <- bind_rows(res, cur_res)
        }
      }
      res
    }
  ),
  tar_target(
    name = eurostat_data,
    command = {
      nc <- open.nc(eurostat_file)

      res <- list()
      for (grp in grp.inq.nc(nc)$grp) {
        for (var_id in grp.inq.nc(grp)$varids) {
          mat <- var.get.nc(grp, var_id)
          # overwrite climate conventions, use quarters instead
          rownames(mat) <- times
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
  )
)
