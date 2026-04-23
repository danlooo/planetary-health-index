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
  tar_file_read(features_csv, "data/features.csv", read_csv(file = !!.x)),
  tar_file_read(eurostat_resolutions, "data/eurostat-resolutions.csv", read_csv(file = !!.x)),
  tar_target(
    name = features,
    command = {
      eurostat_metadata |>
        full_join(features_csv) |>
        left_join(eurostat_resolutions) |>
        mutate(
          sphere = replace_na(sphere, "socio"),
          source = replace_na(source, "Eurostat"),
          label = ifelse(is.na(label), var_id, label),
          description = ifelse(is.na(description), str_glue("from Eurostat dataset {code}"), description),
          temporal_resolution = replace_na(temporal_resolution, "sub monthly"),
          spatial_resolution = replace_na(spatial_resolution, "sub NUTS 3"),
        ) |>
        filter(var_id %in% cube_tbl$var_id) |>
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
    name = cube_tbl,
    command = {
      # entire data as normalized sorted tibble, e.g. for duckdb

      # eurostat
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
            pivot_longer(-time, names_to = "geo", values_to = var_name)
          res[[var_name]] <- cur_res
        }
      }

      eurostat_tbl <-
        reduce(res, ~ full_join(.x, .y, by = join_by(time, geo))) |>
        pivot_longer(-c(time, geo), names_to = "var_id", values_to = "value")

      # bioatmo
      nc <- nc_open("data/level_3_quarter.nc")

      bioatmo_tbl <-
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
                rename(time = TIME_PERIOD)
            }
          )
        ) |>
        pull(data) |>
        reduce(~ full_join(.x, .y, by = join_by(time, geo))) |>
        pivot_longer(-c(time, geo), names_to = "var_id", values_to = "value")

      cube_tbl <-
        bind_rows(eurostat_tbl, bioatmo_tbl) |>
        arrange(var_id, geo, time) |>
        mutate(
          year = str_extract(time, "[0-9]{4}"),
          quarter = str_extract(time, "Q[1-4]")
        ) |>
        mutate(
          year = as_factor(year),
          quarter = as_factor(quarter),
          geo = as_factor(geo),
          var_id = as_factor(var_id)
        ) |>
        select(var_id, geo, year, quarter, value)
      cube_tbl
    }
  ),
  tar_target(
    name = global_stats,
    command = {
      # cache slow operation on many small groups
      cube_tbl |>
        filter(!is.na(value)) |>
        group_by(var_id) |>
        summarise(global_mean = mean(value)) |>
        ungroup()
    }
  ),
  tar_target(
    name = annual_stats,
    command = {
      # cache slow operation on many small groups
      cube_tbl |>
        filter(!is.na(value)) |>
        group_by(var_id, year) |>
        summarise(
          annual_mean = mean(value),
          annual_sd = sd(value)
        ) |>
        ungroup() |>
        left_join(global_stats) |>
        mutate(annual_mean = annual_mean - global_mean) |>
        select(-global_mean)
    }
  ),
  tar_target(
    name = quarterly_stats,
    command = {
      # cache slow operation on many small groups
      cube_tbl |>
        filter(!is.na(value)) |>
        group_by(var_id, quarter) |>
        summarise(quarterly_mean = mean(value)) |>
        ungroup() |>
        left_join(global_stats) |>
        mutate(quarterly_mean = quarterly_mean - global_mean) |>
        select(-global_mean)
    }
  ),
  tar_target(
    name = geo_stats,
    command = {
      # cache slow operation on many small groups
      cube_tbl |>
        filter(!is.na(value)) |>
        group_by(var_id, geo) |>
        summarise(geo_mean = mean(value)) |>
        ungroup() |>
        left_join(global_stats) |>
        mutate(geo_mean = geo_mean - global_mean) |>
        select(-global_mean)
    }
  ),
  tar_target(
    name = preselected_features,
    command = {
      other_features <-
        features_csv |>
        filter(sphere != "socio") |>
        pull(var_id) |>
        c("tp")

      socio_cube <-
        cube_tbl |>
        filter(!var_id %in% other_features) |>
        unite("space_time", geo, quarter, year) |>
        pivot_wider(names_from = var_id, values_from = value) |>
        column_to_rownames("space_time")

      socio_cube[is.na(socio_cube)] <- 0
      feature_cors <- cor(socio_cube, method = "pearson")
      feature_clust <- hclust(as.dist(1 - feature_cors))
      feature_clusters <- cutree(feature_clust, h = 0.5)

      preselected_socio_features <-
        features |>
        filter(!var_id %in% other_features) |>
        mutate(cluster = map_int(var_id, ~ feature_clusters[.x])) |>
        group_by(cluster) |>
        # random shuffling
        sample_frac(1, replace = FALSE) |>
        # prefer pooled features
        arrange(-str_detect(label, "TOTAL"), -str_detect(label, "T")) |>
        slice(1) |>
        ungroup() |>
        pull(var_id)

      c(preselected_socio_features, other_features)
    }
  )
)
