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
      # Extract actual PCA variable names from cube_tbl (dataset-specific PC names)
      pca_var_ids <- cube_tbl |>
        filter(str_detect(var_id, "_PC\\d+$")) |>
        distinct(var_id) |>
        arrange(var_id) |>
        pull(var_id)

      # Create features for PCA components with dataset-specific names
      pca_features <- tibble(
        sphere = "socio",
        var_id = pca_var_ids
      ) |>
        mutate(
          # Extract dataset code from var_id (e.g., "dataset_PC1" -> "dataset")
          dataset_code = str_remove(var_id, "_PC[0-9]+$"),
          pc_number = str_extract(var_id, "PC\\d+$")
        ) |>
        left_join(
          eurostat_metadata |> select(code, unit) |> distinct(),
          by = c("dataset_code" = "code")
        ) |>
        mutate(
          source = "Eurostat",
          label = paste0(
            pc_number, " from ", dataset_code,
            ifelse(is.na(unit), "", paste0(" (", unit, ")"))
          ),
          description = str_glue("Principal component from Eurostat dataset {dataset_code}"),
          temporal_resolution = "sub monthly",
          spatial_resolution = "sub NUTS 3"
        ) |>
        select(-dataset_code, -pc_number, -unit)

      # Combine with non-socio features and arrange
      pca_features |>
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

      # eurostat - perform PCA for each dataset code individually
      nc <- open.nc(eurostat_file)
      eurostat_pca_results <- list()

      for (grp in grp.inq.nc(nc)$grp) {
        grp_name <- grp.inq.nc(grp)$name
        grp_res <- list()

        # Collect all variables for this group/dataset code
        for (var_id in grp.inq.nc(grp)$varids) {
          mat <- var.get.nc(grp, var_id)
          # overwrite climate conventions, use quarters instead
          rownames(mat) <- times
          colnames(mat) <- var.get.nc(nc, "geo")
          var_name <- var.inq.nc(grp, var_id)$name

          cur_res <-
            as_tibble(mat, rownames = "time") |>
            pivot_longer(-time, names_to = "geo", values_to = var_name)
          grp_res[[var_name]] <- cur_res
        }

        # Create wide table for this group only
        grp_wide <- reduce(grp_res, ~ full_join(.x, .y, by = join_by(time, geo)))

        # Apply PCA to this group's variables only
        grp_data_for_pca <-
          grp_wide |>
          select(-c(geo, time)) |>
          select(where(~ {
            x <- .
            # Keep column only if:
            # - not all NA
            # - more than one unique non-NA value
            !all(is.na(x)) &&
              n_distinct(x, na.rm = TRUE) > 1
          }))

        # Only perform PCA if there are variables to analyze
        if (ncol(grp_data_for_pca) > 0) {
          grp_pca <-
            grp_data_for_pca |>
            mutate(
              across(
                where(is.numeric),
                ~ replace_na(.x, mean(.x, na.rm = TRUE))
              )
            ) |>
            prcomp(center = TRUE, scale. = TRUE)

          # Create result table with PC names prefixed by group name
          grp_result <-
            grp_wide |>
            select(geo, time) |>
            separate(time, into = c("year", "quarter")) |>
            bind_cols(grp_pca$x) |>
            rename_with(~ paste0(grp_name, "_", .x), starts_with("PC")) |>
            pivot_longer(-c(geo, year, quarter), names_to = "var_id", values_to = "value")

          eurostat_pca_results[[grp_name]] <- grp_result
        }
      }

      # Combine all PCA results
      eurostat_tbl <-
        eurostat_pca_results |>
        bind_rows() |>
        unite(col = "time", year, quarter, sep = "-")

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
        c("tp") |>
        as.character() # no fatcor

      preselected_socio_features <-
        cube_tbl |>
        filter(str_detect(var_id, "_PC1$")) |>
        distinct(var_id) |>
        arrange(var_id) |>
        pull(var_id) |>
        as.character() # no factor

      c(preselected_socio_features, other_features)
    }
  )
)
