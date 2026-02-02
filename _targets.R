# load libs for cdf4
# dyn.load("/opt/ohpc/pub/libs/hwloc/lib/libhwloc.so.15")
# dyn.load("/opt/ohpc/pub/libs/gnu9/openmpi4/hdf5/1.10.8/lib/libhdf5_hl.so.100")

library(tidyverse)
library(targets)
library(crew)
library(eurostat)
library(arrow)
library(RNetCDF)
library(ncdf4)
library(rrcov3way)
library(khroma)

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
  tar_target(space_times, expand_grid(space = regions, time = times) |> unite("space_time", everything()) |> pull(space_time)),
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
    name = eurostat_data,
    pattern = map(eurostat_datasets),
    command = {
      eurostat_datasets |>
        transmute(
          code,
          data = map(code, ~ get_eurostat(.x, cache = FALSE))
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
          geo0 = map_chr(geo3, ~ str_sub(.x, 1, 2))
        )
    }
  ),
  tar_target(
    name = eurostat_metadata,
    pattern = map(eurostat_data),
    command = {
      eurostat_data |>
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
    name = bioatmo_data,
    command = {
      nc <- nc_open("data/level_3_quarter.nc")

      tibble(var_id = names(nc$var)) |>
        mutate(
          data = map(var_id, ~ {
            mat <- ncvar_get(nc, .x)
            rownames(mat) <- nc$dim$time$vals
            colnames(mat) <- nc$dim$country$vals

            as_tibble(mat) |>
              mutate(TIME_PERIOD = nc$dim$time$vals) |>
              pivot_longer(-TIME_PERIOD, names_to = "geo", values_to = .x) |>
              unite("space_time", geo, TIME_PERIOD)
          })
        ) |>
        deframe()
    }
  ),
  tar_target(
    name = raw_cube,
    command = {
      eurostat_data |>
        transmute(
          code,
          data = map2(data, code, possibly(otherwise = NA, ~ {
            res <-
              .x |>
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
              distinct(var, geo, TIME_PERIOD, .keep_all = TRUE) |>
              # re-scale
              resample_time_to_quarter() |>
              resample_space_to_nuts3(nuts3_regions, eurostat_regions) |>
              # crop to bioatmo range
              filter(TIME_PERIOD %in% times & geo %in% regions) |>
              # to space time cube
              unite("space_time", geo, TIME_PERIOD) |>
              arrange(space_time) |>
              select(var, space_time, values) |>
              distinct(var, space_time, .keep_all = TRUE) |>
              pivot_wider(names_from = var, values_from = values) |>
              complete(space_time = space_times)

            abundant_features <-
              res |>
              summarise(across(where(is.numeric), ~ sum(is.na(.x)))) |>
              pivot_longer(everything()) |>
              filter(value <= max_na_frac * n_expected_rows) |>
              pull(name)

            res[c("space_time", abundant_features)]
          }))
        ) |>
        filter(!is.na(data)) |>
        pull(data) |>
        append(bioatmo_data) |>
        reduce(full_join) |>
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
        pivot_longer(-space_time, names_to = "var_id", values_to = "pre_value") |>
        separate(space_time, c("geo", "year", "quarter")) |>
        # detrend
        group_by(var_id, geo, year) |>
        mutate(value = pre_value - mean(pre_value, na.rm = TRUE)) |>
        group_by(var_id, geo, quarter) |>
        mutate(value = pre_value - mean(pre_value, na.rm = TRUE)) |>
        select(-pre_value) |>
        ungroup() |>
        # pivot to matrix
        transmute(space_time = paste0(geo, "_", year, "-", quarter), var_id, value) |>
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
  ),
  tar_target(
    name = detrended_frequency_plt,
    format = "file",
    command = {
      detrended_data |>
        ggplot(aes(value, color = sphere, group = var_id)) +
        geom_density() +
        geom_vline(xintercept = 0) +
        scale_x_continuous(limits = c(-100, 100)) +
        scale_y_log10() +
        annotation_logticks() +
        scale_color_manual(values = sphere_colors) +
        labs(
          x = "Value", y = "Density", color = "Sphere",
          title = "Detrended feature frequency"
        )

      ggsave("out/detrended-features-frequency.png")
    }
  ),
  tar_target(
    name = detrended_values_plt,
    format = "file",
    command = {
      detrended_data |>
        ggplot(aes(time, geo, fill = value)) +
        geom_tile() +
        scale_fill_vik() +
        facet_wrap(~var_id)

      ggsave("out/detrended-features-values.png")
    }
  ),
  tar_target(
    name = detrended_distribution_quarterly_plt,
    format = "file",
    command = {
      detrended_data |>
        mutate(quarter = quarter(time) |> as_factor()) |>
        ggplot(aes(quarter, value)) +
        geom_boxplot() +
        facet_wrap(~var_id)

      ggsave("out/detrended-distribution-quarterly.png", width = 10, height = 10)
    }
  ),
  tar_target(
    name = detrended_distribution_annual_plt,
    format = "file",
    command = {
      detrended_data |>
        mutate(year = year(time) |> as_factor()) |>
        ggplot(aes(year, value)) +
        geom_boxplot() +
        facet_wrap(~var_id)

      ggsave("out/detrended-distribution-annual.png", width = 10, height = 10)
    }
  ),
  tar_target(
    name = eurostat_availability_plt,
    format = "file",
    command = {
      data <-
        eurostat_data |>
        transmute(
          code,
          data = map2(data, code, possibly(otherwise = NA, ~ {
            .x |>
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
              distinct(var, geo, TIME_PERIOD, .keep_all = TRUE) |>
              # re-scale
              resample_time_to_quarter() |>
              resample_space_to_nuts3(nuts3_regions, eurostat_regions) |>
              # test for NA
              transmute(code = .y, var_id = var, geo, time = yq(TIME_PERIOD), value = values) |>
              filter(!is.na(value))
          }))
        ) |>
        filter(!is.na(data)) |>
        pull(data) |>
        bind_rows()

      data |>
        distinct(var_id, geo, time, .keep_all = TRUE) |>
        count(code, var_id, time) |>
        ggplot(aes(time, var_id, fill = n, color = n)) +
        geom_tile() +
        scale_color_oslo(reverse = TRUE) +
        scale_fill_oslo(reverse = TRUE) +
        theme(axis.text.y = element_blank()) +
        facet_grid(code ~ ., scales = "free_y") +
        labs(
          title = "Eurostat feature availability",
          subtitle = "last.update > 2020-01-01, > 500 spatio-temporal values, any of selected codes",
          x = "Time (quarterly)",
          y = "Feature",
          fill = "Number of NUTS3 regions with data",
          color = "Number of NUTS3 regions with data"
        )
      ggsave("out/eurostat-data-availability.png")
    }
  )
)
