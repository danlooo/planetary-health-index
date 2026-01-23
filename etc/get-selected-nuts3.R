selected_codes <- c(
  "nama_10r_3gdp", "nama_10r_3gva", "nama_10r_2gva",
  "prc_hicp_midx", "prc_hicp_fro", "prc_hicp_ene",
  "lfst_r_lfu3pers", "demo_r_d3dens", "ilc_li02", "ilc_di11", "edat_lfse_04"
)


nuts3_regions <-
  eurostat_regions |>
  filter(nut_level == 3) |>
  transmute(
    geo3 = geo,
    geo2 = map_chr(geo3, ~ str_extract(.x, "[A-Z]+[0-9A-z]{2}")),
    geo1 = map_chr(geo3, ~ str_extract(.x, "[A-Z]+[0-9A-z]{1}")),
    geo0 =  map_chr(geo3, ~ str_extract(.x, "[A-Z]+")),
  )

data <-
  eurostat_files |>
  filter(code %in% selected_codes) |>
  slice(1, 3, 5) |>
  mutate(data = map(path, read_parquet)) |>
  select(-path) |>
  mutate(
    data = map(data, ~ {
      # over sample data to NUTS3
      nuts_data <- .x |>
        inner_join(eurostat_regions) |>
        nest(-nut_level)

      if (3 %in% nuts_data$nut_level) {
        # already in the required resolution
        res <-
          nuts_data |>
          filter(nut_level == 3) |>
          pull(data) |>
          first()

        return(res)
      }

      if (2 %in% nuts_data$nut_level) {
        res <-
          nuts_data |>
          filter(nut_level == 2) |>
          pull(data) |>
          first() |>
          right_join(nuts3_regions |> select(geo = geo2, geo3)) |>
          select(-geo) |>
          rename(geo = geo3)

        return(res)
      }

      if (1 %in% nuts_data$nut_level) {
        res <-
          nuts_data |>
          filter(nut_level == 1) |>
          pull(data) |>
          first() |>
          right_join(nuts3_regions |> select(geo = geo1, geo3)) |>
          select(-geo) |>
          rename(geo = geo3)

        return(res)
      }

      if (0 %in% nuts_data$nut_level) {
        res <-
          nuts_data |>
          filter(nut_level == 0) |>
          pull(data) |>
          first() |>
          right_join(nuts3_regions |> select(geo = geo0, geo3)) |>
          select(-geo) |>
          rename(geo = geo3)

        return(res)
      }
    })
  ) |>
  unnest(data)

vars <-
  data |>
  mutate(code = code) |>
  select(code, everything()) |>
  unite(var, -any_of(c("geo", "TIME_PERIOD", "values")), sep = "_") |>
  # ensure every var has the same spatiotemporal dimensions
  complete(var, geo, TIME_PERIOD) |>
  group_by(var) |>
  nest() |>
  transmute(
    var,
    data = data |> map(~ {
      mat <-
        .x |>
        arrange(TIME_PERIOD, geo) |>
        pivot_wider(names_from = geo, values_from = values) |>
        column_to_rownames(var = "TIME_PERIOD")
    })
  )

nc_path <- str_glue("data/eurostat-nc/nuts3-selected.nc")
nc <- create.nc(nc_path, format = "netcdf4")

# Dimensions
time_labels <- rownames(vars$data[[1]])
geo_labels <- colnames(vars$data[[1]])
dim.def.nc(nc, "TIME_PERIOD", length(time_labels))
dim.def.nc(nc, "geo", length(geo_labels))

# Variables
var.def.nc(nc, "TIME_PERIOD", "NC_DOUBLE", "TIME_PERIOD")
var.def.nc(nc, "geo", "NC_STRING", "geo") # string labels (NetCDF4)

# CF attributes for TIME_PERIOD
att.put.nc(nc, "TIME_PERIOD", "units", "NC_CHAR", "days since 1970-01-01 00:00:00")
att.put.nc(nc, "TIME_PERIOD", "calendar", "NC_CHAR", "gregorian")

TIME_PERIOD_vals <- as.numeric(as.Date(time_labels) - as.Date("1970-01-01"))

# Write vars
var.put.nc(nc, "TIME_PERIOD", TIME_PERIOD_vals)
var.put.nc(nc, "geo", geo_labels)

walk2(vars$var, vars$data, ~ {
  var.def.nc(nc, .x, "NC_DOUBLE", c("TIME_PERIOD", "geo"))
  var.put.nc(nc, .x, as.matrix(.y))
})

close.nc(nc)
