set.seed(1337)

stable_columns <- c("code", "freq", "unit", "geo", "TIME_PERIOD", "values")

selected_codes <- c(
  "nama_10r_3gdp", "nama_10r_3gva", "nama_10r_2gvagr",
  "teicp010", "teicp250", "nama_10_nfa_bs",
  "lfst_r_lfu3pers", "demo_r_d3dens", "ilc_li02", "ilc_di11", "edat_lfse_04",
  "nama_10r_2gfcf", "nama_10r_2emhrw"
)

# prc_hicp_midx is too big for 16G memory

sphere_colors <- c("atmo" = "#87CEEB", "bio" = "#228B22", "socio" = "#808080")
primary_color <- "#006c66"
light_gray_color <- "#a4a4a4"
dark_gray_color <- "#464646"
spheres <- names(sphere_colors)

theme_set(new = theme_classic())

write_nc_tibble <- function(data, nc_path) {
  # uniform dimension with same shape of other cubes
  times <- read_lines("data/quarters.txt")
  locations <- read_lines("data/geo3.txt")

  fill_value <- 9.96921e36

  vars <-
    data |>
    group_by(var) |>
    nest() |>
    transmute(
      var,
      data = data |> map(~ {
        .x |>
          # discard elements without position
          filter(!is.na(geo) & !is.na(TIME_PERIOD)) |>
          # complete table to same shape of other cubes
          select(TIME_PERIOD, geo, values) |>
          filter(TIME_PERIOD %in% times) |>
          filter(geo %in% locations) |>
          distinct(TIME_PERIOD, geo, .keep_all = TRUE) |>
          mutate(
            TIME_PERIOD = factor(TIME_PERIOD, times),
            geo = factor(geo, locations)
          ) |>
          complete(TIME_PERIOD = times, geo = locations) |>
          # pivot to matrix
          arrange(TIME_PERIOD, geo) |>
          pivot_wider(names_from = geo, values_from = values) |>
          column_to_rownames(var = "TIME_PERIOD") |>
          as.matrix()
      })
    )

  nc <- create.nc(nc_path, format = "netcdf4")

  # Dimensions
  dim.def.nc(nc, "TIME_PERIOD", length(times))
  dim.def.nc(nc, "geo", length(locations))

  # Variables
  var.def.nc(nc, "TIME_PERIOD", "NC_STRING", "TIME_PERIOD")
  var.def.nc(nc, "geo", "NC_STRING", "geo") # string labels (NetCDF4)

  # CF attributes for TIME_PERIOD, disable for quarter
  # att.put.nc(nc, "TIME_PERIOD", "units", "NC_CHAR", "days since 1970-01-01 00:00:00")
  # att.put.nc(nc, "TIME_PERIOD", "calendar", "NC_CHAR", "gregorian")

  # Write vars
  var.put.nc(nc, "TIME_PERIOD", times)
  var.put.nc(nc, "geo", locations)

  walk2(vars$var, vars$data, ~ {
    var.def.nc(nc, .x, "NC_DOUBLE", c("TIME_PERIOD", "geo"))
    att.put.nc(nc, .x, "missing_value", "NC_DOUBLE", fill_value)

    .x[is.na(.x)] <- fill_value
    var.put.nc(nc, .x, .y)
  })

  close.nc(nc)

  nc_path
}

resample_time_to_quarter <- function(data, agg_func = mean) {
  freq <- data$freq[[1]]

  if (freq == "Q") {
    # already at target resolution
    return(data)
  } else if (freq == "M") {
    # aggregate monthly to quarterly
    res <-
      data |>
      mutate(TIME_PERIOD = paste0(year(TIME_PERIOD), "-Q", quarter(TIME_PERIOD))) |>
      group_by(var, geo, TIME_PERIOD) |>
      summarise(var, geo, values = agg_func(values))
    return(res)
  } else if (freq == "A") {
    # upscale annual to quarterly
    res <- mutate(data, year = year(TIME_PERIOD))
    res <-
      res |>
      expand(year, quarter = paste0("Q", 1:4)) |>
      left_join(res, by = "year") |>
      mutate(TIME_PERIOD = paste0(year, "-", quarter)) |>
      select(-year, -quarter)
    return(res)
  }
  stop("freq value not implemented.")
}

resample_space_to_nuts3 <- function(data, nuts3_regions, eurostat_regions) {
  nuts_level <-
    data |>
    ungroup() |>
    distinct(geo) |>
    inner_join(eurostat_regions) |>
    count(nut_level) |>
    arrange(-n) |>
    pull(nut_level) |>
    first()

  if (nuts_level == 3) {
    # already at target resolution
    return(data)
  } else if (nuts_level == 2) {
    # up-sample NUTS2 to NUTS3 level
    res <- mutate(data, geo2 = geo)

    res <-
      nuts3_regions |>
      select(geo2, geo3) |>
      filter(geo2 %in% res$geo2) |>
      left_join(res, by = "geo2") |>
      mutate(geo = geo3) |>
      select(-geo2, -geo3)
    return(res)
  } else if (nuts_level == 0) {
    # up-sample country to NUTS3 level
    res <- mutate(data, geo0 = geo)

    res <-
      nuts3_regions |>
      select(geo0, geo3) |>
      filter(geo0 %in% res$geo0) |>
      left_join(res, by = "geo0") |>
      mutate(geo = geo3) |>
      select(-geo0, -geo3)
    return(res)
  }
  stop("nuts_level value not implemented")
}

calculate_cca <- function(cube, x_features, y_features) {
  cca <- stats::cancor(cube[, x_features], cube[, y_features])
  U <- as.matrix(cube[, x_features]) %*% cca$xcoef
  V <- as.matrix(cube[, y_features]) %*% cca$ycoef

  scores <- tibble(
    CCA1 = U[, 1],
    CCA2 = U[, 2],
    ID = rownames(U)
  ) |>
    separate(ID, c("geo", "time"), sep = "_") |>
    mutate(country = geo |> str_extract("^[A-z]+"))

  loadings <- tibble(
    var_id = rownames(cca$xcoef),
    CCA1 = cca$xcoef[, 1],
    CCA2 = cca$xcoef[, 2]
  )

  list(cca = cca, scores = scores, loadings = loadings)
}
