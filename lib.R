stable_columns <- c("code", "freq", "unit", "geo", "TIME_PERIOD", "values")

selected_codes <- c(
  "nama_10r_3gdp", "nama_10r_3gva", "nama_10r_2gvagr",
  "prc_hicp_midx", "teicp010", "teicp250", "nama_10_nfa_bs",
  "lfst_r_lfu3pers", "demo_r_d3dens", "ilc_li02", "ilc_di11", "edat_lfse_04",
  "nama_10r_2gfcf", "nama_10r_2emhrw"
)

sphere_colors <- c("atmo" = "#87CEEB", "bio" = "#228B22", "socio" = "#808080")
primary_color <- "#006c66"
spheres <- names(sphere_colors)

read_nc_feature_socio <- function(path, var_id, label = var_id) {
  nc <- nc_open(path)

  mat <- ncvar_get(nc, var_id)
  rownames(mat) <- nc$dim$TIME_PERIOD$vals
  colnames(mat) <- nc$dim$geo$vals

  as_tibble(mat) |>
    mutate(TIME_PERIOD = nc$dim$TIME_PERIOD$vals) |>
    pivot_longer(-TIME_PERIOD, names_to = "geo", values_to = label) |>
    unite("observation", geo, TIME_PERIOD)
}

read_nc_feature_bioatmo <- function(var_id, path = "out/countrydatacubes/level_3_quarter.nc") {
  nc <- nc_open(path)

  mat <- ncvar_get(nc, var_id)
  rownames(mat) <- nc$dim$time$vals
  colnames(mat) <- nc$dim$country$vals

  as_tibble(mat) |>
    mutate(TIME_PERIOD = nc$dim$time$vals) |>
    pivot_longer(-TIME_PERIOD, names_to = "geo", values_to = var_id) |>
    unite("observation", geo, TIME_PERIOD)
}


write_nc_tibble <- function(data, nc_path) {
  # uniform dimension with same shape of other cubes
  times <- read_lines("quarters.txt")
  locations <- read_lines("geo3.txt")

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

features <- bind_rows(
  tibble(
    sphere = "atmo",
    var_id = c("ssr", "vpd", "tp", "sp", "v10", "u10", "t2m")
  ) |>
    mutate(label = var_id),
  tribble(
    ~var_id, ~label,
    "Day_AQUA_Mxx21x1_061_gapfilled_QCflags_dyn", "Day water Mxx",
    "Night_AQUA_Mxx21x1_061_gapfilled_QCflags_dyn", "Night water Mxx",
    "NDWI_band7gapfilled_061_QCdyn", "NDWI",
    "NDVIgapfilled_061_QCdyn", "NDVI",
    "NIRvgapfilled_061_QCdyn", "NIRv",
    "NEE", "NEE",
    "H", "H",
    "ET", "ET",
    "ET_T", "ET_T",
    "GPP", "GPP",
    "skt", "skt"
  ) |>
    mutate(sphere = "bio"),
  tribble(
    ~path, ~var_id, ~label,
    "out/eurostat-nc/demo_r_d3dens.nc", "demo_r_d3dens", "Pop density",
    "out/eurostat-nc/nama_10r_3gdp.nc", "nama_10r_3gdp", "GDP",
    # read_nc_feature_socio("phi-eu/out/eurostat-nc/nama_10r_2gvagr.nc", "nama_10r_2gvagr_B1G"),
    # read_nc_feature_socio("phi-eu/out/eurostat-nc/nama_10r_2gvagr.nc", "nama_10r_2gvagr_B1GQ"),

    # files not found
    # read_nc_feature_socio("out/eurostat-nc/prc_hicp_midx.nc", "prc_hicp_midx_FOOD", "Food_Price"),
    # read_nc_feature_socio("out/eurostat-nc/prc_hicp_midx.nc", "prc_hicp_midx_FUEL", "Fuel_Price"),
    # read_nc_feature_socio("out/eurostat-nc/prc_hicp_midx.nc", "prc_hicp_midx_ELC_GAS", "GAS_Price"),

    "out/eurostat-nc/edat_lfse_04.nc", "edat_lfse_04_T_ED3-8_Y25-64", "Education",
    "out/eurostat-nc/lfst_r_lfu3pers.nc", "lfst_r_lfu3pers_ED5-8_T_Y20-64", "Employement",
    # read_nc_feature_socio("phi-eu/out/eurostat-nc/teicp250.nc", "teicp250_NRG"), # Energy # NO DATA
    # read_nc_feature_socio("phi-eu/out/eurostat-nc/teicp010.nc", "teicp010_CP01"), # Food Price # NO DATA!
    # read_nc_feature_socio("phi-eu/out/eurostat-nc/nama_10_nfa_bs.nc", "nama_10_nfa_bs_S12_N1173N"),
    # read_nc_feature_socio("phi-eu/out/eurostat-nc/nama_10_nfa_bs.nc", "nama_10_nfa_bs_S11_N13N"),
    "out/eurostat-nc/nama_10_nfa_bs.nc", "nama_10_nfa_bs_S13_N21ON", "Capital_stock"
  ) |>
    mutate(sphere = "socio")
)

calculate_ccas <- function(used_features) {
  atmo_df <-
    features |>
    filter(sphere == "atmo" & label %in% used_features) |>
    mutate(data = map(var_id, read_nc_feature_bioatmo)) |>
    pull(data) |>
    reduce(inner_join) |>
    mutate(across(where(is.numeric), scale))

  bio_df <-
    features |>
    filter(sphere == "bio" & label %in% used_features) |>
    mutate(data = map(var_id, read_nc_feature_bioatmo)) |>
    pull(data) |>
    reduce(inner_join) |>
    mutate(across(where(is.numeric), scale))

  socio_df <-
    features |>
    filter(sphere == "socio" & label %in% used_features) |>
    mutate(data = map2(path, var_id, read_nc_feature_socio)) |>
    pull(data) |>
    reduce(inner_join) |>
    mutate(across(where(is.numeric), scale))

  # "inner join" spheres
  observations <- intersect(socio_df$observation, atmo_df$observation)
  observations <- intersect(observations, bio_df$observation)

  prep_cca <- function(data) {
    data |>
      filter(observation %in% observations) |>
      mutate(across(everything(), ~ replace_na(., 0))) |>
      column_to_rownames("observation")
  }

  atmo_mat <- prep_cca(atmo_df)
  bio_mat <- prep_cca(bio_df)
  socio_mat <- prep_cca(socio_df)

  # setup all canonical analyses
  ccas <- list(
    "bio-socio" = stats::cancor(bio_mat, socio_mat),
    "bio-atmo" = stats::cancor(bio_mat, atmo_mat),
    "socio-bio" = stats::cancor(socio_mat, bio_mat),
    "socio-atmo" = stats::cancor(socio_mat, atmo_mat),
    "atmo-socio" = stats::cancor(atmo_mat, socio_mat),
    "atmo-bio" = stats::cancor(atmo_mat, bio_mat)
  )

  mats <- list(
    "atmo" = atmo_df,
    "bio" = bio_df,
    "socio" = socio_df
  ) |>
    lapply(prep_cca)

  ccas <-
    expand_grid(
      X = c("atmo", "bio", "socio"),
      Y = c("atmo", "bio", "socio")
    ) |>
    filter(X != Y) |>
    mutate(
      cca = map2(X, Y, ~ stats::cancor(mats[[.x]], mats[[.y]])),
      scores = pmap(list(X, Y, cca), function(X, Y, cca) {
        U <- as.matrix(mats[[X]]) %*% cca$xcoef
        V <- as.matrix(mats[[Y]]) %*% cca$ycoef

        tibble(
          CCA1 = U[, 1],
          CCA2 = U[, 2],
          ID = rownames(U)
        ) |>
          separate(ID, c("geo", "time"), sep = "_") |>
          mutate(country = geo |> str_extract("^[A-z]+"))
      }),
      loadings = pmap(list(cca), function(cca) {
        tibble(
          var = rownames(cca$xcoef),
          CCA1 = cca$xcoef[, 1],
          CCA2 = cca$xcoef[, 2]
        )
      })
    )
  return(ccas)
}
