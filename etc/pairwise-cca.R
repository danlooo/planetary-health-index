#!/usr/bin/env R

#
# Pairwise canonical correlation analysis (CCA)
# to explore coupling between two spheres (Biosphere, atmosphere, sociosphere)
#

dyn.load("/opt/ohpc/pub/libs/hwloc/lib/libhwloc.so.15")
dyn.load("/opt/ohpc/pub/libs/gnu9/openmpi4/hdf5/1.10.8/lib/libhdf5_hl.so.100")
dyn.load("/opt/ohpc/pub/apps/gdal/3.5.1/lib/libgdal.so.31")


library(tidyverse)
library(patchwork)
library(ncdf4)
library(arrow)

source("_targets.R")


# CCA requires each sphere to be a nxm matrix of observations with n observations (pairs of space and time) and m features

read_nc_feature_soc <- function(path, var_id) {
  nc <- nc_open(path)

  mat <- ncvar_get(nc, var_id)
  rownames(mat) <- nc$dim$TIME_PERIOD$vals
  colnames(mat) <- nc$dim$geo$vals

  as_tibble(mat) |>
    mutate(TIME_PERIOD = nc$dim$TIME_PERIOD$vals) |>
    pivot_longer(-TIME_PERIOD, names_to = "geo", values_to = var_id) |>
    unite("observation", geo, TIME_PERIOD)
}

read_nc_feature_bioatmo <- function(var_id, path = "/Net/Groups/BGI/work_2/EuropeanPHI/countrydatacubes/level_3_quarter.nc") {
  nc <- nc_open(path)

  mat <- ncvar_get(nc, var_id)
  rownames(mat) <- nc$dim$time$vals
  colnames(mat) <- nc$dim$country$vals

  as_tibble(mat) |>
    mutate(TIME_PERIOD = nc$dim$time$vals) |>
    pivot_longer(-TIME_PERIOD, names_to = "geo", values_to = var_id) |>
    unite("observation", geo, TIME_PERIOD)
}

atmo_df <-
  list(
    read_nc_feature_bioatmo("ET"),
    read_nc_feature_bioatmo("ET_T")
  ) |>
  reduce(left_join) |>
  mutate(across(where(is.numeric), scale))

bio_df <-
  list(
    read_nc_feature_bioatmo("Day_AQUA_Mxx21x1_061_gapfilled_QCflags_dyn"),
    read_nc_feature_bioatmo("Night_AQUA_Mxx21x1_061_gapfilled_QCflags_dyn"),
    read_nc_feature_bioatmo("NDWI_band7gapfilled_061_QCdyn"),
    read_nc_feature_bioatmo("EVIgapfilled_061_QCdyn"),
    read_nc_feature_bioatmo("NDWI_band7gapfilled_061_QCdyn"),
    read_nc_feature_bioatmo("NDVIgapfilled_061_QCdyn"),
    read_nc_feature_bioatmo("NIRvgapfilled_061_QCdyn"),
    read_nc_feature_bioatmo("NEE"),
    read_nc_feature_bioatmo("ET"),
    read_nc_feature_bioatmo("ET_T"),
    read_nc_feature_bioatmo("GPP"),
    read_nc_feature_bioatmo("skt")
  ) |>
  reduce(left_join) |>
  mutate(across(where(is.numeric), scale))

soc_df <-
  list(
    read_nc_feature_soc("data/eurostat-nc/nama_10_nfa_bs.nc", "nama_10_nfa_bs_S13_N21ON"),
    read_nc_feature_soc("data/eurostat-nc/nama_10_nfa_bs.nc", "nama_10_nfa_bs_S11_N13N"),
    read_nc_feature_soc("data/eurostat-nc/nama_10_nfa_bs.nc", "nama_10_nfa_bs_S12_N1173N")
  ) |>
  reduce(left_join) |>
  mutate(across(where(is.numeric), scale))

# "inner join" spheres
observations <- intersect(soc_df$observation, atmo_df$observation)
observations <- intersect(observations, bio_df$observation)

prep_cca <- function(data) {
  data |>
    filter(observation %in% observations) |>
    mutate(across(everything(), ~ replace_na(., 0))) |>
    column_to_rownames("observation")
}

biplot_cca <- function(X, Y, cca) {
  U <- as.matrix(X) %*% cca$xcoef
  V <- as.matrix(Y) %*% cca$ycoef

  scores_df <- tibble(
    Can1 = U[, 1],
    Can2 = U[, 2],
    ID = rownames(U)
  ) |>
    separate(ID, c("geo", "time"), sep = "_") |>
    mutate(country = geo |> str_extract("^[A-z]+"))

  arrow_scale <- 1

  loadings_df <- tibble(
    var = rownames(cca$xcoef),
    x = 0,
    y = 0,
    xend = cca$xcoef[, 1] * arrow_scale - mean(scores_df$Can1),
    yend = cca$xcoef[, 2] * arrow_scale - mean(scores_df$Can2)
  )

  scores_df |>
    arrange(geo, time) |>
    ggplot(aes(x = Can1, y = Can2)) +
    geom_density2d_filled() +
    geom_density_2d(color = "darkgrey") +
    scale_fill_grey(start = 1, end = 0) +
    guides(fill = "none") +
    # geom_point(aes(alpha = 0.1)) +
    geom_line(
      data = ~ filter(.x, geo %in% c("AT111", "AT112")),
      mapping = aes(color = geo)
    ) +
    geom_segment(
      data = loadings_df,
      mapping = aes(x = x, y = y, xend = xend, yend = yend),
      arrow = arrow(length = unit(0.2, "cm")),
      color = "darkred"
    ) +
    geom_text(
      data = loadings_df,
      mapping = aes(x = xend, y = yend, label = var),
      color = "darkred",
      hjust = 1.1
    ) +
    theme_classic() +
    coord_equal() +
    labs(
      title = "Canonical Correlation Biplot, z-scale",
      x = "CCA1",
      y = "CCA2"
    )
}


mats <- list(
  "atmo" = atmo_df,
  "bio" = bio_df,
  "soc" = soc_df
) |>
  lapply(prep_cca)

ccas <-
  expand_grid(
    X = c("atmo", "bio", "soc"),
    Y = c("atmo", "bio", "soc")
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
    }),
    plt = pmap(list(X, Y, cca), ~ biplot_cca(mats[[..1]], mats[[..2]], ..3))
  )

# biplot
ccas$plt[[1]]

# combined loadings plot
ccas |>
  select(X, Y, loadings) |>
  unnest() |>
  arrange(X) |>
  mutate(var = fct_reorder(var, X)) |>
  ggplot(aes(var, CCA1, color = Y)) +
  geom_point(size = 3) +
  geom_hline(yintercept = 0) +
  scale_color_manual(values = sphere_colors) +
  scale_y_continuous(limits = c(-0.03, 0.03)) +
  coord_flip() +
  theme_classic() +
  labs(
    title = "Loadings",
    subtitle = "Pairwise Canonical Correlation Analysis",
    x = "Explaining variable",
    color = "Target Sphere"
  )
