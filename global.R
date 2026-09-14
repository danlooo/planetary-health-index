library(shiny)
library(tidyverse)
library(bslib)
library(plotly)
library(ggsci)
library(targets)
library(shinycssloaders)
library(ncdf4)
library(lubridate)
library(eurostat)
library(rnaturalearth)
library(khroma)
library(yaml)

source("lib.R")

shinyOptions(cache = cachem::cache_mem(max_size = 1e9))
enableBookmarking("server")

# Load land and ocean data
ocean_sf <- ne_download(scale = 50, type = "ocean", category = "physical", returnclass = "sf")
land_sf <- ne_countries(scale = "medium", returnclass = "sf")

tar_load(nuts3_sf)
tar_load(features)
tar_load(eurostat_metadata)
tar_load(nuts3_regions)
tar_load(cube_tbl)
tar_load(global_stats)
tar_load(annual_stats)
tar_load(quarterly_stats)
tar_load(geo_stats)
all_preselected_features <- tar_read(preselected_features)

theme_set(
    theme_classic(base_size = 18) + theme(
        legend.position = "bottom",
        legend.margin = margin(t = 0, b = 0),
        legend.box.spacing = unit(0, "pt"),
        plot.margin = margin(t = 2, r = 2, b = 2, l = 2),
        axis.title.x = element_text(margin = margin(t = 4)),
        axis.title.y = element_text(margin = margin(r = 4)),
        axis.text.x = element_text(margin = margin(t = 2)),
        axis.text.y = element_text(margin = margin(r = 2)),
        axis.ticks.length = unit(2, "pt")
    )
)
quarters <- seq(
    from = ymd("2001-01-01"),
    to   = ymd("2025-12-31"),
    by   = "3 months"
)

withSpinner <- partial(shinycssloaders::withSpinner, color = primary_color, type = 8)
