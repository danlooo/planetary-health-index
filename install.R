#!/usr/bin/env Rscript

options(repos = c(CRAN = "https://packagemanager.rstudio.com/all/__linux__/jammy/latest"))

install.packages(c(
    "bslib",
    "crew",
    "eurostat",
    "ggnewscale",
    "ggsci",
    "giscoR",
    "httr2",
    "languageserver",
    "ncdf4",
    "plotly",
    "rnaturalearth",
    "rnaturalearthdata",
    "RNetCDF",
    "rrcov3way",
    "sf",
    "shiny",
    "shinycssloaders",
    "targets",
    "tarchetypes",
    "khroma"
))
