#!/usr/bin/env Rscript

options(repos = c(CRAN = 'https://packagemanager.rstudio.com/all/__linux__/jammy/latest'))

install.packages(c(
    'shiny', 'plotly', 'ggsci', 'ggnewscale', 'bslib', "RNetCDF",
    'targets', 'shinycssloaders', 'ncdf4', 'crew', 'eurostat', "rrcov3way", "languageserver"
))