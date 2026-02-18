# Use the official tidyverse image as the base image
FROM rocker/geospatial:4.3.3

# Install dependencies
RUN apt-get update && apt-get install -y \
    sudo \
    libcurl4-gnutls-dev \
    libudunits2-dev \
    libcairo2-dev \
    libxt-dev \
    libssl-dev \
    libssh2-1-dev \
    gnupg-utils \
    libglpk-dev \
    libnetcdf-dev

COPY install.R ./
RUN Rscript install.R

WORKDIR /srv/shiny-server
COPY *.R ./
COPY data data/
COPY _targets _targets/
RUN mkdir out
RUN R -e "targets::tar_make()"
EXPOSE 80
CMD R -e "shiny::runApp('/srv/shiny-server', host='0.0.0.0', port=80)"