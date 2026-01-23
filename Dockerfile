# Use the official tidyverse image as the base image
FROM rocker/tidyverse:4.3.3

ENV DATA_PASSWORD=""

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
COPY app.R ./
COPY lib.R ./
COPY data.gpg ./data.gpg
EXPOSE 80
CMD rm -rf data && mkdir data && \
    gpg --decrypt --batch --yes --pinentry-mode loopback --passphrase $DATA_PASSWORD data.gpg | tar -xzf - -C . && \
    R -e "shiny::runApp('/srv/shiny-server', host='0.0.0.0', port=80)"