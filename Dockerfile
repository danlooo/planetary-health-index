# Use the official tidyverse image as the base image
FROM rocker/tidyverse:4.1.0

ENV DATA_PASSWORD=""

# Install dependencies
RUN apt-get update && apt-get install -y \
    sudo \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    libssl-dev \
    libssh2-1-dev \
    gnupg-utils

RUN echo "options(repos = c(CRAN = 'https://packagemanager.rstudio.com/all/__linux__/focal/latest'))" >> /usr/local/lib/R/etc/Rprofile.site
RUN R -e "install.packages(c('shiny', 'plotly', 'ggsci', 'ggnewscale', 'bslib'))"
WORKDIR /srv/shiny-server
COPY app.R ./
COPY data.gpg ./data.gpg
EXPOSE 80
CMD rm -rf data && mkdir data && \
    gpgtar --decrypt --directory . --gpg-args "--passphrase=${DATA_PASSWORD} --batch" data.gpg && \
    R -e "shiny::runApp('/srv/shiny-server', host='0.0.0.0', port=80)"