FROM rocker/shiny:4.2.1

ENV SHINY_LOG_STDERR 1

RUN apt-get update && apt-get install -y \
    libcurl4-gnutls-dev \
    libssl-dev \
    libxml2 \
    libudunits2-dev \
    libproj-dev \
    libgdal-dev

RUN R -e 'install.packages(c(\
        "shiny",\
        "plotly",\
        "leaflet",\
        "leaflet.extras",\
        "plyr",\
        "dplyr",\
        "rgdal",\
        "shinyWidgets",\
        "rnaturalearth",\
        "tidyverse",\
        "sf",\
        "rgeos",\
        "httr",\
        "jsonlite",\
        "raster",\
        "data.table",\
        "DT",\
        "shinycssloaders",\
        "RColorBrewer",\
        "shinydisconnect",\
        "shinyjs",\
        "leaflet.multiopacity"\
    ),\
    repos="https://packagemanager.rstudio.com/cran/__linux__/focal/2022-09-02"\
)'

COPY ./dashboard-urbanshift/* /srv/shiny-server/

EXPOSE 3838

# WAF blocks UA-less requests
RUN echo 'GDAL_HTTP_USERAGENT=GDAL' >> /usr/local/lib/R/etc/Renviron.site && \
    echo 'GDAL_DISABLE_READDIR_ON_OPEN=EMPTY_DIR' >> /usr/local/lib/R/etc/Renviron.site

CMD ["/usr/bin/shiny-server"]
