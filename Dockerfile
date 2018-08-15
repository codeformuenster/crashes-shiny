FROM rocker/rstudio:3.5.0

# load shiny into rstudio image (see https://github.com/rocker-org/shiny/pull/31/)
EXPOSE 3838

RUN export ADD="shiny" && \
    bash /etc/cont-init.d/add

COPY ./shinyapp /srv/shiny-server/

# install system package needed for some R packages
RUN apt-get update && apt-get install -y --no-install-recommends libpng-dev

# install R packages so that they are cached (in opposite to installing them via R)
RUN install2.r --error leaflet leaflet.extras dplyr shinycssloaders DT

CMD ["/usr/bin/shiny-server"]
