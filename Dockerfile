FROM rocker/rstudio:3.5.2

RUN apt-get update && apt-get install -y --no-install-recommends libpng-dev libpq-dev \
  && install2.r --error leaflet leaflet.extras dplyr shinycssloaders DT RPostgres jsonlite stringi plotly \
  && ADD="shiny" bash /etc/cont-init.d/add \
  && chown shiny:shiny /var/lib/shiny-server

USER shiny
COPY ./shinyapp /srv/shiny-server/

EXPOSE 3838
CMD ["/usr/bin/shiny-server"]
