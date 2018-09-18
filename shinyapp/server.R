library(shiny)

# Define server logic required to draw the map
server <- function(input, output, session) {

  # open connection to database
  db_con <- dbConnect(dbDriver("PostgreSQL"), dbname = "ms_unfaelle",
                    host = Sys.getenv("DB_HOST"), port = 5432,
                    user = "postgres", password = "ms_unfaelle")

  # close connection to database once shiny session ended
  session$onSessionEnded(
    function(){
    if (!is.null(db_con)) {
      dbDisconnect(db_con)
    }
  })

  text_to_number <- function(text) {
    return(ifelse(is.na(as.numeric(text)), 0, as.numeric(text)))
  }

  accidents_filtered <- eventReactive(input$QueryBtn, ignoreNULL=FALSE, {

    pedfilter <- 0
    bikefilter <- 0
    carfilter <- 0
    truckfilter <- 0
    restfilter <- 0

    # involved vehicles
    if ("ped" %in% input$vehicles) {
      pedfilter <- 1
    }
    if ("bike" %in% input$vehicles) {
      bikefilter <- 1
    }
    if ("car" %in% input$vehicles) {
      carfilter <- 1
    }
    if ("truck" %in% input$vehicles) {
      truckfilter <- 1
    }
    if ("rest" %in% input$vehicles) {
      restfilter <- 1
    }

    sql_string <- paste0("SELECT unfalldaten_raw.*,",
                          " st_x(the_geom) as longitude, st_y(the_geom) as latitude",
                          " FROM unfalldaten_raw",
                          " JOIN unfalldaten_geometries on unfalldaten_raw.id = unfalldaten_geometries.unfall_id",
                          " WHERE REGEXP_REPLACE(COALESCE(fg, '0'), '[^0-9]*' ,'0')::integer >= ", pedfilter,
                          " AND REGEXP_REPLACE(COALESCE(rf, '0'), '[^0-9]*' ,'0')::integer >= ", bikefilter,
                          " AND REGEXP_REPLACE(COALESCE(pkw, '0'), '[^0-9]*' ,'0')::integer >= ", carfilter,
                          " AND REGEXP_REPLACE(COALESCE(lkw, '0'), '[^0-9]*' ,'0')::integer >= ", truckfilter,
                          " AND (REGEXP_REPLACE(COALESCE(mofa, '0'), '[^0-9]*' ,'0')::integer) ",
                          "   + (REGEXP_REPLACE(COALESCE(kkr, '0'), '[^0-9]*' ,'0')::integer) ",
                          "   + (REGEXP_REPLACE(COALESCE(krad, '0'), '[^0-9]*' ,'0')::integer) ",
                          "   + (REGEXP_REPLACE(COALESCE(kom, '0'), '[^0-9]*' ,'0')::integer) ",
                          "   + (REGEXP_REPLACE(COALESCE(sonstige, '0'), '[^0-9]*' ,'0')::integer) >= ",truckfilter,
                          " LIMIT 200;")

    print(sql_string)

    filtered <- dbGetQuery(db_con, sql_string)

    print(head(filtered))

    # hit and run?
    if (input$hit_and_run == "yes") {
      filtered <- filtered %>%
        filter(flucht == TRUE)
    } else if (input$hit_and_run == "no") {
      filtered <- filtered %>%
        filter(flucht == FALSE)
    }

    if (nrow(filtered) > 0) {

      filtered <- filtered %>%
        # mutate(flucht = if_else(flucht == "", FALSE, TRUE)) %>%
        mutate(sv = text_to_number(sv)) %>%
        mutate(lv = text_to_number(lv)) %>%
        mutate(t = text_to_number(t)) %>%
        mutate(fg = text_to_number(fg)) %>%
        mutate(rf = text_to_number(rf)) %>%
        mutate(pkw = text_to_number(pkw)) %>%
        mutate(lkw = text_to_number(lkw)) %>%
        mutate(mofa = text_to_number(mofa)) %>%
        mutate(kkr = text_to_number(kkr)) %>%
        mutate(krad = text_to_number(krad)) %>%
        mutate(sonstige = text_to_number(sonstige))
    }

    return(filtered)
  })

  # LEAFLET -----------------------------------------------------------------

  output$number_of_accidents <- renderText({
    paste0("Anzahl Unfälle: ", nrow(accidents_filtered()))
  })

  output$karte <- renderLeaflet({
    # pal11 <- colorNumeric(palette = "PuRd",
                          # accidents_filtered()$anzahl_beteiligte)
    # pal12 <- colorNumeric(palette = "PuBuGn",
                          # ks4_sp_ll()@data$pears)
    # pal13 <- colorNumeric(palette = "YlOrRd",
    #                       ks4_sp_ll()@data$total_area)

    accidents_to_plot <- accidents_filtered()
    visualization_options <- c("Heatmap")

    if (nrow(accidents_to_plot) < 2000) {
      visualization_options <- c(visualization_options, "Markers")
    }

    if (nrow(accidents_to_plot > 0)) {

      map <- leaflet(data = accidents_to_plot) %>%
        setView(lat = 51.96, lng = 7.62, zoom = 12) %>%
        addProviderTiles(provider = "Esri.WorldImagery", group = "Terrain") %>%
        addProviderTiles(provider = "OpenStreetMap.BlackAndWhite", group = "OSM (B & W)") %>%
        addCircles(lng = ~longitude,
                   lat = ~latitude,
                   color = "black",
                   opacity = 0.8,
                   weight = 0.5,
                   radius = 200,  #  Radius could be assigned to a another variable
                   fillOpacity = 0.5,
                   popup = paste0(accidents_filtered()$tag,
                                 ",", accidents_filtered()$datum,
                                 ",", accidents_filtered()$uhrzeit,
                                 ", id: ",  accidents_filtered()$id,
                                 ", ", accidents_filtered()$vu_ort,
                                 " ", accidents_filtered()$vu_hoehe,
                                 ",\nTote: ",  accidents_filtered()$t,
                                 ", Schwerverletzte: ",  accidents_filtered()$sv,
                                 ", Leichtverletzte: ",  accidents_filtered()$lv,
                                 ", PKW: ", accidents_filtered()$pkw,
                                 ", LKW: ", accidents_filtered()$lkw,
                                 ", Fußgänger: ", accidents_filtered()$fg,
                                 ", Fahrräder: ", accidents_filtered()$rf,
                                 ", sonstige: ", accidents_filtered()$mofa +
                                   accidents_filtered()$kkr +
                                   accidents_filtered()$krad +
                                   accidents_filtered()$sonstige),
                   group = "Markers") %>%
        addWebGLHeatmap(lng = ~longitude,
                        lat = ~latitude,
                        size = 500, units = "m",
                        group = "Heatmap") %>%
        ### Groups
        hideGroup("Markers") %>%
        hideGroup("Terrain") %>%
        showGroup("OSM (B & W)") %>%
        # Layers control
        addLayersControl(
          baseGroups = c("Terrain",
                         "OSM (B & W)"),
          overlayGroups = visualization_options,
          options = layersControlOptions(collapsed = FALSE)
        )
    } else {
      map <- leaflet() %>%
        setView(lat = 51.96, lng = 7.62, zoom = 12) %>%
        addProviderTiles(provider = "Esri.WorldImagery", group = "Terrain") %>%
        addProviderTiles(provider = "OpenStreetMap.BlackAndWhite", group = "OSM (B & W)") %>%
        hideGroup("Terrain") %>%
        showGroup("OSM (B & W)") %>%
        # Layers control
        addLayersControl(
          baseGroups = c("Terrain",
                         "OSM (B & W)"),
          options = layersControlOptions(collapsed = FALSE)
        )
    }

    map
  })

   output$accidents_table <- DT::renderDataTable({
    all_accidents <- dbGetQuery(db_con, "SELECT * FROM unfalldaten_raw")

    DT::datatable(all_accidents, options = list(orderClasses = TRUE))
  })
}


