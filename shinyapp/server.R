library(shiny)

# Define server logic required to draw the map
server <- function(input, output, session) {

  # open connection to database
  db_con <- dbConnect(dbDriver("PostgreSQL"), dbname = "ms_unfaelle",
                    # TODO
                     host = "localhost", port = 5432,
                    #host = "accidents-shiny-postgis", port = 5432,
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

  crashes_filtered <- eventReactive(input$QueryBtn, ignoreNULL = FALSE, {
    # filter involved vehicles
    
    ped_filter <- FALSE
    bike_filter <- FALSE
    car_filter <- FALSE
    truck_filter <- FALSE
    rest_filter <- FALSE

    if ("ped" %in% input$vehicles) {
      ped_filter <- TRUE
    }
    if ("bike" %in% input$vehicles) {
      bike_filter <- TRUE
    }
    if ("car" %in% input$vehicles) {
      car_filter <- TRUE
    }
    if ("truck" %in% input$vehicles) {
      truck_filter <- TRUE
    }
    if ("rest" %in% input$vehicles) {
      rest_filter <- TRUE
    }
    
    # filter injuries
    
    slightly_filter <- FALSE
    seriously_filter <- FALSE
    dead_filter <- FALSE
    
    if ("slightly" %in% input$injured) {
      slightly_filter <- TRUE
    }
    if ("seriously" %in% input$injured) {
      seriously_filter <- TRUE
    }
    if ("dead" %in% input$injured) {
      dead_filter <- TRUE
    }

    # TODO hit and run?
    # if (input$hit_and_run == "yes") {
    #   filtered <- filtered %>%
    #     filter(flucht == TRUE)
    # } else if (input$hit_and_run == "no") {
    #   filtered <- filtered %>%
    #     filter(flucht == FALSE)
    # }
    
    year_filter <- "("
    for (yidx in 1:length(input$years)) {
    			if (yidx != length(input$years)) {
    				year_filter <- paste0(year_filter, "'" , input$years[yidx], "',")
    			} else {
    				# no comma after last date
    				year_filter <- paste0(year_filter, "'" , input$years[yidx], "'")
    			}
    	}
    year_filter <- paste0(year_filter, ")")
    
    if (length(input$months) > 0) {
      month_filter <- "("
      for (midx in 1:length(input$months)) {
      			if (midx != length(input$months)) {
      				month_filter <- paste0(month_filter, "'" , input$months[midx], "',")
      			} else {
      				# no comma after last date
      				month_filter <- paste0(month_filter, "'" , input$months[midx], "'")
      			}
      	}
      month_filter <- paste0(month_filter, ")")
    } else {
      month_filter <- paste0("(", "'1',", "'2',", "'3',", "'4',", "'5',", "'6',", 
                              "'7',", "'8',", "'9',", "'10',", "'11',", "'12'", ")")
    }
    
    if (length(input$weekdays) > 0) {
      weekdays_filter <- "("
      for (widx in 1:length(input$weekdays)) {
        print(widx)
      			if (widx != length(input$weekdays)) {
      				weekdays_filter <- paste0(weekdays_filter, "'" , input$weekdays[widx], "',")
      			} else {
      				# no comma after last date
      				weekdays_filter <- paste0(weekdays_filter, "'" , input$weekdays[widx], "'")
      			}
      	}
      weekdays_filter <- paste0(weekdays_filter, ")")
    } else {
      weekdays_filter <- paste0("(", "'0',", "'1',", "'2',", "'3',", "'4',", "'5',", "'6'", ")")
    }
    
    sql_string <- paste0("SELECT unfalldaten_raw.*,",
                         " st_x(the_geom) as longitude, st_y(the_geom) as latitude,",
                         " parsed_timestamp, parsed_german_weekday",
                         " FROM unfalldaten_raw",
                         " JOIN unfalldaten_geometries on unfalldaten_raw.id = unfalldaten_geometries.unfall_id",
                         " JOIN unfalldaten_timestamps on unfalldaten_raw.id = unfalldaten_timestamps.unfall_id",
                         " WHERE EXTRACT(year from parsed_timestamp) in ", year_filter,
                         " AND EXTRACT(month from parsed_timestamp) in ", month_filter,
                         " AND EXTRACT(hours from parsed_timestamp) >= ", input$hour_filter[1],
                         " AND EXTRACT(hours from parsed_timestamp) < ", input$hour_filter[2],
                         " AND EXTRACT(dow from parsed_timestamp) in ", weekdays_filter,
                         # if_else(vehicle_filter, " AND (", ""),
                         if_else(ped_filter, " AND REGEXP_REPLACE(COALESCE(fg, '0'), '[^0-9]*' ,'0')::integer > 0", ""),
                         if_else(bike_filter, " AND REGEXP_REPLACE(COALESCE(rf, '0'), '[^0-9]*' ,'0')::integer > 0", ""),
                         if_else(car_filter, " AND REGEXP_REPLACE(COALESCE(pkw, '0'), '[^0-9]*' ,'0')::integer > 0", ""),
                         if_else(truck_filter, " AND REGEXP_REPLACE(COALESCE(lkw, '0'), '[^0-9]*' ,'0')::integer > 0", ""),
                         if_else(rest_filter, paste0(" AND (REGEXP_REPLACE(COALESCE(mofa, '0'), '[^0-9]*' ,'0')::integer ",
                         "   + REGEXP_REPLACE(COALESCE(kkr, '0'), '[^0-9]*' ,'0')::integer ",
                         "   + REGEXP_REPLACE(COALESCE(krad, '0'), '[^0-9]*' ,'0')::integer ",
                         "   + REGEXP_REPLACE(COALESCE(kom, '0'), '[^0-9]*' ,'0')::integer ",
                         "   + REGEXP_REPLACE(COALESCE(sonstige, '0'), '[^0-9]*' ,'0')::integer) > 0"), ""),
                         if_else(slightly_filter, " AND REGEXP_REPLACE(COALESCE(lv, '0'), '[^0-9]*' ,'0')::integer > 0", ""),
                         if_else(seriously_filter," AND REGEXP_REPLACE(COALESCE(sv, '0'), '[^0-9]*' ,'0')::integer > 0", ""),
                         if_else(dead_filter," AND REGEXP_REPLACE(COALESCE(t, '0'), '[^0-9]*' ,'0')::integer > 0", "")
                        )
                        #" LIMIT 20000;")

    print(sql_string)

    filtered <- dbGetQuery(db_con, sql_string)

    print(head(filtered))

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
        mutate(kom = text_to_number(kom)) %>%
        mutate(sonstige = text_to_number(sonstige))
    }

    return(filtered)
  })

  output$number_of_crashes <- renderText({
    paste0("Anzahl Unfälle: ", nrow(crashes_filtered()))
  })

  # LEAFLET -----------------------------------------------------------------

  
  output$karte <- renderLeaflet({
    # pal11 <- colorNumeric(palette = "PuRd",
                          # crashes_filtered()$anzahl_beteiligte)
    # pal12 <- colorNumeric(palette = "PuBuGn",
                          # ks4_sp_ll()@data$pears)
    # pal13 <- colorNumeric(palette = "YlOrRd",
    #                       ks4_sp_ll()@data$total_area)

    crashes_to_plot <- crashes_filtered()
    visualization_options <- c("Heatmap")
    
    print(nrow(crashes_to_plot))
    
    print(nrow(crashes_filtered()))
    
    if (nrow(crashes_to_plot) < 2000) {
      visualization_options <- c(visualization_options, "Markers")
    }

    if (nrow(crashes_to_plot > 0)) {

      map <- leaflet(data = crashes_to_plot) %>%
        setView(lat = 51.96, lng = 7.62, zoom = 12) %>%
        addProviderTiles(provider = "Esri.WorldImagery", group = "Terrain") %>%
        addProviderTiles(provider = "CartoDB.Positron", group = "schematisch") %>%
        addMarkers(lng = ~longitude,
                   lat = ~latitude,
                   popup = paste0(crashes_filtered()$tag,
                                 ", ", crashes_filtered()$datum,
                                 ", ", crashes_filtered()$uhrzeit,
                                 ", id: ", crashes_filtered()$id,
                                 ", ", crashes_filtered()$vu_ort,
                                 " ", crashes_filtered()$vu_hoehe,
                                 ",<br>Tote: ", crashes_filtered()$t,
                                 ", Schwerverletzte: ", crashes_filtered()$sv,
                                 ", Leichtverletzte: ", crashes_filtered()$lv,
                                 ",<br>PKW: ", crashes_filtered()$pkw,
                                 ", LKW: ", crashes_filtered()$lkw,
                                 ", Fußgänger: ", crashes_filtered()$fg,
                                 ", Fahrräder: ", crashes_filtered()$rf,
                                 ", sonstige Verkehrsmittel : ", crashes_filtered()$mofa +
                                   crashes_filtered()$kkr +
                                   crashes_filtered()$krad +
                                   crashes_filtered()$kom +
                                   crashes_filtered()$sonstige),
                   group = "Markers") %>%
        addWebGLHeatmap(lng = ~longitude,
                        lat = ~latitude,
                        # intensity = 0.5, TODO use interactively with slider
                        size = 500, units = "m",
                        group = "Heatmap") %>%
        ### Groups
        hideGroup("Markers") %>% 
        hideGroup("Terrain") %>%
        showGroup("schematisch)") %>%
        # Layers control
        addLayersControl(
          baseGroups = c("Terrain",
                         "schematisch"),
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

   output$crashes_table <- DT::renderDataTable({
    all_crashes <- dbGetQuery(db_con, "SELECT * FROM unfalldaten_raw")

    DT::datatable(all_crashes, options = list(orderClasses = TRUE))
  })
}


