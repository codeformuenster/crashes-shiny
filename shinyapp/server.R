library(shiny)

# Define server logic required to draw the map
server <- function(input, output, session) {
  
  global_vars = reactiveValues(years_button_toggle = TRUE, filter_changed = FALSE)

  # open connection to database
  db_con <- dbConnect(RPostgres::Postgres(), dbname = "postgres",
                    host = Sys.getenv("POSTGRES_HOST"), port = 5432,
                    user = "postgres", password = "ms_unfaelle")

  # close connection to database once shiny session ended
  session$onSessionEnded(
    function(){
    if (!is.null(db_con)) {
      dbDisconnect(db_con)
    }
  })
  
  crashes_filtered <- eventReactive(input$update_button, ignoreNULL = FALSE, {
    
    # filter involved vehicles (with)
    
    ped_filter <- FALSE
    bike_filter <- FALSE
    car_filter <- FALSE
    truck_filter <- FALSE
    bus_filter <- FALSE
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
    if ("bus" %in% input$vehicles) {
      bus_filter <- TRUE
    }
    if ("rest" %in% input$vehicles) {
      rest_filter <- TRUE
    }
    
    # filter *not* involved vehicles (without)
    
    without_ped_filter <- FALSE
    without_bike_filter <- FALSE
    without_car_filter <- FALSE
    without_truck_filter <- FALSE
    without_bus_filter <- FALSE
    without_rest_filter <- FALSE
    
    if ("ped" %in% input$without_vehicles) {
      without_ped_filter <- TRUE
    }
    if ("bike" %in% input$without_vehicles) {
      without_bike_filter <- TRUE
    }
    if ("car" %in% input$without_vehicles) {
      without_car_filter <- TRUE
    }
    if ("truck" %in% input$without_vehicles) {
      without_truck_filter <- TRUE
    }
    if ("bus" %in% input$without_vehicles) {
      without_bus_filter <- TRUE
    }
    if ("rest" %in% input$without_vehicles) {
      without_rest_filter <- TRUE
    }
    
    # filter injuries
    
    no_injuries_filter <- FALSE
    slightly_filter <- FALSE
    seriously_filter <- FALSE
    dead_filter <- FALSE
    
    if ("no_injuries" %in% input$injured) {
      no_injuries_filter <- TRUE
    }
    if ("slightly" %in% input$injured) {
      slightly_filter <- TRUE
    }
    if ("seriously" %in% input$injured) {
      seriously_filter <- TRUE
    }
    if ("dead" %in% input$injured) {
      dead_filter <- TRUE
    }
    
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
    
    sql_string <- 
      paste("WITH geo AS",
            "(SELECT",
            "(data->>'accident_id') as accident_id,",
            "(data->>'lat')::float as latitude,",
            "(data->>'lon')::float as longitude",
            "FROM objects WHERE parent_id = '/buckets/accidents/collections/geometries'",
            "AND resource_name = 'record')",
            "SELECT",
            "DISTINCT ON (geo.accident_id) geo.accident_id,",
            "data->>'participants_age_01' as age1,",
            "data->>'participants_age_02' as age2,",
            "data->'participants_01' AS participants_01,",
            "data->'participants_01_registration' AS participants_01_registration,",
            "data->'participants_02' AS participants_02,",
            "data->'participants_02_registration' AS participants_02_registration,",
            "data->>'number_of_participants' as no_of_participants,",
            "date_part('year', date(data->>'date')) AS year,",
            "date_part('month', date(data->>'date')) AS month,",
            "date_part('hour', TO_TIMESTAMP(data->>'time_of_day', 'HH24:MI:SS')::TIME) as hour,",
            "date_part('minute', TO_TIMESTAMP(data->>'time_of_day', 'HH24:MI:SS')::TIME) as minute,",
            "date_part('dow', date(data->>'date')) AS german_weekday,",
            "date_part('day', date(data->>'date')) AS day_of_month,",
            "date(data->>'date') AS full_date,",
            "data->>'place' as street,",
            "data->>'place_near' as street_additional,",
            "data->>'deaths' as deaths,",
            "data->>'seriously_injured' as sv,",
            "data->>'slightly_injured' as lv,",
            "(data->>'car')::integer as car,",
            "(data->>'lorry')::integer as lorry,",
            "(data->>'pedestrian')::integer as pedestrian,",
            "(data->>'bicycle')::integer as bicycle,",
            "(data->>'moped')::integer as moped,",
            "(data->>'omnibus')::integer as omnibus,",
            "(data->>'motorcycle')::integer as motorcycle,",
            "(data->>'small_moped')::integer as small_moped,",
            "(data->>'other_road_user')::integer as other_road_user,",
            "data->>'helmet' as bike_helmet,",
            "data->'source_file' AS source_file,",
            "data->'import_timestamp' AS import_timestamp,",
            "data->'source_file_hash' AS source_file_hash,",
            "data->'source_row_number' AS source_row_number,",
            "data->'day_of_week' AS day_of_week,",
            "data->'time_of_day' AS time_of_day,",
            "data->'accident_category' AS accident_category,",
            "data->'hit_and_run' AS hit_and_run,",
            "data->'urban' AS urban,",
            "data->'extra_urban' AS extra_urban,",
            "data->'alcoholized' AS alcoholized,",
            "data->'accident_type' AS accident_type,",
            "data->'cause_1_4' AS cause_1_4,",
            "data->'cause_2' AS cause_2,",
            "data->'cause_3' AS cause_3,",
            "data->'cause_other' AS cause_other,",
            "data->'cause_02' AS cause_02,",
            "data->'light_conditions' AS light_conditions,",
            "data->'road_condition' AS road_condition,",
            "data->'participants_child' AS participants_child,",
            "data->'participants_18_24' AS participants_18_24,",
            "data->'participants_senior' AS participants_senior,",
            "geo.latitude, geo.longitude",
            "FROM objects JOIN geo ON objects.id = geo.accident_id",
            "WHERE parent_id = '/buckets/accidents/collections/accidents_raw'",
            "AND resource_name = 'record'",
            "AND date_part('year', date(data->>'date')) in ", year_filter,
            "AND date_part('month', date(data->>'date')) in ", month_filter,
            "AND date_part('dow', date(data->>'date')) in ", weekdays_filter,
            "AND date_part('hour', TO_TIMESTAMP(data->>'time_of_day', 'HH24:MI:SS')::TIME) >= ", input$hour_filter[1],
            "AND date_part('hour', TO_TIMESTAMP(data->>'time_of_day', 'HH24:MI:SS')::TIME) < ", input$hour_filter[2],
            if_else(input$bike_helmet, "AND (data->>'helmet') like '%j%'", ""),
            if_else(input$single_participant, "AND (data->>'number_of_participants')::integer = '1'", ""),
            "AND (((data->>'participants_age_01')::integer >= ", input$age_filter[1],
            "AND (data->>'participants_age_01')::integer <= ", input$age_filter[2], ")",
            "OR ((data->>'participants_age_02')::integer >= ", input$age_filter[1],
            "AND (data->>'participants_age_02')::integer <= ", input$age_filter[2], "))",
            if_else(car_filter, " AND data->>'car' > '0'", ""),
            if_else(ped_filter," AND data->>'pedestrian' > '0'", ""),
            if_else(bike_filter, " AND data->>'bicycle' > '0'", ""),
            if_else(truck_filter, " AND data->>'lorry' > '0'", ""),
            if_else(bus_filter, " AND data->>'omnibus' > '0'", ""),
            if_else(rest_filter, paste0(" AND (((data->>'moped')::integer",
                                       " + (data->>'motorcycle')::integer",
                                       " + (data->>'small_moped')::integer",
                                       " + (data->>'other_road_user')::integer) > '0')"), ""),
            if_else(without_car_filter, " AND data->>'car' = '0'", ""),
            if_else(without_ped_filter," AND data->>'pedestrian' = '0'", ""),
            if_else(without_bike_filter, " AND data->>'bicycle' = '0'", ""),
            if_else(without_truck_filter, " AND data->>'lorry' = '0'", ""),
            if_else(without_bus_filter, " AND data->>'omnibus' = '0'", ""),
            if_else(without_rest_filter, paste0(" AND (((data->>'moped')::integer",
                                       " + (data->>'motorcycle')::integer",
                                       " + (data->>'small_moped')::integer",
                                       " + (data->>'other_road_user')::integer) = '0')"), ""),
            if_else(no_injuries_filter | slightly_filter | seriously_filter | dead_filter, "AND (", ""),
            if_else(no_injuries_filter, paste0(" (data->>'slightly_injured' = '0'",
                              " AND data->>'seriously_injured' = '0'",
                              " AND data->>'deaths' = '0')"), ""),
            if_else(no_injuries_filter & slightly_filter, " OR ", ""),
            if_else(slightly_filter, " data->>'slightly_injured' > '0'", ""),
            if_else((no_injuries_filter | slightly_filter) & seriously_filter, " OR ", ""),
            if_else(seriously_filter," data->>'seriously_injured' > '0'", ""),
            if_else((no_injuries_filter | slightly_filter | seriously_filter) & dead_filter, " OR ", ""),
            if_else(dead_filter," data->>'deaths' > '0'", ""),
            if_else(no_injuries_filter | slightly_filter | seriously_filter | dead_filter, ")", ""))
    
    print(sql_string)
    
    filtered <- dbGetQuery(db_con, sql_string)
    
    print(head(filtered))
    
    # reset color of refresh button
    global_vars$filter_changed <- FALSE
    
    return(filtered)
  })

  # observe all filters and toggle the global variable to color the refresh button
  observeEvent(c(input$vehicles, input$without_vehicles, input$injured,
                 input$age_filter, input$bike_helmet, input$single_participant,
                 input$hour_filter, input$weekdays, input$months, input$years,
                 input$years_button, input$heatmap_toggle, input$markers_toggle,
                 input$heatmap_size, input$heatmap_intensity), {
    global_vars$filter_changed <- TRUE
  })
  
  output$button <- renderUI({
    if (global_vars$filter_changed) {
        actionButton(inputId = "update_button", "Aktualisieren", icon = icon("refresh"),
                     style = "color: white; background-color: #86D956;")
      } else {
        actionButton(inputId = "update_button", "Aktualisieren", icon = icon("refresh"),
                     style = "color: black; background-color: #FFFFFF")
      }
  })

  output$number_of_crashes <- renderText({
    paste0("Anzahl Unfälle: ", nrow(crashes_filtered()))
  })

  # LEAFLET -----------------------------------------------------------------

  center <- reactive({
    if (is.null(input$karte_center)) {
      return(c(51.96, 7.62))
    } else {
      return(input$karte_center)
    }
  })
  
  zoom <- reactive({
    if (is.null(input$karte_zoom)) {
      return(13)
    } else {
      return(input$karte_zoom)
    }
  })
  
  update_map <- function(){
    
    renderLeaflet({
     print(nrow(crashes_filtered()))
     if (nrow(crashes_filtered()) > 0) {
       leaflet(data = crashes_filtered()) %>%
        addProviderTiles(provider = "OpenStreetMap.DE", group = "schematisch") %>% 
        addProviderTiles(provider = "Esri.WorldImagery", group = "Satellit") %>% 
        addProviderTiles(provider = "OpenTopoMap", group = "topographisch") %>% 
        # Layers control
        addLayersControl(
          baseGroups = c("schematisch", "Satellit", "topographisch"),
          options = layersControlOptions(collapsed = FALSE)
        )
    } else {
      leaflet() %>%
         addPopups(lat = 51.96, lng = 7.62,
                   popup = "Es gibt keine Daten, die den aktuellen Filtereinstellungen entsprechen.", 
                    options = popupOptions(closeButton = FALSE))
     }
    })
  }
  
  output$karte <- update_map()
  
  observeEvent(input$reset_map_button, ignoreNULL = FALSE, {
    proxy <- leafletProxy("karte", data = crashes_filtered()) %>% 
      setView(lat = 51.96, lng = 7.62, zoom = 13)
    update_map()
  })
  
  observeEvent(input$bike_helmet, {
    if (input$bike_helmet) {
     updateSelectizeInput(session, "vehicles", selected = c(input$vehicles, "bike")) 
    }
  })
  
  # observe heatmap, ignoreNULL is to have this executed at the start
  observeEvent(input$update_button, ignoreNULL = FALSE, {
    proxy <- leafletProxy("karte", data = crashes_filtered())
    if (length(crashes_filtered()) > 0) {
      if (input$heatmap_toggle) {
        proxy %>% 
          clearGroup("Heatmap") %>% 
           addWebGLHeatmap(lng = ~longitude,
                          lat = ~latitude,
                          intensity = input$heatmap_intensity,
                          size = input$heatmap_size, units = "m",
                          opacity = 0.75,
                          group = "Heatmap") %>%
          showGroup("Heatmap") %>% 
          setView(lat = center()[1], lng = center()[2], zoom = zoom())
      } else {
        proxy %>% 
          clearGroup("Heatmap")
      }
    }
  })
  
  # marker observe, ignoreNULL is to have this executed at the start
  observeEvent(input$update_button, ignoreNULL = FALSE, {
    proxy <- leafletProxy("karte", data = crashes_filtered())
    if (length(crashes_filtered()) > 0) {
      if (input$markers_toggle) {
        proxy %>% 
          clearGroup("Markers") %>% 
          addMarkers(lng = ~longitude,
             lat = ~latitude,
             clusterOptions = markerClusterOptions(),
             popup = paste0(names(weekdays_string_to_numbers[crashes_filtered()$german_weekday + 1]),
                           ", ", crashes_filtered()$day_of_month,
                           ".", crashes_filtered()$month,
                           ".", crashes_filtered()$year,
                           ", ", crashes_filtered()$hour,
                           ":", sprintf("%02d", crashes_filtered()$minute), " Uhr",
                           ", ", crashes_filtered()$street,
                           " ", crashes_filtered()$street_additional,
                           ", Alter: ", crashes_filtered()$age1, " & ", crashes_filtered()$age2, 
                           ",<br>Tote: ", crashes_filtered()$deaths,
                           ", Schwerverletzte: ", crashes_filtered()$sv,
                           ", Leichtverletzte: ", crashes_filtered()$lv,
                           ",<br>PKW: ", crashes_filtered()$car,
                           ", LKW: ", crashes_filtered()$lorry,
                           ", Bus: ", crashes_filtered()$omnibus,
                           ", Fußgänger: ", crashes_filtered()$pedestrian,
                           ", Fahrräder: ", crashes_filtered()$bicycle,
                           ", sonstige Verkehrsmittel: ", 
                             crashes_filtered()$moped +
                             crashes_filtered()$omnibus +
                             crashes_filtered()$motorcycle +
                             crashes_filtered()$small_moped +
                             crashes_filtered()$other_road_user,
                           ", id: ", crashes_filtered()$accident_id),
             group = "Markers") %>%
          showGroup("Markers") %>% 
          setView(lat = center()[1], lng = center()[2], zoom = zoom())
      } else {
        proxy %>% 
          clearGroup("Markers")
      }
    }
  })

  observeEvent(input$years_button, {
    
    if (global_vars$years_button_toggle) {
      updateSelectizeInput(session, "years",
                         selected = c("2007", "2008", "2009", "2010",
                                      "2011", "2012", "2013", "2014",
                                      "2015", "2016", "2017", "2018"))
      updateActionButton(session, "years_button",
                         label = "Nur letztes Jahr",
                         icon = icon("calendar-minus"))
    } else {
      updateSelectizeInput(session, "years",
                         selected = c("2018"))
      updateActionButton(session, "years_button",
                         label = "Alle Jahre",
                         icon = icon("calendar-plus"))
    }
    
    isolate({
      global_vars$years_button_toggle <- !global_vars$years_button_toggle
    })
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$dataset, "gefilterte_unfaelle.csv", sep = "")
    },
    content = function(file) {
      write.csv(crashes_filtered(), file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )
  
  output$crashes_table <- DT::renderDataTable({
    DT::datatable(crashes_filtered())
  })
}


