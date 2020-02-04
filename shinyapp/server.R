library(shiny)
library(plotly)

# Define server logic required to draw the map
server <- function(input, output, session) {
  
  global_vars = reactiveValues(years_button_toggle = TRUE,
                               filter_changed = FALSE,
                               update_visible_data = TRUE,
                               crashes_static = NULL)

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
  
  crashes_filtered <- eventReactive(global_vars$update_visible_data, ignoreInit = TRUE, {
    
    crashes_static <- NULL
    
    # de-validate map
    # filtered <- NULL
    
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
    
    # filter causer (Hauptverursacher)
    
    ped_causer_filter <- FALSE
    bike_causer_filter <- FALSE
    car_causer_filter <- FALSE
    truck_causer_filter <- FALSE
    bus_causer_filter <- FALSE
    
    if ("ped" %in% input$causer) {
      ped_causer_filter <- TRUE
    }
    if ("bike" %in% input$causer) {
      bike_causer_filter <- TRUE
    }
    if ("car" %in% input$causer) {
      car_causer_filter <- TRUE
    }
    if ("truck" %in% input$causer) {
      truck_causer_filter <- TRUE
    }
    if ("bus" %in% input$causer) {
      bus_causer_filter <- TRUE
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
    
    type_filter <- "("
    for (tidx in 1:length(input$type)) {
      if (tidx != length(input$type)) {
        type_filter <- paste0(type_filter, "'" , input$type[tidx], "',")
      } else {
        # no comma after last entry
        type_filter <- paste0(type_filter, "'" , input$type[tidx], "'")
      }
    }
    type_filter <- paste0(type_filter, ")")
    
    type_detail_temp <- input$type_detail

    # remove whitespace
    type_detail_temp <- gsub(" ", "", type_detail_temp, fixed = TRUE)
    # split at ','
    type_detail_temp <- strsplit(type_detail_temp, split = ",")
    # create a vector
    type_detail_temp <- unlist(type_detail_temp)
    # only consider three-letter codes
    type_detail_temp <- type_detail_temp[nchar(type_detail_temp) == 3]
    
    if (length(type_detail_temp) > 0) {
      # create a string
      type_detail_filter <- "("
      for (tidx in 1:length(type_detail_temp)) {
        if (tidx != length(type_detail_temp)) {
          type_detail_filter <- paste0(type_detail_filter, "'" , type_detail_temp[tidx], "',")
        } else {
          # no comma after last entry
          type_detail_filter <- paste0(type_detail_filter, "'" , type_detail_temp[tidx], "'")
        }
      }
      type_detail_filter <- paste0(type_detail_filter, ")")
    } else {
      type_detail_filter <- NA
    }
    
    crash_cause_temp <- input$crash_cause

    # remove whitespace
    crash_cause_temp <- gsub(" ", "", crash_cause_temp, fixed = TRUE)
    # split at ','
    crash_cause_temp <- strsplit(crash_cause_temp, split = ",")
    # create a vector
    crash_cause_temp <- unlist(crash_cause_temp)
    # only consider two-letter codes
    crash_cause_temp <- crash_cause_temp[nchar(crash_cause_temp) == 2]
    
    if (length(crash_cause_temp) > 0) {
      # create a string
      crash_cause_filter <- "("
      for (tidx in 1:length(crash_cause_temp)) {
        if (tidx != length(crash_cause_temp)) {
          crash_cause_filter <- paste0(crash_cause_filter, "'" , crash_cause_temp[tidx], "',")
        } else {
          # no comma after last entry
          crash_cause_filter <- paste0(crash_cause_filter, "'" , crash_cause_temp[tidx], "'")
        }
      }
      crash_cause_filter <- paste0(crash_cause_filter, ")")
    } else {
      crash_cause_filter <- NA
    }
    
    if (length(input$years) > 0) {
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
    } else {
      year_filter <- ""
    }
    
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
            "DISTINCT ON (id) id,",
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
            "(data->>'deaths')::integer as deaths,",
            "(data->>'seriously_injured')::integer as sv,",
            "(data->>'slightly_injured')::integer as lv,",
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
            "FROM objects LEFT JOIN geo ON objects.id = geo.accident_id",
            "WHERE parent_id = '/buckets/accidents/collections/accidents_raw'",
            "AND resource_name = 'record'",
            "AND SUBSTRING(data->>'accident_type', 1, 1) in ", type_filter,
            if_else(!is.na(type_detail_filter), paste0("AND data->>'accident_type' in ", type_detail_filter), ""),
            if_else(!is.na(crash_cause_filter), paste0("AND data->>'cause_2' in ", crash_cause_filter), ""),
            if_else(nchar(year_filter) > 0, paste0("AND date_part('year', date(data->>'date')) in ", year_filter), ""),
            "AND date_part('month', date(data->>'date')) in ", month_filter,
            "AND date_part('dow', date(data->>'date')) in ", weekdays_filter,
            "AND date_part('hour', TO_TIMESTAMP(data->>'time_of_day', 'HH24:MI:SS')::TIME) >= ", input$hour_filter[1],
            "AND date_part('hour', TO_TIMESTAMP(data->>'time_of_day', 'HH24:MI:SS')::TIME) < ", input$hour_filter[2],
            if_else(input$bike_helmet &
                      # helmet only coded from 2015 onwards
                      (is.element("2015", input$years) |
                         is.element("2016", input$years) |
                         is.element("2017", input$years) |
                         is.element("2018", input$years)),
                    "AND (data->>'helmet') like '%j%'", ""),
            if_else(input$single_participant, "AND (data->>'number_of_participants')::integer = '1'", ""),
            if_else(input$age_toggle,
              # filter age only if explicity asked for; data quality is rather bad
              paste0(
                # age participant1
                " AND ( ",
                "( (data->>'participants_age_01')::integer >= ", input$age_filter[1],
                " AND (data->>'participants_age_01')::integer <= ", input$age_filter[2], ")",
                # age participant 2, only coded from 2015 onwards
                if_else(
                  (is.element("2015", input$years) |
                     is.element("2016", input$years) |
                     is.element("2017", input$years) |
                     is.element("2018", input$years)),
                  paste0(" OR ",
                         "( (data->>'participants_age_02')::integer >= ", input$age_filter[1],
                         " AND (data->>'participants_age_02')::integer <= ", input$age_filter[2], ") )")
                  , ")") # end if_else 2015-2018
            ), ""), # end if_else age_toggle
            if_else(car_filter, " AND data->>'car' IS NOT NULL", ""),
            if_else(ped_filter," AND data->>'pedestrian' IS NOT NULL", ""),
            if_else(bike_filter, " AND data->>'bicycle' IS NOT NULL", ""),
            if_else(truck_filter, " AND data->>'lorry' IS NOT NULL", ""),
            if_else(bus_filter, " AND data->>'omnibus' IS NOT NULL", ""),
            if_else(rest_filter, paste0(" AND ( data->>'moped' IS NOT NULL",
                                       " OR data->>'motorcycle' IS NOT NULL ",
                                       " OR data->>'small_moped' IS NOT NULL",
                                       " OR data->>'other_road_user' IS NOT NULL)"), ""),
            # TODO: proper check for NaN's in the query!
            if_else(without_car_filter, " AND data->>'car' IS NULL", ""),
            if_else(without_ped_filter," AND data->>'pedestrian' IS NULL", ""),
            if_else(without_bike_filter, " AND data->>'bicycle' IS NULL", ""),
            if_else(without_truck_filter, " AND data->>'lorry' IS NULL", ""),
            if_else(without_bus_filter, " AND data->>'omnibus' IS NULL", ""),
            if_else(without_rest_filter, paste0(" AND (data->>'moped' IS NULL",
                                       " AND data->>'motorcycle' IS NULL",
                                       " AND data->>'small_moped' IS NULL",
                                       " AND data->>'other_road_user' IS NULL)"), ""),
            
            
            if_else((car_causer_filter |
                      ped_causer_filter |
                      bike_causer_filter |
                      truck_causer_filter |
                      bus_causer_filter), " AND (", ""),
            if_else(car_causer_filter, " (data->>'participants_01' in ('21', '22', '25'))", ""),
            if_else(car_causer_filter & ped_causer_filter,
                    " OR ", ""),
            if_else(ped_causer_filter," (data->>'participants_01' in ('81', '84', '93'))", ""),
            if_else((car_causer_filter |
                       ped_causer_filter) & bike_causer_filter,
                    " OR ", ""),
            if_else(bike_causer_filter, " (data->>'participants_01' in ('71', '72', '03'))", ""),
            if_else((car_causer_filter |
                       ped_causer_filter |
                       bike_causer_filter) & truck_causer_filter,
                    " OR ", ""),
            if_else(truck_causer_filter,
                    " (data->>'participants_01' in ('40', '42', '44', '46', '43', '48', '51', '52', '53', '54', '55', '57', '58'))", ""),
            if_else((car_causer_filter |
                       ped_causer_filter |
                       bike_causer_filter |
                       truck_causer_filter) & bus_causer_filter,
                    " OR ", ""),
            if_else(bus_causer_filter, " (data->>'participants_01' in ('31', '32', '33', '34', '35'))", ""),
            if_else((car_causer_filter |
                       ped_causer_filter |
                       bike_causer_filter |
                       truck_causer_filter |
                       bus_causer_filter), " )", ""),
            
            if_else(no_injuries_filter | slightly_filter | seriously_filter | dead_filter, "AND (", ""),
            if_else(no_injuries_filter, paste0(" (data->>'slightly_injured' IS NULL",
                              " AND data->>'seriously_injured' IS NULL",
                              " AND data->>'deaths' IS NULL)"), ""),
            if_else(no_injuries_filter & slightly_filter, " OR ", ""),
            if_else(slightly_filter, " data->>'slightly_injured' IS NOT NULL", ""),
            if_else((no_injuries_filter | slightly_filter) & seriously_filter, " OR ", ""),
            if_else(seriously_filter," data->>'seriously_injured' IS NOT NULL", ""),
            if_else((no_injuries_filter | slightly_filter | seriously_filter) & dead_filter, " OR ", ""),
            if_else(dead_filter," data->>'deaths' IS NOT NULL", ""),
            if_else(no_injuries_filter | slightly_filter | seriously_filter | dead_filter, ")", ""))
    
    print(sql_string)
    
    filtered <- dbGetQuery(db_con, sql_string)
    
    print(head(filtered))
    
    # reset color of refresh button
    global_vars$filter_changed <- FALSE
    
    return(filtered)
  })
  
  # observe all filters and toggle the global variable to color the refresh button
  observeEvent(c(input$vehicles, input$without_vehicles, input$causer, input$injured,
                 input$age_toggle, input$age_filter, input$bike_helmet, input$single_participant,
                 input$type, input$type_detail, input$crash_cause,
                 input$hour_filter, input$weekdays, input$months, input$years,
                 input$years_button, input$heatmap_toggle, input$markers_toggle,
                 input$heatmap_size, input$heatmap_intensity), {
    global_vars$filter_changed <- TRUE
  })
  
  output$refresh_button <- renderUI({
    if (global_vars$filter_changed) {
      actionButton(inputId = "update_button", "Aktualisieren", icon = icon("refresh"),
                     style = "color: white; background-color: #86D956;")
    } else {
      actionButton(inputId = "update_button", "Aktualisieren", icon = icon("refresh"),
                     style = "color: black; background-color: #FFFFFF")
    }
  })
  
  # this triggers an SQL query and the display of markers and / or the heatmap
  # via global_vars$update_visible_data (but only once for every filter change)
  observeEvent(input$update_button, ignoreNULL = FALSE, ignoreInit = TRUE, {
    if (global_vars$filter_changed) {
      global_vars$update_visible_data <- !global_vars$update_visible_data
    }
  })
  
  output$heatmap_size_slider <- renderUI({
    if (input$heatmap_toggle) {
      sliderInput(
         "heatmap_size",
         "Heatmap-Größe (in m)",
         min = 0, max = 1000,
         value = 300)
    }
  })
  
  output$heatmap_intensity_sider <- renderUI({
    if (input$heatmap_toggle) {
      sliderInput(
        "heatmap_intensity",
        "Heatmap-Intensität",
        min = 0, max = 1,
        value = 0.2)
    }
  })
  
  output$age_slider <- renderUI({
    if (input$age_toggle) {
      sliderInput(
        "age_filter",
        "Alter (ab 2010):",
        min = 1,
        max = 100,
        value = c(0, 100)
      )
    }
  })

  output$number_of_crashes <- renderText({
    crashes_filtered_without_location <-
      crashes_filtered()[!complete.cases(crashes_filtered()[, c("latitude", "longitude")]), ]
    
    # text output:
    if_else(nrow(crashes_filtered()) == 0,
                 "Es gibt keine Daten, die den aktuellen Filtereinstellungen entsprechen.",
                 paste0("Anzahl Unfälle: ", nrow(crashes_filtered()),
                        if_else(nrow(crashes_filtered_without_location) > 0,
                                paste0(", davon ", nrow(crashes_filtered_without_location),
                                       " noch ohne Geolokalisation (d.h. nicht auf Karte sichtbar; ",
                                       a(href = "https://crashes-editor.codeformuenster.org/",
                                       target = "_blank", "mithelfen und hinzufügen!"),
                                       "),"),
                                ""),
                        "<br>",
                        sum(crashes_filtered()$lv, na.rm = TRUE), " Leichtverletzte, ",
                        sum(crashes_filtered()$sv, na.rm = TRUE), " Schwerverletzte, ",
                        sum(crashes_filtered()$deaths, na.rm = TRUE), " Getötete")
            )
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
  
  # observe heatmap
  observeEvent(global_vars$update_visible_data, {
    crashes_filtered_with_location <-
      crashes_filtered()[complete.cases(crashes_filtered()[, c("latitude", "longitude")]), ]
    proxy <- leafletProxy("karte", data = crashes_filtered_with_location)
    if (length(crashes_filtered_with_location) > 0) {
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
  
  # marker observe
  observeEvent(global_vars$update_visible_data, {
    crashes_filtered_with_location <-
      crashes_filtered()[complete.cases(crashes_filtered()[, c("latitude", "longitude")]), ]
    proxy <- leafletProxy("karte", data = crashes_filtered_with_location)
    if (length(crashes_filtered_with_location) > 0) {
      if (input$markers_toggle) {
        proxy %>% 
          clearGroup("Markers") %>% 
          addMarkers(lng = ~longitude,
                     lat = ~latitude,
                     clusterOptions = markerClusterOptions(),
                     popup = paste0(names(weekdays_string_to_numbers[crashes_filtered_with_location$german_weekday]),
                           ", ", crashes_filtered_with_location$day_of_month,
                           ".", crashes_filtered_with_location$month,
                           ".", crashes_filtered_with_location$year,
                           ", ", crashes_filtered_with_location$hour,
                           ":", sprintf("%02d", crashes_filtered_with_location$minute), " Uhr",
                           "<br>",
                           crashes_filtered_with_location$street,
                           " ", crashes_filtered_with_location$street_additional,
                           "<br>",
                           if_else(!is.na(crashes_filtered_with_location$age1),
                                   paste0(" Alter 1: ", crashes_filtered_with_location$age1), ""),
                           if_else(!is.na(crashes_filtered_with_location$age2),
                                   paste0(" Alter 2: ", crashes_filtered_with_location$age2), ""),
                           if_else(!is.na(crashes_filtered_with_location$deaths),
                                   paste0("<br>", crashes_filtered_with_location$deaths, " Tote"), ""),
                           if_else(!is.na(crashes_filtered_with_location$sv),
                                   paste0("<br>", crashes_filtered_with_location$sv, " Schwerverletzte"), ""),
                           if_else(!is.na(crashes_filtered_with_location$lv),
                                   paste0("<br>", crashes_filtered_with_location$lv, " Leichtverletzte"), ""),
                           "<br>",
                           "<br>",
                           if_else(!is.na(crashes_filtered_with_location$car),
                                  paste0(crashes_filtered_with_location$car, " PKW<br>"), ""),
                           if_else(!is.na(crashes_filtered_with_location$lorry),
                                   paste0(crashes_filtered_with_location$lorry, " LKW<br>"), ""),
                           if_else(!is.na(crashes_filtered_with_location$omnibus),
                                   paste0(crashes_filtered_with_location$omnibus, " Bus(se)<br>"), ""),
                           if_else(!is.na(crashes_filtered_with_location$pedestrian),
                                   paste0(crashes_filtered_with_location$pedestrian, " Fußgänger<br>"), ""),
                           if_else(!is.na(crashes_filtered_with_location$bicycle),
                                   paste0(crashes_filtered_with_location$bicycle, " Fahrrad/Fahrräder<br>"), ""),
                           if_else(!is.na(crashes_filtered_with_location$moped), 
                                   paste0(crashes_filtered_with_location$moped, " Moped(s)<br>"), ""),
                           if_else(!is.na(crashes_filtered_with_location$motorcycle), 
                                   paste0(crashes_filtered_with_location$motorcycle, " Motorrad/Motorräder<br>"), ""),
                           if_else(!is.na(crashes_filtered_with_location$small_moped), 
                                   paste0(crashes_filtered_with_location$small_moped, " kleine(s) Moped(s)<br>"), ""),
                           if_else(!is.na(crashes_filtered_with_location$other_road_user), 
                                   paste0(crashes_filtered_with_location$other_road_user, " sonstige(s) Verkehrsmittel"), ""),
                           "<br>",
                           a(href = "https://recht.nrw.de/lmi/owa/br_show_anlage?p_id=15549",
                             target = "_blank", "Unfalltyp"),
                           ": ",
                           crashes_filtered_with_location$accident_type,
                           ", ",
                          a(href = "https://recht.nrw.de/lmi/owa/br_show_anlage?p_id=15541",
                             target = "_blank", "Unfallursache"),
                           ": ",
                           crashes_filtered_with_location$cause_2,
                           "<br>id: ", crashes_filtered_with_location$id,
                           "<br>",
                           "(Ort falsch? ",
                           "<a href='https://crashes-editor.codeformuenster.org/korrektur/",
                          crashes_filtered_with_location$id,
                          "' target = '_blank'>Hier korrigieren!</a>",
                           ")"),
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

  NullPlot <- ggplotly(
    ggplot(NULL) +
      annotate(
        geom = "text",
        x = 0,
        y = 0,
        label = "Keine Daten vorhanden. Bitte Auswahl ändern.",
        hjust = 0
      ) + theme_void()
  )

  observe({
    global_vars$crashes_static <- crashes_filtered()
  })

  aggregrated_data_hour <- reactive({
    
    crashes_hour <-
      global_vars$crashes_static %>%
      group_by(year, hour) %>%
      summarise(count_hour = n()) %>%
      mutate(year_character = paste0(year)) %>%
      ungroup()

    return(crashes_hour)
  })

  output$plotTime <- renderPlotly({

    if (is.null(aggregrated_data_hour()) || nrow(aggregrated_data_hour()) == 0) {
      p <- NullPlot
    } else {
      p <- plot_ly(data = aggregrated_data_hour(),
                   x = ~hour,
                   y = ~count_hour,
                   type = "bar",
                   color = ~year_character,
                   name = ~year_character,
                   hoverinfo = "text",
             text = ~paste0(year, ", ", hour, " Uhr, ", count_hour, " Unfälle")) %>%
            layout(xaxis = list(title = "Uhrzeit"),
                   yaxis = list(title = "Anzahl"),
                   legend = list(x = 0.1, y = 0.9),
                   showlegend = TRUE
                  ) %>%
            config(plot_ly(), displayModeBar = T, displaylogo = F, locale = 'de')
  	}
  })
  
  aggregrated_data_vehicle <- reactive({

    # if_else(car_causer_filter, " (data->>'participants_01' in ('21', '22', '25'))", ""),
    # if_else(ped_causer_filter," (data->>'participants_01' in ('81', '84', '93'))", ""),
    # if_else(bike_causer_filter, " (data->>'participants_01' in ('71', '72', '03'))", ""),
    # if_else(truck_causer_filter,
    # " (data->>'participants_01' in ('40', '42', '44', '46', '43', '48', '51', '52', '53', '54', '55', '57', '58'))", ""),
    # if_else(bus_causer_filter, " (data->>'participants_01' in ('31', '32', '33', '34', '35'))", ""),

    crashes_vehicle <-
      global_vars$crashes_static %>%
      mutate(causer_human =
               if_else(participants_01 %in% c(21, 22, 25), "PKW",
                      if_else(participants_01 %in% c(81, 84, 93), "Fuß",
                              if_else(participants_01 %in% c(71, 72, 03), "Fahrrad",
                                      if_else(participants_01 %in% c(40, 42, 44, 46, 43, 48, 51, 52, 53, 54, 55, 57, 58), "LKW",
                                              if_else(participants_01 %in% c(31, 32, 33, 34, 35), "Bus", "sonstige")))))) %>%

      mutate(victim_human =
               if_else(participants_02 %in% c(21, 22, 25), "PKW",
                      if_else(participants_02 %in% c(81, 84, 93), "Fuß",
                              if_else(participants_02 %in% c(71, 72, 03), "Fahrrad",
                                      if_else(participants_02 %in% c(40, 42, 44, 46, 43, 48, 51, 52, 53, 54, 55, 57, 58), "LKW",
                                              if_else(participants_02 %in% c(31, 32, 33, 34, 35), "Bus", "sonstige")))))) %>%
      group_by(causer_human, victim_human) %>%
      summarise(count_causer = n()) %>%
      ungroup()

    return(crashes_vehicle)
  })
  
  output$plotVehicle <- renderPlotly({
 	  
    if (is.null(aggregrated_data_vehicle()) || nrow(aggregrated_data_vehicle()) == 0) {
      p <- NullPlot
    } else {
      p <- plot_ly(data = aggregrated_data_vehicle(),
                   x = ~causer_human,
                   y = ~victim_human,
                   z = ~count_causer,
                   type = "heatmap",
                   hoverinfo = "text",
                   text = ~paste0(causer_human, " hat ", count_causer, " Unfälle mit ", victim_human, " verursacht.")) %>%
            add_annotations(x = ~causer_human,
                            y = ~victim_human,
                            text = ~paste0(count_causer),
                            showarrow = FALSE,
                            bgcolor = "lightgrey",
                            font = list(color = "black")) %>%
            layout(xaxis = list(title = "HauptverursacherIn (lt. Polizei)"),
                   yaxis = list(title = "GeschädigteR"),
                   legend = list(x = 0.1, y = 0.9),
                   showlegend = TRUE) %>%
            colorbar(title = "Anzahl Unfälle") %>%
            config(plot_ly(), displayModeBar = T, displaylogo = F, locale = 'de')
    }
  })
  
  output$crashes_table <- DT::renderDataTable({
    DT::datatable(crashes_filtered())
  })
  
}


