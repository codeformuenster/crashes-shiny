library(shiny)

# Define server logic required to draw the map
server <- function(input, output, session) {
  
	accidents_filtered <- reactive({
	  
	  filtered <- accidents
		
	  # involved vehicles
	  if ("ped" %in% input$vehicles) {
  	  filtered <- filtered %>% 
  	    filter(fg >= 1)
	  }
	  if ("bike" %in% input$vehicles) {
  	  filtered <- filtered %>% 
  	    filter(rf >= 1)
	  }
	  if ("car" %in% input$vehicles) {
  	  filtered <- filtered %>% 
  	    filter(pkw >= 1)
	  }
	  if ("truck" %in% input$vehicles) {
  	  filtered <- filtered %>% 
  	    filter(lkw >= 1)
	  }
	  if ("rest" %in% input$vehicles) {
  	  filtered <- filtered %>% 
  	    filter(mofa >= 1 | kkr >= 1 | krad >= 1 | kom >= 1 | sonstige >= 1)
	  }
	  
	  
	  # hit and run?
	  if (input$hit_and_run == "yes") {
			filtered <- filtered %>% 
				filter(flucht == TRUE)
		} else if (input$hit_and_run == "no") {
			filtered <- filtered %>% 
			  filter(flucht == FALSE)
		}
	  
		return(filtered)
	})
	
  # LEAFLET -----------------------------------------------------------------
	
  output$karte <- renderLeaflet({
    pal11 <- colorNumeric(palette = "PuRd",
                          accidents_filtered()$anzahl_beteiligte)
    # pal12 <- colorNumeric(palette = "PuBuGn",
                          # ks4_sp_ll()@data$pears) 
    # pal13 <- colorNumeric(palette = "YlOrRd",
    #                       ks4_sp_ll()@data$total_area)
    
    print(nrow(accidents_filtered()))
    
    map <- leaflet(data = accidents_filtered()) %>%
    	setView(lat = 51.96, lng = 7.62, zoom = 12) %>% 
      addProviderTiles(provider = "Esri.WorldImagery", group = "Terrain") %>%
      addProviderTiles(provider = "OpenStreetMap.BlackAndWhite", group = "OSM (B & W)") %>%
      # addProviderTiles("Stamen.TonerLite", group = "Toner Lite") %>%
      addCircles(lng = ~longitude,
                 lat = ~latitude,
                 color = "black",
                 opacity = 0.8,
                 weight = 0.5,
                 radius = 200,  #  Radius could be assigned to a another variable
                 fillOpacity = 0.5,
                 popup = paste(accidents_filtered()$tag, 
                               ",", accidents_filtered()$datum,
                               ",", accidents_filtered()$uhrzeit,
                               ", id: ",  accidents_filtered()$id,
                               ",\nTote: ",  accidents_filtered()$t,
                               ", Schwerverletzte: ",  accidents_filtered()$sv,
                               ", Leichtverletzte: ",  accidents_filtered()$lv),
      					 group = "Markers") %>%
    		addWebGLHeatmap(lng = ~longitude,
    										lat = ~latitude,
    										size = 500, units = "m",
    										group = "Heatmap") %>%
      ### Groups
      # hideGroup("Markers") %>%
      hideGroup("Terrain") %>%
      showGroup("OSM (B & W)") %>%
      # Layers control
      addLayersControl(
        baseGroups = c("Terrain", "OSM (B & W)"),
        overlayGroups = c("Heatmap",
                          "Markers"
                          ),
        options = layersControlOptions(collapsed = FALSE)
      )
    
    map
  })
  
   output$accidents_table <- DT::renderDataTable({
    DT::datatable(accidents, options = list(orderClasses = TRUE))
  })
  
}


