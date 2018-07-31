library(shiny)

# Define server logic required to draw the map
server <- function(input, output, session) {
  
  # LEAFLET -----------------------------------------------------------------
  
	# get data from https://gitlab.com/cedrix-opendata/unfalldaten-muenster/geoLocator/blob/master/geolocator.result.csv
	# and manually execute the following commands
	# TODO to-be-automized
  # unfaelle = read.csv("ms_unfaelle.csv")
  # locations = read.csv("geolocator.result.csv", col.names = c("id", "latitude", "longitude"), header = FALSE)
  # accidents = merge(x = unfaelle, y = locations, by = "id", all.y = TRUE)
  # accidents = accidents[1:20000,]
  
  output$karte <- renderLeaflet({
    pal11 <- colorNumeric(palette = "PuRd",
                          accidents$anzahl_beteiligte)
    # pal12 <- colorNumeric(palette = "PuBuGn",
                          # ks4_sp_ll()@data$pears) 
    # pal13 <- colorNumeric(palette = "YlOrRd",
    #                       ks4_sp_ll()@data$total_area)
    
    m11 <- leaflet(data = accidents) %>%
    	setView(lat = 51.96, lng = 7.62, zoom = 12) %>% 
      addProviderTiles(provider = "Esri.WorldImagery", group = "Terrain") %>%
      addProviderTiles(provider = "OpenStreetMap.BlackAndWhite", group = "OSM (B & W)") %>%
      # addProviderTiles("Stamen.TonerLite", group = "Toner Lite") %>%
      # Apples
      # addCircles(lng = accidents$longitude,
      #            lat = accidents$latitude,
      #            color = "black",
      #            opacity = 0.8,
      #            weight = 0.5,
      #            radius = 200,  #  Radius could be assigned to a another variable
      #            fillOpacity = 0.5,
      #            popup = NULL,
      # 					 group = "Apples") %>%
      # # Pears
      # addCircles(lng = accidents$longitude,
      #            lat = accidents$latitude,
      #            color = "black",
      #            opacity = 1, radius = 200, weight = 1,
      #            fillOpacity = 0.3,
      #            popup = NULL, group = "Pears") %>%
    		addWebGLHeatmap(lng = ~longitude, lat = ~latitude, size = 250
    										, units = "m") %>%
      ### Groups
      # hideGroup("Pears") %>%
      # showGroup("Apples") %>%
      hideGroup("Terrain") %>%
      showGroup("OSM (B & W)") %>%
      # Layers control
      addLayersControl(
        baseGroups = c("Terrain", "OSM (B & W)"),
        overlayGroups = c("Apples",
                          "Pears"
                          ),
        options = layersControlOptions(collapsed = FALSE)
      )
    
    m11
  })
  
}


