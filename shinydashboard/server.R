server <- function(input, output) {
  
  
  # Factorize study regions
  unique_regions <- c("NCSR", "NCCSR", "SFBSR", "CCSR", "SCSR")
  mpas$study_regi <- factor(mpas$study_regi, levels = unique_regions)
  
  # Define custom color palette
  custom_colors <- c("#C4A9C2", "#71D9B0", "#D8C397", "#6EA1C8", "#796655")
  pal <- colorFactor(palette = custom_colors, domain = unique_regions)
  
  # Filter for regional mpas
  ncsr_mpas <- mpas |> 
    filter(study_regi == "NCSR")
  
  nccsr_mpas <- mpas |> 
    filter(study_regi == "NCCSR")
  
  sfbsr_mpas <- mpas |> 
    filter(study_regi == "SFBSR")
  
  ccsr_mpas <- mpas |> 
    filter(study_regi == "CCSR") 
  
  scsr_mpas <- mpas |> 
    filter(study_regi == "SCSR")
  
  # build statewide leaflet map
  output$statewide_map_output <- renderLeaflet({
    
    # leaflet map
    leaflet() |> 
      addProviderTiles(providers$CartoDB.Positron) |> 
      setView(lng = -119.6989, lat = 34.4203, zoom = 6) |> 
      addMiniMap(toggleDisplay = TRUE, minimized = FALSE) |> 
      addPolygons(data = sr_boundaries,
                  color = ~pal(study_regi), # Assign colors based on study_region
                  weight = 3,
                  popup = paste0("Study Region Name:  ", sr_boundaries$name, "<br>",
                                 "Study Region Abbreviation:  ", sr_boundaries$study_regi, "<br>")) |> 
      addPolygons(data = mpas,
                  color = "darkgoldenrod", # Assign colors based on study_region
                  weight = 3,
                  popup = paste0("MPA Name:  ", mpas$shortname, "<br>",
                                 "MPA Type:  ", mpas$type, "<br>",
                                 "Study Region:  ", mpas$study_regi)) |>  
      addLegend(position = "topright",
                pal = pal,
                values = mpas$study_regi,
                title = "MPA Study Region",
      ) 
    
      
    })
  
  
        # Build ncsr leaflet map
    output$ncsr_map_output <- renderLeaflet({
      leaflet() |> 
        addProviderTiles(providers$CartoDB.Positron) |> 
        setView(lng = -124.3917, lat = 40.4667, zoom = 8) |> 
        addMiniMap(toggleDisplay = TRUE, minimized = FALSE) |> 
        addPolygons(data = ncsr_mpas,
                    color = "darkgoldenrod", 
                    weight = 2,
                    fillOpacity = 0,     # No fill
                    
                    popup = paste0("MPA Name:  ", ncsr_mpas$shortname, "<br>",
                                   "MPA Type:  ", ncsr_mpas$type, "<br>",
                                   "Study Region:  ", ncsr_mpas$study_regi))
      
  })
  
    
    # Build nccsr leaflet map
    output$nccsr_map_output <- renderLeaflet({
      leaflet() |> 
        addProviderTiles(providers$CartoDB.Positron) |> 
        setView(lng = -123.14, lat = 38.51, zoom = 8) |> 
        addMiniMap(toggleDisplay = TRUE, minimized = FALSE) |> 
        addPolygons(data = nccsr_mpas,
                    color = "darkgoldenrod", 
                    weight = 2,
                    fillOpacity = 0,     # No fill
                    
                    popup = paste0("MPA Name:  ", nccsr_mpas$shortname, "<br>",
                                   "MPA Type:  ", nccsr_mpas$type, "<br>",
                                   "Study Region:  ", nccsr_mpas$study_regi))
      
    }) 
    
    
    # Build sfbsr leaflet map
    output$sfbsr_map_output <- renderLeaflet({
      leaflet() |> 
        addProviderTiles(providers$CartoDB.Positron) |> 
        setView(lng = -122.1158, lat = 37.6965, zoom = 9) |> 
        addMiniMap(toggleDisplay = TRUE, minimized = FALSE) |> 
        addPolygons(data = sfbsr_mpas,
                    color = "darkgoldenrod", 
                    weight = 2,
                    fillOpacity = 0,     # No fill
                    
                    popup = paste0("MPA Name:  ", sfbsr_mpas$shortname, "<br>",
                                   "MPA Type:  ", sfbsr_mpas$type, "<br>",
                                   "Study Region:  ", sfbsr_mpas$study_regi))
      
    })
    
    
    # Build ccsr leaflet map
    output$ccsr_map_output <- renderLeaflet({
      leaflet() |> 
        addProviderTiles(providers$CartoDB.Positron) |> 
        setView(lng = -121.9183, lat = 36.5253, zoom = 8) |> 
        addMiniMap(toggleDisplay = TRUE, minimized = FALSE) |> 
        addPolygons(data = ccsr_mpas,
                    color = "darkgoldenrod", 
                    weight = 2,
                    fillOpacity = 0,     # No fill
                    
                    popup = paste0("MPA Name:  ", ccsr_mpas$shortname, "<br>",
                                   "MPA Type:  ", ccsr_mpas$type, "<br>",
                                   "Study Region:  ", ccsr_mpas$study_regi))
      
    })
    
    # Build scsr leaflet map
    output$scsr_map_output <- renderLeaflet({
      leaflet() |> 
        addProviderTiles(providers$CartoDB.Positron) |> 
        setView(lng = -119.6989, lat = 34.4203, zoom = 8) |> 
        addMiniMap(toggleDisplay = TRUE, minimized = FALSE) |> 
        addPolygons(data = scsr_mpas,
                    color = "darkgoldenrod", 
                    weight = 2,
                    fillOpacity = 0,     # No fill
                    
                    popup = paste0("MPA Name:  ", scsr_mpas$shortname, "<br>",
                                   "MPA Type:  ", scsr_mpas$type, "<br>",
                                   "Study Region:  ", scsr_mpas$study_regi))
      
    })
    
}