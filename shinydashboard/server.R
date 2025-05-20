server <- function(input, output) {
  
  
  # Factorize study regions
  unique_regions <- c("NCSR", "NCCSR", "SFBSR", "CCSR", "SCSR")
  mpas$study_regi <- factor(mpas$study_regi, levels = unique_regions)
  
  # Define custom color palette
  custom_colors <- c("#C4A9C2", "#71D9B0", "#D8C397", "#6EA1C8", "#796655")
  pal <- colorFactor(palette = custom_colors, domain = unique_regions)
  
  pal_type <- colorFactor(palette = "Set3", domain = mpas$type)
  
  
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
      setView(lng = -119.6989, lat = 34.4203, zoom = 7) |> 
      addMiniMap(toggleDisplay = TRUE, minimized = FALSE) |> 
      addPolygons(data = sr_boundaries,
                  color = ~pal(study_regi), # Assign colors based on study_region
                  fillOpacity =  0,
                  weight = 1,
                  popup = paste0("Study Region Name:  ", sr_boundaries$name, "<br>",
                                 "Study Region Abbreviation:  ", sr_boundaries$study_regi, "<br>")) |> 
      addPolygons(data = mpas,
                  fillColor = ~pal_type(type),
                  fillOpacity =  0.7,
                  color = "black", # Assign colors based on study_region
                  weight = 1,
                  popup = paste0("MPA Name:  ", mpas$shortname, "<br>",
                                 "MPA Type:  ", mpas$type, "<br>",
                                 "Study Region:  ", mpas$study_regi)) |>  

      addLegend(position = "topright",
                pal = pal_type,
                values = mpas$type,
                title = "MPA Type")
    
      
    })
  
  
  
  ## BEGIN REGIONAL MAPS
  
  
  
  
  
  ## NCSR
  
  
  # Reactive expression to determine which dataset to use
  selected_map_data <- reactive({
    req(input$ncsr_mapchoice_input)  # Ensure input is not NULL
    
    switch(input$ncsr_mapchoice_input,
           "MPA Boundaries" = ncsr_mpas,  # Default 
           "Substrate" = ncsr_substrate  

    )
  })
  
  # Palettes
  selected_palette <- reactive({
    req(input$ncsr_mapchoice_input)
    
    if (input$ncsr_mapchoice_input == "Substrate") {
      colorFactor(palette = c("#204035FF", "#4A7169FF", "#849383", "#BEB59CFF", "#998467", "#735231FF", "#49271BFF"), 
                  domain = ncsr_substrate$cmecs_sc_category)
    } else if (input$ncsr_mapchoice_input == "MPA Boundaries") {
      colorFactor(palette = "Set3", 
                  domain = ncsr_mpas$type)
    } 
  })
  
  # Pop-ups
  selected_map_data2 <- reactive({
    df <- selected_map_data()
    if (input$ncsr_mapchoice_input == "Substrate") {
      df$popup_content <- paste0(
        "<strong>MPA Name:</strong> ",      df$shortname,          "<br/>",
        "<strong>Type:</strong> ",          df$type,               "<br/>",
        "<strong>Study Region:</strong> ",  df$study_regi,         "<br/>",
        "<strong>Component:</strong> ",     df$cmecs_sc_category
      )
    } else {
      df$popup_content <- paste0(
        "<strong>MPA Name:</strong> ",      df$shortname,          "<br/>",
        "<strong>Type:</strong> ",          df$type,               "<br/>",
        "<strong>Study Region:</strong> ",  df$study_regi
      )
    }
    df
  })
  
  
  # Create reactive NCSR map
  output$ncsr_map_output <- renderLeaflet({
    df     <- selected_map_data2()
    pal    <- selected_palette()
    choice <- input$ncsr_mapchoice_input
    
    leaflet(df) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = -124.3917, lat = 40.4667, zoom = 7) %>%
      addMiniMap(toggleDisplay = TRUE, minimized = FALSE) %>%
      addPolygons(
        fillColor   = ~pal(get(ifelse(choice=="Substrate","cmecs_sc_category","type"), df)),
        fillOpacity = 0.7,
        color       = "black",
        weight      = 0.5,
        popup       = ~popup_content
      ) %>%
      addLegend(
        position = "topright",
        pal      = pal,
        values   = if (choice=="Substrate") df$cmecs_sc_category else df$type,
        title    = paste(choice, "Within MPAs")
      )
  })
  
  
  
  
  
  
  
  
  
  # NCCSR 
  
    
  # Reactive expression to determine which dataset to use
  selected_map_data_nccsr <- reactive({
    req(input$nccsr_mapchoice_input)  # Ensure input is not NULL
    
    switch(input$nccsr_mapchoice_input,
           "MPA Boundaries" = nccsr_mpas,  # Default 
           "Substrate" = nccsr_substrate  
           
    )
  })
  
  # Palettes
  selected_palette_nccsr <- reactive({
    req(input$nccsr_mapchoice_input)
    
    if (input$nccsr_mapchoice_input == "Substrate") {
      colorFactor(palette = c("#204035FF", "#4A7169FF", "#849383", "#BEB59CFF", "#998467", "#735231FF", "#49271BFF"), 
                  domain = nccsr_substrate$cmecs_sc_category)
    } else if (input$nccsr_mapchoice_input == "MPA Boundaries") {
      colorFactor(palette = "Set3", 
                  domain = nccsr_mpas$type)
    } 
  })
  
  # Pop-ups
  selected_map_data2_nccsr <- reactive({
    df_nccsr <- selected_map_data_nccsr()
    if (input$nccsr_mapchoice_input == "Substrate") {
      df_nccsr$popup_content <- paste0(
        "<strong>MPA Name:</strong> ",      df_nccsr$shortname,          "<br/>",
        "<strong>Type:</strong> ",          df_nccsr$type,               "<br/>",
        "<strong>Study Region:</strong> ",  df_nccsr$study_regi,         "<br/>",
        "<strong>Component:</strong> ",     df_nccsr$cmecs_sc_category
      )
    } else {
      df_nccsr$popup_content <- paste0(
        "<strong>MPA Name:</strong> ",      df_nccsr$shortname,          "<br/>",
        "<strong>Type:</strong> ",          df_nccsr$type,               "<br/>",
        "<strong>Study Region:</strong> ",  df_nccsr$study_regi
      )
    }
    df_nccsr
  })
  
  
  # Create reactive NCCSR map
  output$ncsr_map_output <- renderLeaflet({
    df_nccsr     <- selected_map_data2_nccsr()
    pal_nccsr    <- selected_palette_nccsr()
    choice_nccsr <- input$nccsr_mapchoice_input
    
    leaflet(df_nccsr) |> 
      addProviderTiles(providers$CartoDB.Positron) |> 
      setView(lng = -123.04444, lat = 38.83528, zoom = 7) |> 
      addMiniMap(toggleDisplay = TRUE, minimized = FALSE) |> 
      addPolygons(
        fillColor   = ~pal_nccsr(get(ifelse(choice=="Substrate","cmecs_sc_category","type"), df_nccsr)),
        fillOpacity = 0.7,
        color       = "black",
        weight      = 0.5,
        popup       = ~popup_content
      ) %>%
      addLegend(
        position = "topright",
        pal      = pal_nccsr,
        values   = if (choice=="Substrate") df_nccsr$cmecs_sc_category else df_nccsr$type,
        title    = paste(choice, "Within MPAs")
      )
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