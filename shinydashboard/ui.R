# dashboard header ----
header <- dashboardHeader(
  
  # title ----
  title = "MarineBioMaps",
  titleWidth = 400
  
)

# dashboard sidebar ----
sidebar <- dashboardSidebar(
  
  # sidebarMenu ----
  sidebarMenu(
    
    menuItem(text = "Welcome", tabName = "welcome", icon = icon("star")),
    menuItem(text = "Interactive Maps", icon = icon("water"),
             menuSubItem("Statewide MPA Overview", tabName = "statewide"),
             menuSubItem("North Coast", tabName = "ncsr"),
             menuSubItem("North Central Coast", tabName = "nccsr"),
             menuSubItem("San Franciso Bay", tabName = "sfbsr"),
             menuSubItem("Central Coast", tabName = "ccsr"),
             menuSubItem("South Coast", tabName = "scsr")
             ) # End menuItem
    
  ) # END sidebarMenu
  
) # END dashboardSidebar

# dashboard body ----
body <- dashboardBody(
  
  # set theme
  use_theme("dashboard-fresh-theme.css"),
  
  # add font styling
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom-font.css")
  ),
  
  # tabItems ----
  tabItems(
    
    # welcome tabItem ----
    tabItem(tabName = "welcome",
            
            # left-hand column ----
            column(width = 9,
                   
                   # background info box ----
                   box(width = NULL,
                       
                       title = tagList(icon("water"),
                                       strong("Evaluating the protection of diverse\nand representative coastal and marine habitats within California’s Marine Protected Area (MPA) network")),
                       includeMarkdown("text/intro.md"),
                       tags$img(src = "bigkelplei.jpg",
                                alt = "An overhead aerial image of kelp taken near Santa Cruz Island by photographer Leilanie Rubinstein",
                                style = "max-width: 100%;"),
                       tags$h6("Images Source:", 
                               tags$a(href = "https://leirubinstein.github.io/nature-photography.html", 
                                      "Leilanie Rubinstein"),
                               style = "text-align: center;")
                       
                   ) # END backgroudn info box 
                   
            ), # END left-hand column
            
            # right-hand column ----
            column(width = 3,
                   
                   # first fluidRow ----
                   fluidRow(
                     
                     # citation box ----
                     box(width = NULL,
                         
                         title = tagList(icon("table"),
                                         strong("Data Source")),
                         includeMarkdown("text/citation.md")
                         
                     ) # END citation box
                     
                   ), # END first fluidRow 
                   
                   # second fluidRow ----
                   fluidRow(
                     
                     # disclaimer box ----
                     box(width = NULL,
                         
                         title = tagList(icon("triangle-exclamation"),
                                         tags$strong("Disclaimer")),
                         includeMarkdown("text/disclaimer.md")
                         
                     ) # END disclaimer box 
                     
                   ), # END second fluidRow
                   
                   fluidRow(
                     box(width = NULL,
                         title = tagList(icon("file-pdf"), strong("See the complete analysis:")),
                         div(style = "text-align: center; padding-bottom: 20px;",  # Adds bottom padding
                             tags$a("Download Full MBM Report", href = "mbm_report.pdf", target = "_blank", 
                                    style = "color: white; background-color: #C4A9C2; padding: 10px 20px; border-radius: 5px; text-decoration: none;")
                         )
                     )
                     
                   ) # End third fluid row (pdf)
                   
            ) # END right-hand column
            
    ), # END welcome tabItem
    
    # Statewide tabItem ----
    tabItem(tabName = "statewide",
            
            # fluidRow ----
            fluidRow(
              
              # leaflet box ----
              box(width = 8,
                  
                  title = tags$strong("California Marine Protected Areas"),
                  
                  # leaflet output ----
                  leafletOutput(outputId = "statewide_map_output") |> 
                    withSpinner(type = 1, color = "darkgoldenrod")
                  
              ) # END leaflet box
              
            ) # END fluidRow
            
    ), # END statewide tabItem
    
    # NCSR tabItem ----
    tabItem(tabName = "ncsr",
            
            fluidRow(
              # input box ----
              box(width = 4,
                  title = tags$strong("Select Habitat Component of Interest:"),
                  
                  pickerInput(inputId = "ncsr_mapchoice_input",  # Updated ID
                              label = "Select one variable from the list",
                              choices = c("MPA Boundaries",
                                          "Biota",
                                          "Substrate",
                                          "Depth Zone",
                                          "Estuary"),
                              selected = "MPA Boundaries",  # Default selection
                              multiple = FALSE,
                              options = pickerOptions(actionsBox = TRUE)),
                  
                  
                  
              ), # END input box
              
              # Leaflet box ----
              box(width = 8,
                  
                  title = tags$strong("North Coast Study Region"),
                  
                  # Leaflet output for ncsr ----
                  leafletOutput(outputId = "ncsr_map_output") |> 
                    withSpinner(type = 1, color = "darkgoldenrod")
                  
                  ) # END leaflet box
              
            ) # End ncsr fluidrow
            
        ), # End ncsr tabItem
    
    
    
    
    
    
    # NCCSR tabItem ----
    tabItem(tabName = "nccsr",
            
            fluidRow(
              
              # input box ----
              box(width = 4,
                  title = tags$strong("Select Habitat Component of Interest:"),
                  
                  pickerInput(inputId = "nccsr_mapchoice_input",  
                              label = "Select one variable from the list",
                              choices = c("MPA Boundaries",
                                          "Biota",
                                          "Substrate",
                                          "Depth Zone",
                                          "Estuary"),
                              selected = "MPA Boundaries",  # Default selection
                              multiple = FALSE,
                              options = pickerOptions(actionsBox = TRUE)),
                  
              ), # END input box
              
              # Leaflet box ----
              box(width = 8,
                  
                  title = tags$strong("North Central Coast Study Region"),
                  
                  # Leaflet output for nccsr ----
                  leafletOutput(outputId = "nccsr_map_output") |> 
                    withSpinner(type = 1, color = "darkgoldenrod")
                  
              ) # END leaflet box
              
            ) # End nccsr fluidrow
            
    ), # End nccsr tabItem
    
    
    
    
    
    
    
    # SFBSR tabItem ----
    tabItem(tabName = "sfbsr",
            
            fluidRow(
              
              # Leaflet box ----
              box(width = 8,
                  
                  title = tags$strong("San Francisco Bay Study Region"),
                  
                  # Leaflet output for sfbsr ----
                  leafletOutput(outputId = "sfbsr_map_output") |> 
                    withSpinner(type = 1, color = "darkgoldenrod")
                  
              ) # END leaflet box
              
            ) # End sfbsr fluidrow
            
    ), # End sfbsr tabItem
    
    # CCSR tabItem ----
    tabItem(tabName = "ccsr",
            
            fluidRow(
              
              # Leaflet box ----
              box(width = 8,
                  
                  title = tags$strong("Central Coast Study Region"),
                  
                  # Leaflet output for ccsr ----
                  leafletOutput(outputId = "ccsr_map_output") |> 
                    withSpinner(type = 1, color = "darkgoldenrod")
                  
              ) # END leaflet box
              
            ) # End ccsr fluidrow
            
    ), # End ccsr tabItem
    
    # SCSR tabItem ----
    tabItem(tabName = "scsr",
            
            fluidRow(
              
              # Leaflet box ----
              box(width = 8,
                  
                  title = tags$strong("South Coast Study Region"),
                  
                  # Leaflet output for scsr ----
                  leafletOutput(outputId = "scsr_map_output") |> 
                    withSpinner(type = 1, color = "darkgoldenrod")
                  
              ) # END leaflet box
              
            ) # End scsr fluidrow
            
    ) # End scsr tabItem
    
  ) # END tabItems 
  
) # END dashboardBody

# combine all into dashboardPage ----
dashboardPage(header, sidebar, body)