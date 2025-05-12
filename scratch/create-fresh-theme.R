library(fresh)

create_theme(
  
  adminlte_color(
    light_blue = "lightseagreen"
  ),
  
  adminlte_global(
    content_bg = "gainsboro"
  ),
  
  adminlte_sidebar(
    dark_bg = "gray",
    dark_hover_bg = "darkgoldenrod",
    dark_color = "darkslategray",
    width = "400px"
  ),
  
  
  output_file = here::here("shinydashboard","www","dashboard-fresh-theme.css")
  
)
