
ui <- fluidPage(
  
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
  ),
  
  titlePanel("Dane County Urban Heat Island Exploration"),
  
  sidebarLayout(
    sidebarPanel(
      h5("Select a site on the map if you want to observe data at a specific location, or use the dropdown below if you want to observe data by site characteristics."),
      selectInput(inputId = "filterSpace", label = "Filter data spatially by:", choices = c(" ", "Site", "Land cover"), selected = " "),
      uiOutput("siteSelectUI"),
      selectInput(inputId = "filterTime", label = "Filter data in time by:", choices = c(" ", "Year", "Month", "Day")),
      uiOutput("timeSelectUI")
    ),
    
    mainPanel(
      
      leafletOutput("map", height = 350) %>%
        withSpinner(type = 3,
                    color.background = "white"), 
      uiOutput("plotUI")
      
    )
  )
)
