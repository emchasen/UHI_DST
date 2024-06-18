
ui <- fluidPage(
  
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
  ),
  
  titlePanel("Dane County Urban Heat Island Exploration"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Specify data to observe temperature patterns."),
      selectInput(inputId = "filterSpace", label = "Filter data spatially by:", choices = c(" ", "Site", "Land cover"), selected = " "),
      uiOutput("siteSelectUI"),
      uiOutput("landCoverSelectUI"),
      br(),
      uiOutput("filterTimeUI")
      #uiOutput("timeSelectUI")
    ),
    
    mainPanel(
      
      leafletOutput("map", height = 350) %>%
        withSpinner(type = 3,
                    color.background = "white"), 
      uiOutput("plotUI")
      
    )
  )
)
