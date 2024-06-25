
ui <- fluidPage(
  
  useShinyjs(),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
  ),
  
  titlePanel("Dane County Urban Heat Island Exploration"),
  h4("Observe temperature patterns by site or land cover."),
  leafletOutput("map", height = 350) #%>%
    # withSpinner(type = 3,
    #             color.background = "white"), 
  ,
  hr(),
  wellPanel(
    fluidRow(
      column(6,
             selectInput(inputId = "filterSpace", label = "Filter data spatially by:", choices = c(" ", "Site", "Land cover"), selected = " "),
             uiOutput("siteSelectUI"),
             uiOutput("landCoverSelectUI"),
             br(),
             uiOutput("addSelectionUI")),
      column(6,
             uiOutput("filterTimeUI"))
      )),
  br(),
  br(),
  uiOutput("plotUI")
)
