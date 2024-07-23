
ui <- fluidPage(
  
  useShinyjs(),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
  ),
  
  titlePanel("Dane County Urban Heat Island Exploration"),
  h4("Observe temperature patterns by site or land cover."),
  wellPanel(
    fluidRow(
      column(6,
             selectInput(inputId = "filterSpace", label = "Filter data spatially by:", choices = c(" ", "Site", "Land cover"), selected = " "),
             uiOutput("siteSelectUI"),
             uiOutput("landCoverSelectUI")),
      column(6,
             uiOutput("filterTimeUI"))
    )),
  fluidRow(
    column(8,
           leafletOutput("map", height = 350) %>%
             withSpinner(type = 3,
                         color.background = "white")),
    column(4, 
           uiOutput("addSelectionUI"))
  ),
  hr(),
  #fluidRow(
   # column(3,
           #radioButtons("tempLabel", "Degree units display", choices = c("Celsius", "Fahrenheit"), selected = "Celsius", inline = TRUE),
    #column(9,
     #      )
  #),
  uiOutput("plotUI")
)
