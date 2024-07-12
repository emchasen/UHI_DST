
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
             uiOutput("landCoverSelectUI"),
             br(),
             uiOutput("addSelectionUI")),
      column(6,
             uiOutput("filterTimeUI"))
    )),
  leafletOutput("map", height = 350) %>%
    withSpinner(type = 3,
                color.background = "white"),
  hr(),
  radioButtons("tempLabel", "Degree units display", choices = c("Celsius", "Fahrenheit"), selected = "Celsius", inline = TRUE),
  uiOutput("plotUI")
)
