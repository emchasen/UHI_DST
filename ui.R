
ui <- fluidPage(
  
  useShinyjs(),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  ),
  
  titlePanel("Dane County Urban Heat Island Exploration"),
  h4("Observe temperature patterns by site or land cover."),
  wellPanel(
    fluidRow(
      column(4,
             fluidRow(
               column(5,
                      selectInput(inputId = "filterSpace", label = "Filter data spatially by:", choices = c(" ", "Site", "Land cover"), selected = " "),
                      uiOutput("siteSelectUI"),
                      uiOutput("landCoverSelectUI")),
               column(7,
                      uiOutput("filterTimeUI")))),
      column(6, 
             leafletOutput("map", height = 350) %>%
               withSpinner(type = 3,
                           color.background = "white")),
      column(2, 
             # radioButtons("layers", "Map layers:",
             #               choices = c("Land cover" = "landCover",
             #                           "Jan 2023 day" = "janDay",
             #                           "Jan 2023 night" = "janNight",
             #                           "Jul 2023 day" = "julDay",
             #                           "Jul 2023 night" = "julNight",
             #                           "None" = "none"),
             #              selected = "none"),
             selectInput("layers", "Map layers", choices = layerChoices, selected = "None"),
             helpText("Month and daytime layers refer to monthly average temperatures (°F) from 2023."))
      )
  ),
  hr(),
  #uiOutput("dateRangeText"),
  tags$head(
    tags$style(
      HTML(".shiny-notification {
           height: 100px;
           width: 800px;
           position:fixed;
           top: calc(50% - 50px);
           left: calc(50% - 400px);
           font-size: 250%;
           text-align: center;
           }
           "
      )
    )
  ),
  #fluidRow(
   # column(8,
           uiOutput("plotUI")#),
    #column(4,
     #      uiOutput("addSelectionUI"))
  #)
)
