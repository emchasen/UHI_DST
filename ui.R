
ui <- fluidPage(
  
  useShinyjs(),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  ),
  
  titlePanel("Dane County Urban Heat Island Exploration"),
  h4("Observe temperature patterns by site or land cover."),
  tabsetPanel(
    tabPanel("Explore data",
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
                        selectInput("layers", "Map layers*", choices = layerChoices, selected = "None"),
                        helpText("*Month. day/night layers display monthly average temperatures (°F) from 2023.")
                 )
               )
             ),
             hr(),
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
             ),
    tabPanel("Instructions",
             p("To observe temperature data for Dane County, first decide if you want to observe data from a specific sensor ('site'),
             or group of sensors by land cover type. If you click on 'site', you can choose your location by clicking directly on the map."),
             p("Next, decide if you want to observe temperature patterns over a year, month, day or date range. Note that the granularity of the
               data differs depending on the time range that you are viewing."),
             p("After selecting your time filter and choosing the time, the data is displayed visually in a chart.
               You can change the degree units and the chart will update. If you hover the mouse cursor over the chart, data will 
               be displayed in text format as the cursor moves. Additional features include the ability to zoom in and out of the chart,
               and to reset the original chart axes. If multiple legend items are visible, you can also toggle individual legend items on and
               off, which will hide or display data."),
             p("Comparison data allows the user to select another site or landcover to compare to the original data set. Note that once you have 
               selected an additional data set, a user can change the selected time but if the user changes the 'Filter data in time', the second data set will need to
               be reselected."),
             p("Use the 'Clear selection(s)' button at any time to reset the application.")),
    tabPanel("Methods")
  )
 
)
