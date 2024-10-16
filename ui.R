
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
             br(),
             p(HTML("<b>Data selection</b>")),
             p("To observe temperature data for Dane County, first determine if you want to observe data from a single location ('Site'),
             or across land cover type ('Land cover'). If you click on 'site', choose your location by clicking directly on the map. If you
             click 'Land cover', choose the group by using the drop down menu."),
             p("Next, decide if you want to observe temperature patterns over a year, month, day or date range."),
             p(HTML("<b>Data visualization</b>")),
             p("After selecting your time filter and choosing the time, the data is displayed visually in a chart.
               You can change the degree units and the chart will update. If you hover the mouse cursor over the chart, data will 
               be displayed in text format as the cursor moves. Additional features include the ability to zoom in and out of the chart,
               and to reset the original chart axes. If multiple legend items are visible, you can also toggle individual legend items on and
               off, which will hide or display data."),
             p(HTML("<b>Data comparison</b>")),
             p("Comparison data allows the user to select another site or landcover to compare to the original data set for the selected time period."),
             p(HTML("<i>Note: Once you have selected an additional data set, a user can change the selected time (e.g. if data has been filtered by 
                    Year, the user can change which year is observed), but if the user changes the 'Filter data in time' (e.g. originally the
                    data is filtered by Year and then the user decides to filter by month), the second data set will need to be reselected.</i>")),
             p(HTML("<b>Clear data</b>")),
             p("Use the 'Clear selection(s)' button at any time to reset the application."),
             p(HTML("<b>Map layers</b>")),
             p("Use the drop down menu to toggle between different map layers, or none at all."),
             p(HTML("<b>Download data</b>")),
             p("Click the 'Download Data' button to save the filtered data directly to your computer. The downloaded data is the data represented on the 
               figures, and not the raw data. For example, when observing data filtered by month, the data are the mean, minimum and maximum and not the data
               used to calculate the statistics. Data are downloaded in degrees Celsius.")),
    tabPanel("Methods",
             br(),
             p(HTML("<b>Data collection</b>")),
             p("This application displays data from a network of 150 temperature and humidity sensors installed on streetlights and utility poles 
               across the Dane County metropolitan area, which have been taking recordings every 15 minutes since 2012. The data displayed in the application
               uses the subset of data which is recorded every hour."),
             p(HTML("<b>Map layers</b>")),
             p(em("Land cover")),
             p("Land cover is displayed from the ", tags$a(href="https://dnr.wisconsin.gov/maps/WISCLAND", "Wiscland 2.0"), "data layer."),
             p(em("Heat maps")),
             p("Describe the interpolation process here."),
             br(),
             br(),
             hr(style = "margin-top:0px"),
             p(em("Source code is available ", tags$a(href="https://github.com/emchasen/UHI_DST/", "here"))))
  )
 
)
