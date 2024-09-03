
ui <- fluidPage(
  
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  ),
  
  titlePanel("Dane County Urban Heat Island Exploration"),
  h4("Observe temperature patterns by site or land cover."),
  leafletOutput("map", height = 350) %>%
    withSpinner(type = 3,
                color.background = "white"),
  wellPanel(
    fluidRow(
      column(6,
             selectInput(inputId = "filterSpace", label = "Filter data spatially by:", choices = c(" ", "Site", "Land cover"), selected = " "),
             uiOutput("siteSelectUI"),
             uiOutput("landCoverSelectUI"),
             # shinyjs::useShinyjs(),
             # actionButton("filterData", "Make plot")
             ),
      column(6,
             uiOutput("filterTimeUI")))),
     
  hr(),
  uiOutput("dateRangeText"),
  uiOutput("plotUI") #%>% withSpinner()#,
  # wellPanel(
  #   shinyjs::useShinyjs(),
  #   fluidRow(
  #     column(6,
  #            checkboxInput(inputId = "addSelect", label = "Compare additional location", value = FALSE)
  #            #actionButton("clear", "Clear selection(s)")
  #     ),
  #     column(6,
  #            uiOutput("filterSpace2UI"),
  #            uiOutput("siteSelect2UI"),
  #            uiOutput("landCoverSelect2UI"))
  #   )
  #)
)
