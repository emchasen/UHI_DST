
ui <- fluidPage(
  
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
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
           wellPanel(
             shinyjs::useShinyjs(),
             fluidRow(
               column(6,
                      checkboxInput(inputId = "addSelect", label = "Compare additional location", value = FALSE)
                      ),
               column(6,
                      uiOutput("filterSpace2UI"),
                      uiOutput("siteSelect2UI"),
                      uiOutput("landCoverSelect2UI"))
               )
             )
           )
           #uiOutput("addSelectionUI"))
  ),
  hr(),
  uiOutput("plotUI") %>% withSpinner()
)
