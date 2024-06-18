server <- function(input, output, session) {
  
  # reactiveVal for the map object, and corresponding output object.
  react_map <- reactiveVal(base_map())
  
  # map-------------------------
  output$map <- renderLeaflet({
    react_map()
  })
  
   observeEvent(input$filterSpace, {
     
      if(input$filterSpace == "Site") {
     #   #print("site")
     #   
       leafletProxy("map") %>%
         clearGroup("circles") %>%
         addCircleMarkers(data = sites_sf,
                          lng = ~lon, lat = ~lat,
                          layerId = ~sid,
                          group = "sites",
                          color = "black",
                          weight = 1,
                          opacity = 0.6,
                          fill = TRUE,
                          fillColor = ~palSite(cat),
                          fillOpacity = 0.6,
                          popup = ~categoryString)  %>%
         addLegend("topleft", pal = palSite, values = sites$cat,
                   title = "Land cover",
                   layerId = "landLegend",
                   #labFormat = labelFormat(prefix = "$"),
                   opacity = 1)
       
       output$siteSelectUI <- renderUI({
         
         req(input$filterSpace == "Site")
         tagList(
           h5("Find your site on the map and click the site marker"),
           uiOutput("siteID")
         )
        })

     } else if(input$filterSpace == "Land cover") {

       leafletProxy("map") %>%
         clearGroup("sites") %>%
         clearGroup("circles") %>%
         addRasterImage(landCover, group = "landCover",
                        colors = c("#b50101", "#e8d1d2",  "#cb9147", "darkgreen", "skyblue1", "steelblue3", "wheat")) %>%
         addLegend("topleft", pal = palCover, values = c("Urban", "Suburban", "Rural/Ag", "Forest", "Open water", "Wetland", "Barren/shrubland"),
                   title = "Land groups",
                   layerId = "landLegend",
                   #labFormat = labelFormat(prefix = "$"),
                   opacity = 1)

       output$landCoverSelectUI <- renderUI({

         req(input$filterSpace == "Land cover")
         selectInput(inputId = "landCover", label = "Select land cover", choices = c("Rural", "Suburban", "Urban"))

       })
       
       sid(NULL)
       sidData(NULL)
     }
     
   })
  
   # to tell users that they have selected a site, and which it is
  sid <- reactiveVal()
  # to create the data for site
  sidData <- reactiveVal()
  # to create data for landcover group
  landData <- reactiveVal()
   
  observeEvent(input$map_marker_click, {
    
    print(input$map_marker_click)
    click <- input$map_marker_click
    sid(click$id)
    
    #return sidData
    sidDF <- siteData(dat, site = click$id)
    sidData(sidDF)
    
  })
  
  output$siteID <- renderUI({
    
    req(sid())
    textOutput("sideIDtext")
    
  })
  
  output$sideIDtext <- renderText({
    
    paste("Your site is:", sid()) 
    
  })
  
  # observeEvent(input$landCover, {
  #   
  #   lcData <- coverData(dat, input$landCover)
  #   landData(lcData)
  #   
  # })
  
  output$filterTimeUI <- renderUI({
    
    req(input$filterSpace != " ")
    tagList(
      selectInput(inputId = "filterTime", label = "Filter data in time by:", choices = c(" ", "Year", "Month", "Day")),
      uiOutput("timeSelectUI")
    )
    
  })
  
  
  output$timeSelectUI <- renderUI({
    
    #req(input$filterTime != " ")
    req(
      isTruthy(sidData()) || isTruthy(input$landCover)
      )
    
    print("insideTimeSelect")
    print(head(sidData()))
    print(!is.null(sidData()))
    
    tagList(
      uiOutput("yearSelect"),
      uiOutput("monthSelect"),
      uiOutput("daySelect")
    )
    # if(input$filterTime == "Year") {
    #   selectInput(inputId = "year", label = "Select year", choices = c("", yearChoices))
    # } else if(input$filterTime == "Month") {
    #   monthChoices = siteYearData(sidData(), yearSelect = input$year)
    #   print(monthChoices)
    #   # tagList(
    #   #   selectInput(inputId = "year", label = "Select year", choices = c("", yearChoices)),
    #   #   selectInput(inputId = "month", label = "Select month", choices = monthChoices)
    #   # )
    #  } else if(input$filterTime == "Day") {
    #    tagList(
    #      selectInput(inputId = "year", label = "Select year", choices = c("", yearChoices)),
    #      selectInput(inputId = "month", label = "Select month", choices = monthChoices),
    #      selectInput(inputId = "day", label = "Select day", choices = 1:31)
    #    )
    #  }
  })
  
  output$yearSelect <- renderUI({
    
    if(!is.null(sidData())) {
      yearChoices <- unique(sidData()$year)
    } else {
      yearChoices <- c(2012:2023)
    }
    req(input$filterTime != " ")
    selectInput(inputId = "year", label = "Select year", choices = c("", yearChoices))
    
  })
  
  output$monthSelect <- renderUI({
    
    req(input$year)
    req(input$filterTime == "Month" || input$filterTime == "Day")
    monthChoices <- siteYearData(sidData(), input$year)
    monthChoices <- month.abb[sort(unique(monthChoices$month))]
    print(monthChoices)
    selectInput(inputId = "month", label = "Select month", choices = c("", monthChoices))
  
  })
  
  output$daySelect <- renderUI({
    
    req(input$year)
    req(input$month)
    req(input$filterTime == "Day")
    dayChoices <- siteMonthData(sidData(), yearSelect = input$year, monthSelect = input$month)
    dayChoices <- sort(unique(dayChoices$day))
    selectInput(inputId = "day", label = "Select day", choices = dayChoices)
    
  })
  
output$plotUI <- renderUI({

  req(isTruthy(input$year),
      isTruthy(sidData()) || isTruthy(input$landCover)
  )

  if(!is.null(sidData())) {
    plotData <- dat %>%
      filter(cat == input$landCover,
             year == input$year)
  } else {
    plotData <- sidData() %>%
      filter(year == input$year)
  }

  print(head(plotData))

  renderPlot({
    yearPlot(plotData)
    })

  })

}
