server <- function(input, output, session) {
  
  # reactiveVal for the map object, and corresponding output object.
  react_map <- reactiveVal(base_map())
  
  # map-------------------------
  output$map <- renderLeaflet({
    react_map()
  })
  
  # reactive vals--------------
  # to tell users that they have selected a site, and which it is
  sid <- reactiveVal()
  # to create the data for site
  sidData <- reactiveVal()
  # to label which data set
  spaceFilter <- reactiveVal()
  
   observeEvent(input$filterSpace, {
     
      if(input$filterSpace == "Site") {
        
        spaceFilter("site")
        
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
                    opacity = 1)
       
       output$siteSelectUI <- renderUI({
         
         req(input$filterSpace == "Site")
         tagList(
           h5("Find your site on the map and click the site marker"),
           uiOutput("siteID")
         )
        })

     } else if(input$filterSpace == "Land cover") {
       
       spaceFilter("landcover")

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
    print(spaceFilter())
    
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
    
    if(spaceFilter() == "site") {
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
    if(spaceFilter() == "site") {
      monthChoices <- siteYearData(sidData(), input$year)
      monthChoices <- month.abb[sort(unique(monthChoices$month))]
    } else {
      monthChoices <- month.abb[1:12]
    }
    
    print(monthChoices)
    selectInput(inputId = "month", label = "Select month", choices = c("", monthChoices))
  
  })
  
  output$daySelect <- renderUI({
    
    req(input$year)
    req(input$month)
    print(input$month)
    req(input$filterTime == "Day")
    if(spaceFilter() == "site") {
      dayChoices <- siteMonthData(sidData(), yearSelect = input$year, monthSelect = match(input$month, month.abb))
      ##TODO have to either edit data to have month.abb or filter by number
      print(dayChoices)
      dayChoices <- sort(unique(dayChoices$day))
    } else {
      dayChoices <- days %>% filter(year == input$year, month == input$month)
      dayChoices <- 1:dayChoices$day
    }
    print(dayChoices)
    
    selectInput(inputId = "day", label = "Select day", choices = c("", dayChoices))
    
  })
  
output$plotUI <- renderUI({

  req(isTruthy(input$year),
      isTruthy(sidData()) || isTruthy(input$landCover)
  )
  
  if(input$filterTime == "Year") {
    plotOutput("yearFig")
  } else if(input$filterTime == "Month") {
    plotOutput("monthFig")
  } else {
    plotOutput("dayFig")
  }
  # tagList(
  #   plotOutput("yearFig"),
  #   plotOutput("monthFig"),
  #   plotOutput("dayFig")
  # )

  })

output$yearFig <- renderPlot({
  
  req(input$year)
  req(input$filterTime == "Year")
  #if whichever data is selected, and then filter to the year
  if(spaceFilter() == "site") {
    sidDat <- sidData() %>% filter(year == input$year) 
    title <- paste("Temperatures at", sid(), "in", input$year)
    p <- yearPlot(sidDat, title)
  } else {
    title <- paste("Temperatures in", input$landCover, "areas in", input$year)
    landCoverDat <- dat %>% filter(cat == input$landCover, year == input$year)
    print(head(landCoverDat))
    p <- yearPlot(landCoverDat, title)
  }
  
  return(p)
  
})

output$monthFig <- renderPlot({
  
  req(input$filterTime == "Month")
  req(input$month)
  
  if(spaceFilter() == "site") {
    #print("inside site month plot")
    #print(head(sidData()))
    sidDat <- siteMonthData(sidData(), yearSelect = input$year, monthSelect = match(input$month, month.abb))
    # sidDat <- sidDat %>% mutate_if(is.character, as.factor)
    # print(summary(sidDat))
    title <- paste("Temperatures at", sid(), "in", input$month, input$year)
    #print(title)
    p <- monthPlot(sidDat, title)
  } else {
    title <- paste("Temperatures in", input$landCover, "areas in", input$month, input$year)
    landCoverDat <- dat %>% filter(cat == input$landCover, year == input$year, month == match(input$month, month.abb))
    print(head(landCoverDat))
    p <- monthPlot(landCoverDat, title)
  }
  
  return(p)
  
})

output$dayFig <- renderPlot({
  
  req(input$filterTime == "Day")
  req(input$day)
  
  if(spaceFilter() == "site") {
    print("inside site day plot")
    #print(head(sidData()))
    sidDat <- siteDayData(sidData(), yearSelect = input$year, monthSelect = match(input$month, month.abb), daySelect = input$day)
    # sidDat <- sidDat %>% mutate_if(is.character, as.factor)
    # print(summary(sidDat))
    title <- paste0("Temperatures at ", sid(), " on ", input$month, " ", input$day, ", ", input$year)
    #print(title)
    p <- dayPlot(sidDat, title, "site")
  } else {
    title <- paste0("Temperatures in ", input$landCover, " areas on ", input$month, " ", input$day, ", ", input$year)
    landCoverDat <- dat %>% filter(cat == input$landCover, year == input$year, month == match(input$month, month.abb), day == input$day)
    print(nrow(landCoverDat))
    p <- dayPlot(landCoverDat, title, "landcover")
  }
  
  return(p)
  
})


}
