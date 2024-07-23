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
  sid2 <- reactiveVal()
  # to create the data for site
  sidData <- reactiveVal()
  # to label which data set
  spaceFilter <- reactiveVal()
  # landCover for reset
  cover <- reactiveVal()
  cover2 <- reactiveVal()
  
  # change map display for filterSpace-----------
   observeEvent(input$filterSpace, {
     
      if(input$filterSpace == "Site") {
        
        spaceFilter("site")
        
        leafletProxy("map") %>%
          addCircleMarkers(data = sites_sf,
                            lng = ~lon, lat = ~lat,
                            layerId = ~sid,
                            group = "siteData",
                            color = "black",
                            weight = 1,
                            opacity = 0.6,
                            fill = TRUE,
                            fillColor = ~palSite(cat),
                            fillOpacity = 0.6,
                            popup = ~categoryString) 
       
       output$siteSelectUI <- renderUI({
         
         req(input$filterSpace == "Site")
         tagList(
           h5("Find your site on the map and click the site marker"),
           uiOutput("siteID")
         )
        })
       
       cover(NULL)
       
     } else if(input$filterSpace == "Land cover") {
       
       spaceFilter("landcover")

       leafletProxy("map") %>%
         clearGroup("siteData") 

       output$landCoverSelectUI <- renderUI({

         req(input$filterSpace == "Land cover")
         selectInput(inputId = "landCover", label = "Select land cover", choices = c(" ", "Rural", "Suburban", "Urban"))
         
       })
       
       sid(NULL)
       sidData(NULL)
     }
     
   })
  
   
  # create sid-------------------
  observeEvent(input$map_marker_click, {
    
    ##TODO this can only be created if addSelect is false
    if(!isTruthy(input$addSelect)) {
      click <- input$map_marker_click
      sid(click$id)
      print("first click")
      print(sid())
      
      #return sidData
      sidDF <- siteData(dat, site = click$id)
      sidData(sidDF)
    }
    
  })
  
  output$siteID <- renderUI({
    
    req(sid())
    textOutput("sideIDtext")
    
  })
  
  output$sideIDtext <- renderText({
    
    description <- unique(sidData()$categoryString)
    paste0("Your site is ", sid(), ": ", description) 
    
  })
  
  # create landcover----------------
  observeEvent(input$landCover, {

    if(input$landCover != " ") {
      landcover <- input$landCover
      cover(landcover)
    }

  })
  
  
  
  # filterTimeUI--------------------
  output$filterTimeUI <- renderUI({
    
    #req(input$filterSpace != " ")
    req(isTruthy(cover()) || isTruthy(sid()))
    #print("inside filterTimeUI")
    #print(input$landCover)
    tagList(
      selectInput(inputId = "filterTime", label = "Filter data in time by:", choices = c(" ", "Year", "Month", "Day")),
      uiOutput("timeSelectUI")
    )
    
  })
  
  #addSelectionUI------------------
  output$addSelectionUI <- renderUI({
    
    req(input$tempLabel)
    wellPanel(
      tagList(
        fluidRow(
          column(6,
                 checkboxInput(inputId = "addSelect", label = "Compare additional location(s)", value = FALSE)),
          column(6,
                 uiOutput("filterSpace2UI"),
                 uiOutput("siteSelect2UI"),
                 uiOutput("landCoverSelect2UI"))
        )
      )
    )
    
  })
  
  output$filterSpace2UI <- renderUI({
    
    req(input$addSelect)
    selectInput(inputId = "filterSpace2", label = "Filter comparison data by:", choices = c(" ", "Site", "Land cover"), selected = " ")
    
  })
  
  output$siteSelect2UI <- renderUI({

    req(input$filterSpace2 == "Site")
    tagList(
      h5("Find second site on the map and click the site marker"),
      #uiOutput("siteID2")
    )
  })
  
  ## update map for second site selection-----------------
  observeEvent(input$filterSpace2, {
    
    if(input$filterSpace2 == "Site") {
      
      leafletProxy("map") %>%
        addCircleMarkers(data = sites_sf,
                         lng = ~lon, lat = ~lat,
                         layerId = ~sid,
                         group = "siteData",
                         color = "black",
                         weight = 1,
                         opacity = 0.6,
                         fill = TRUE,
                         fillColor = ~palSite(cat),
                         fillOpacity = 0.6,
                         popup = ~categoryString) 
      
      observeEvent(input$map_marker_click, {
        
        click2 <- input$map_marker_click
        sid2(click2$id)
        print("second click")
        print(sid2())
        
      })
      
    } else if(input$filterSpace2 == "Land cover") {
      
      leafletProxy("map") %>%
        clearGroup("siteData")
    }
      
  })
  
  observeEvent(input$landCover2, {
    
    if(input$landCover2 != " ") {
      landcover <- input$landCover2
      cover2(landcover)
    }
    
  })
  
  

  output$landCoverSelect2UI <- renderUI({

    req(input$filterSpace2 == "Land cover")
    landCoverChoices = c(" ", "Rural", "Suburban", "Urban")
    alreadyChose = cover()  
    newChoices = landCoverChoices[!landCoverChoices %in% alreadyChose]
    tagList(
      selectInput(inputId = "landCover2", label = "Select comparison land cover", choices = newChoices)
    )
  })
  
  # timeSelectUI-------------
  output$timeSelectUI <- renderUI({
    
    req(
      isTruthy(sid()) || isTruthy(input$landCover)
      )
    
    tagList(
      uiOutput("yearSelect"),
      uiOutput("monthSelect"),
      uiOutput("daySelect")
    )
   
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
      if(input$year == 2012) {
        monthChoices <- month.abb[3:12] 
      } else{
        monthChoices <- month.abb[1:12]
      }
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
  
  # create data-----------------
  ##dat1---------------
  dat1 <- reactive({
    
    req(input$filterSpace)
    req(input$year)
    
    
    if(input$filterSpace == "Site") {
      landLabel1 = sid()
    } else if(input$filterSpace == "Land cover") {
      landLabel1 = input$landCover
    }
    
    
    d1 <- createSiteYearData(siteType = input$filterSpace, dat = dat, yearSelect = input$year, landLabel = landLabel1)
    
    #print(head(d1))
    return(d1)
    
  })

  
  ##dat2-----------------
  dat2 <- reactive({
  
    req(
      isTruthy(sid2()) || isTruthy(cover2())
    )
    
    ##TODO needs to start creating after filterSpace is selected
    
    print("creating dat2")
    print(sid2())
    print(cover2())
    
    if(input$filterSpace2 == "Site") {
      landLabel2 <- sid2()
    } else if(input$filterSpace2 == "Land cover") {
      landLabel2 <- cover2()
    }

    d2 <- createSiteYearData(siteType = input$filterSpace2, dat = dat, yearSelect = input$year, landLabel = landLabel2)
    #print(head(d2))
    return(d2)
    
  })
  
  #plotUI---------------------------
  output$plotUI <- renderUI({
    
    req(isTruthy(input$year),
      isTruthy(sidData()) || isTruthy(input$landCover)
    )
    
    tagList(
      radioButtons("tempLabel", "Degree units display", choices = c("Celsius", "Fahrenheit"), selected = "Celsius", inline = TRUE),
      if(input$filterTime == "Year") {
        withSpinner(plotlyOutput("yearFig"), type = 8)
      } else if(input$filterTime == "Month") {
        withSpinner(plotlyOutput("monthFig"), type = 8)
      } else {
        withSpinner(plotlyOutput("dayFig"), type = 8)
      }
    )
    
   
  })
  


  ### year plot---------
  
  output$yearFig <- renderPlotly({
  
  req(dat1())
  req(input$filterTime == "Year")
  req(input$year)
  deg = input$tempLabel 
  
  dat1df <- dat1()
  
  if(deg == "Celsius") {
    convertDat1 <- dat1df %>%
      mutate(temp = round(tempC,1))
    yLabel = "Mean temp (°C)"
  } else if(deg == "Fahrenheit") {
    yLabel = "Mean temp (°F)"
    convertDat1 <- dat1df %>%
      mutate(temp = round(CtoF(tempC),1))
  }
  
  if(input$filterSpace == "Site") {
    title = paste("Temperatures at ", sid(), " in ", input$year)
    landLabel1 = sid() ##TODO this can only be created at first map click
  } else if(input$filterSpace == "Land cover") {
    title = paste("Temperatures in", input$landCover, "areas in", input$year)
    landLabel1 = input$landCover
  }
  
  if(isTruthy(sid2()) || isTruthy(cover2())) {
    print("moving to compare plot")
    dat2df <- dat2()
    if(deg == "Celsius") {
      convertDat2 <- dat2df %>%
        mutate(temp = round(tempC,1))
    } else if(deg == "Fahrenheit") {
      convertDat2 <- dat2df %>%
        mutate(temp = round(CtoF(tempC),1))
    }
    
    if(input$filterSpace2 == "Site") {
      landLabel2 <- sid2()
    } else if(input$filterSpace2 == "Land cover") {
      landLabel2 <- input$landCover2
    }
    
    title = paste("Temperatures in", input$year)
    print("dat1")
    print(head(convertDat1))
    print(landLabel1)
    print("dat2")
    print(head(convertDat2))
    print(landLabel2)
    p <- completeYearPlot(dat1 = convertDat1, dat2 = convertDat2, title = title, 
                          landLabel1 = landLabel1, landLabel2 = landLabel2, yLabel = yLabel, compare = TRUE)
  } else {
    print("single plot")
    print(head(convertDat1))
    print(landLabel1)
    p <- completeYearPlot(dat1 = convertDat1, title = title, landLabel1 = landLabel1, yLabel = yLabel, compare = FALSE)
  }
 
  return(p)
  
})
  

output$monthFig <- renderPlotly({
  
  req(input$filterTime == "Month")
  req(input$month)
  deg = input$tempLabel
  
  if(spaceFilter() == "site") {
    #print("inside site month plot")
    #print(head(sidData()))
    sidDat <- siteMonthData(sidData(), yearSelect = input$year, monthSelect = match(input$month, month.abb))
    # sidDat <- sidDat %>% mutate_if(is.character, as.factor)
    # print(summary(sidDat))
    title <- paste("Temperatures at", sid(), "in", input$month, input$year)
    #print(title)
    p <- monthPlot(sidDat, title, degree = deg)
  } else {
    title <- paste("Temperatures in", input$landCover, "areas in", input$month, input$year)
    landCoverDat <- dat %>% filter(cat == input$landCover, year == input$year, month == match(input$month, month.abb))
    print(head(landCoverDat))
    p <- monthPlot(landCoverDat, title, degree = deg)
  }
  
  return(p)
  
})

output$dayFig <- renderPlotly({
  
  req(input$filterTime == "Day")
  req(input$day)
  deg = input$tempLabel
  
  if(spaceFilter() == "site") {
    print("inside site day plot")
    #print(head(sidData()))
    sidDat <- siteDayData(sidData(), yearSelect = input$year, monthSelect = match(input$month, month.abb), daySelect = input$day)
    # sidDat <- sidDat %>% mutate_if(is.character, as.factor)
    # print(summary(sidDat))
    title <- paste0("Temperatures at ", sid(), " on ", input$month, " ", input$day, ", ", input$year)
    #print(title)
    p <- dayPlot(sidDat, title, degree = deg, datType = "site")
  } else {
    title <- paste0("Temperatures in ", input$landCover, " areas on ", input$month, " ", input$day, ", ", input$year)
    landCoverDat <- dat %>% filter(cat == input$landCover, year == input$year, month == match(input$month, month.abb), day == input$day)
    print(nrow(landCoverDat))
    p <- dayPlot(landCoverDat, title, degree = deg, datType = "landcover")
  }
  
  return(p)
  
})


}
