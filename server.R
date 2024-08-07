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
      #print("first click")
      #print(sid())
      
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
    
    req(isTruthy(input$year),
        isTruthy(sidData()) || isTruthy(input$landCover)
    )
    
    wellPanel(
      tagList(
        fluidRow(
          column(6,
                 checkboxInput(inputId = "addSelect", label = "Compare additional location(s)", value = FALSE),
                 actionButton(inputId = "clear", label = "Clear selection(s)")),
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
      ##TODO can't change from land cover selection back to site (after land cover has been selected and graph made)
      ##TODO vice versa. Is this because dat2?
      
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
        #print("second click")
        #print(sid2())
        
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
      monthChoices <- sidData() %>% filter(year == input$year)
      monthChoices <- month.abb[sort(unique(monthChoices$month))]
    } else {
      if(input$year == 2012) {
        monthChoices <- month.abb[3:12] 
      } else{
        monthChoices <- month.abb[1:12]
      }
    }
    #print(monthChoices)
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
  rv <- reactiveValues(
    
    dat1 = NULL,
    dat2 = NULL
    
  )
  
  ## dat1---------------
  observe({
    
    req(input$filterSpace)
    req(input$year)
    
    if(input$filterSpace == "Site") {
      landLabel1 = sid()
    } else if(input$filterSpace == "Land cover") {
      landLabel1 = input$landCover
    }
    
    if(input$filterTime == "Year") {
      print("creating year dat1")
      rv$dat1 <- createSiteYearData(siteType = input$filterSpace, dat = dat, yearSelect = input$year, landLabel = landLabel1)
    } else if(input$filterTime == "Month") {
      req(input$month)
      print("creating month dat1")
      month = which(input$month == month.abb)[[1]]
      rv$dat1 <- createSiteMonthData(siteType = input$filterSpace, dat = dat, yearSelect = input$year, monthSelect = month, landLabel = landLabel1)
    } else if(input$filterTime == "Day") {
      req(input$day)
      print("creating day dat1")
      month = which(input$month == month.abb)[[1]]
      rv$dat1 <- createSiteDayData(siteType = input$filterSpace, dat = dat, yearSelect = input$year, monthSelect = month, daySelect = input$day, landLabel = landLabel1)
    }
    
    print(head(rv$dat1))

  })
  
  ##dat2-----------------
  
  observe({
    
      req(
        isTruthy(sid2()) || isTruthy(cover2())
      )
      req(input$filterSpace2)
      req(input$year)

      print(input$filterSpace2)
      if(input$filterSpace2 == "Site") {
        landLabel2 <- sid2()
      } else if(input$filterSpace2 == "Land cover") {
        landLabel2 <- cover2()
      }
      
      if(input$filterTime == "Year") {
        print("creating year dat2")
        rv$dat2 <- createSiteYearData(siteType = input$filterSpace2, dat = dat, yearSelect = input$year, landLabel = landLabel2)
      } else if(input$filterTime == "Month") {
        req(input$month)
        print("creating month dat2")
        month = which(input$month == month.abb)[[1]]
        rv$dat2 <- createSiteMonthData(siteType = input$filterSpace2, dat = dat, yearSelect = input$year, monthSelect = month, landLabel = landLabel2)
      } else if(input$filterTime == "Day") {
        req(input$day)
        print("creating day dat2")
        month = which(input$month == month.abb)[[1]]
        rv$dat2 <- createSiteDayData(siteType = input$filterSpace2, dat = dat, yearSelect = input$year, monthSelect = month, daySelect = input$day, landLabel = landLabel2)
      }
  })
 
  
  #plotUI---------------------------
  
  output$plotUI <- renderUI({
    
    # req(isTruthy(input$year),
    #   isTruthy(sidData()) || isTruthy(input$landCover)
    # )
    req(rv$dat1)
    
    tagList(
      radioButtons("tempLabel", "Degree units display", choices = c("Celsius", "Fahrenheit"), selected = "Celsius", inline = TRUE),
      if(input$filterTime == "Year") {
        uiOutput("yearFig")
        #withSpinner(plotlyOutput("yearFig"), type = 8)
      } else if(input$filterTime == "Month") {
        #withSpinner(plotlyOutput("monthFig"), type = 8)
        uiOutput("monthFig")
      } else if(input$filterTime == "Day") {
        #withSpinner(plotlyOutput("dayFig"), type = 8)
        uiOutput("dayFigure")
      }
    )
    
   
  })
  

  ### year plot ---------
  output$yearFig <- renderUI({
    
    req(input$filterTime == "Year")
    
    if(isTruthy(sid2()) || isTruthy(cover2())) {
      withSpinner(plotlyOutput("compareDatYearFig"), type = 8)
    } else {
      withSpinner(plotlyOutput("singleDatYearFig"), type = 8)
    }
    
  })
  
  output$singleDatYearFig <- renderPlotly({
  
    print("singleDatYearFig")
    dat1df <- req(rv$dat1)
    req(input$year)
    deg <- req(input$tempLabel) 
    #print(head(dat1df))
 
  
    if(deg == "Celsius") {
      convertDat1 <- dat1df %>%
        mutate(temp = round(tempC,1))
      yLabel = "Mean temp (°C)"
    } else if(deg == "Fahrenheit") {
      yLabel = "Mean temp (°F)"
      convertDat1 <- dat1df %>%
        mutate(temp = round(CtoF(tempC),1))
    }
    #print(head(convertDat1))
  
    if(input$filterSpace == "Site") {
      title = paste("Temperatures at", sid(), "in", input$year)
      landLabel1 = sid() 
    } else if(input$filterSpace == "Land cover") {
      title = paste("Temperatures in", input$landCover, "areas in", input$year)
      landLabel1 = input$landCover
    }
  
    completeYearPlot(dat1 = convertDat1, title = title, landLabel1 = landLabel1, yLabel = yLabel, compare = FALSE)
  
  })
  
  output$compareDatYearFig <- renderPlotly({
    
    print("compareYearFig")
    req(input$year)
    dat1df <- req(rv$dat1)
    dat2df <- req(rv$dat2)
    deg <- req(input$tempLabel) 
    
    if(deg == "Celsius") {
      convertDat1 <- dat1df %>%
        mutate(temp = round(tempC,1))
      convertDat2 <- dat2df %>%
        mutate(temp = round(tempC,1))
      yLabel = "Mean temp (°C)"
    } else if(deg == "Fahrenheit") {
      convertDat1 <- dat1df %>%
        mutate(temp = round(CtoF(tempC),1))
      convertDat2 <- dat2df %>%
        mutate(temp = round(CtoF(tempC),1))
      yLabel = "Mean temp (°F)"
    }
    print(head(convertDat1))
    print(head(convertDat2))
    
    if(input$filterSpace == "Site") {
      landLabel1 = sid() 
    } else if(input$filterSpace == "Land cover") {
      landLabel1 = input$landCover
    }
    print("landLabel1")
    print(landLabel1)
    
    if(input$filterSpace2 == "Site") {
      landLabel2 = sid2() 
    } else if(input$filterSpace2 == "Land cover") {
      landLabel2 = input$landCover2
    }
    print("landLabel2")
    print(landLabel2)
    
    title = paste("Temperatures in", input$year)
    
    completeYearPlot(dat1 = convertDat1, dat2 = convertDat2, title = title, 
                     landLabel1 = landLabel1, landLabel2 = landLabel2, yLabel = yLabel, compare = TRUE)
    
  })
  
  ### month plot ---------
  output$monthFig <- renderUI({
    
    req(input$filterTime == "Month")
    
    if(isTruthy(sid2()) || isTruthy(cover2())) {
      withSpinner(plotlyOutput("compareDatMonthFig"), type = 8)
    } else {
      withSpinner(plotlyOutput("singleDatMonthFig"), type = 8)
    }
    
  })
  
  output$singleDatMonthFig <- renderPlotly({
    
    dat1df <- req(rv$dat1)
    req(input$year)
    deg <- req(input$tempLabel) 
    #print(head(dat1df))
    
    if(deg == "Celsius") {
      convertDat1 <- dat1df 
      yLabel = "Mean temp (°C)"
    } else if(deg == "Fahrenheit") {
      yLabel = "Mean temp (°F)"
      convertDat1 <- dat1df %>%
        mutate(meanTemp = round(CtoF(meanTemp),2),
               minTemp = round(CtoF(minTemp), 2),
               maxTemp = round(CtoF(maxTemp), 2))
    }
    
    if(input$filterSpace == "Site") {
      title = paste0("Temperatures at ", sid(), " in ", input$month, ", ", input$year)
      landLabel1 = sid() 
    } else if(input$filterSpace == "Land cover") {
      title = paste0("Temperatures in ", input$landCover, " areas in ", input$month, ", ", input$year)
      landLabel1 = input$landCover
    }
    
    completeMonthPlot(dat1 = convertDat1, title = title, landLabel1 = landLabel1, yLabel = yLabel, compare = FALSE)
    
  })
  
  output$compareDatMonthFig <- renderPlotly({

    dat1df <- req(rv$dat1)
    dat2df <- req(rv$dat2)
    deg <- req(input$tempLabel)

    if(deg == "Celsius") {
      convertDat1 <- dat1df 
      convertDat2 <- dat2df 
      yLabel = "Mean temp (°C)"
    } else if(deg == "Fahrenheit") {
      convertDat1 <- dat1df %>%
        mutate(meanTemp = round(CtoF(meanTemp),2),
               minTemp = round(CtoF(minTemp), 2),
               maxTemp = round(CtoF(maxTemp), 2))
      convertDat2 <- dat2df %>%
        mutate(meanTemp = round(CtoF(meanTemp),2),
               minTemp = round(CtoF(minTemp), 2),
               maxTemp = round(CtoF(maxTemp), 2))
      yLabel = "Mean temp (°F)"
    }
    # print(head(convertDat1))
    # print(head(convertDat2))

    if(input$filterSpace == "Site") {
      landLabel1 = sid()
    } else if(input$filterSpace == "Land cover") {
      landLabel1 = input$landCover
    }

    if(input$filterSpace2 == "Site") {
      landLabel2 = sid2()
    } else if(input$filterSpace2 == "Land cover") {
      landLabel2 = input$landCover2
    }

    title = paste0("Temperatures in ", input$month, ", ", input$year)

    completeMonthPlot(dat1 = convertDat1, dat2 = convertDat2, title = title,
                     landLabel1 = landLabel1, landLabel2 = landLabel2, yLabel = yLabel, compare = TRUE)

  })
  
  ### day plot ---------
  output$dayFigure <- renderUI({
    
    req(input$filterTime == "Day")
    
    if(isTruthy(sid2()) || isTruthy(cover2())) {
      withSpinner(plotlyOutput("compareDatDayFig"), type = 8)
    } else {
      withSpinner(plotlyOutput("singleDatDayFig"), type = 8)
    }
    
  })
  
  output$singleDatDayFig <- renderPlotly({
    
    dat1df <- req(rv$dat1)
    deg <- req(input$tempLabel) 
    
    if(deg == "Celsius") {
      convertDat1 <- dat1df 
      yLabel = "Mean temp (°C)"
    } else if(deg == "Fahrenheit") {
      yLabel = "Mean temp (°F)"
      ##TODO how should I handle the se? I can't convert it with CtoF
      convertDat1 <- dat1df %>%
        mutate(meanTemp = round(CtoF(meanTemp),2))
    }
    print(head(convertDat1))
    
    if(input$filterSpace == "Site") {
      title = paste0("Temperatures at ", sid(), " on ", input$month, " ", input$day,", ", input$year)
      landLabel1 = sid() 
    } else if(input$filterSpace == "Land cover") {
      title = paste0("Temperatures in ", input$landCover, " areas on ", input$day, " ", input$month, ", ", input$year)
      landLabel1 = input$landCover
    }
    
    completeDayPlot(dat1 = convertDat1, datType1 = input$filterSpace, title = title, landLabel1 = landLabel1, yLabel = yLabel, compare = FALSE)
    
  })
  
  output$compareDatDayFig <- renderPlotly({
    
    print("compareDayFig")
    dat1df <- req(rv$dat1)
    dat2df <- req(rv$dat2)
    deg <- req(input$tempLabel)
  
    if(deg == "Celsius") {
      convertDat1 <- dat1df 
      convertDat2 <- dat2df 
      yLabel = "Mean temp (°C)"
    } else if(deg == "Fahrenheit") {
      ##TODO how should I handle the se? I can't convert it with CtoF
      convertDat1 <- dat1df %>%
        mutate(meanTemp = round(CtoF(meanTemp),2))
      convertDat2 <- dat2df %>%
        mutate(meanTemp = round(CtoF(meanTemp),2))
      yLabel = "Mean temp (°F)"
    }
    print(head(convertDat1))
    print(head(convertDat2))
    
    if(input$filterSpace == "Site") {
      landLabel1 = sid()
    } else if(input$filterSpace == "Land cover") {
      landLabel1 = input$landCover
    }
    print("landLabel1")
    print(landLabel1)
    
    if(input$filterSpace2 == "Site") {
      landLabel2 = sid2()
    } else if(input$filterSpace2 == "Land cover") {
      landLabel2 = input$landCover2
    }
    print("landLabel2")
    print(landLabel2)
    
    title = paste0("Temperatures on ", input$month, " ", input$day, ", ", input$year)
    
    completeDayPlot(dat1 = convertDat1, dat2 = convertDat2, datType1 = input$filterSpace, datType2 = input$filterSpace2,
                    title = title, landLabel1 = landLabel1, landLabel2 = landLabel2, yLabel = yLabel, compare = TRUE)
    
  })


# output$monthFig <- renderPlotly({
#   
#   req(input$filterTime == "Month")
#   req(input$month)
#   deg = input$tempLabel
#   
#   if(spaceFilter() == "site") {
#     #print("inside site month plot")
#     #print(head(sidData()))
#     sidDat <- siteMonthData(sidData(), yearSelect = input$year, monthSelect = match(input$month, month.abb))
#     # sidDat <- sidDat %>% mutate_if(is.character, as.factor)
#     # print(summary(sidDat))
#     title <- paste("Temperatures at", sid(), "in", input$month, input$year)
#     #print(title)
#     p <- monthPlot(sidDat, title, degree = deg)
#   } else {
#     title <- paste("Temperatures in", input$landCover, "areas in", input$month, input$year)
#     landCoverDat <- dat %>% filter(cat == input$landCover, year == input$year, month == match(input$month, month.abb))
#     print(head(landCoverDat))
#     p <- monthPlot(landCoverDat, title, degree = deg)
#   }
#   
#   return(p)
#   
# })

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

# clear--------------------
observeEvent(input$clear, {
  
  rv$dat1 <- NULL
  rv$dat2 <- NULL
  
  
})


}
