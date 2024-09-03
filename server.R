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
       
       rv$dat1 <- NULL
       rv$dat2 <- NULL
       sid(NULL)
       sid2(NULL)
       cover(NULL)
       cover2(NULL)
       updateSelectInput(inputId = "filterTime", selected = " ")
       updateSelectInput(inputId = "landCover", selected = " ")
       updateSelectInput(inputId = "year", selected = " ")
       updateSelectInput(inputId = "month", selected = " ")
       updateSelectInput(inputId = "day", selected = " ")
       updateSelectInput(inputId = "filterSpace2", selected = " ")
       
     } else if(input$filterSpace == "Land cover") {
       
       spaceFilter("landcover")

       leafletProxy("map") %>%
         clearGroup("siteData") 

       output$landCoverSelectUI <- renderUI({

         req(input$filterSpace == "Land cover")
         selectInput(inputId = "landCover", label = "Select land cover", choices = c(" ", "Rural", "Suburban", "Urban"))
         
       })
       
       rv$dat1 <- NULL
       rv$dat2 <- NULL
       sid(NULL)
       sid2(NULL)
       cover(NULL)
       cover2(NULL)
       updateSelectInput(inputId = "filterTime", selected = " ")
       updateSelectInput(inputId = "landCover", selected = " ")
       updateSelectInput(inputId = "year", selected = " ")
       updateSelectInput(inputId = "month", selected = " ")
       updateSelectInput(inputId = "day", selected = " ")
       updateSelectInput(inputId = "filterSpace2", selected = " ")

     }
     
   })
  
   
  # create sid-------------------
  observeEvent(input$map_marker_click, {
    
    if(!isTruthy(rv$dat1)) {  
      #print("inside istruthy")
      #print(input$addSelect)
      click <- input$map_marker_click
      sid(click$id)
      lat <- as.numeric(click$lat)
      lon <- as.numeric(click$lng)
      
      #return sidData needed to filter time
      sidDF <- siteData(dat, site = click$id)
      sidData(sidDF)
      # print(head(sidDF))
      # 
      # text = unique(sidData()$categoryString)
      # print(text)
      # print(lat)
      # 
      # leafletProxy("map") %>%
      #   clearGroup("cur_site") %>%
      #   addPopups(data = sidDF, 
      #             lat = ~lat, lng = ~lon, text,
      #             group = "cur_site")
      
    } #else {
      #print("outside istruthy")
      #print(input$addSelect)
    #}

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
      #dat1(NULL)
      rv$dat1 <- NULL
    }

  })
  

  # filterTimeUI--------------------
  output$filterTimeUI <- renderUI({
    
    req(isTruthy(cover()) || isTruthy(sid()))
    
    tagList(
      selectInput(inputId = "filterTime", label = "Filter data in time by:", choices = c(" ", "Year", "Month", "Day", "Date range")),
      uiOutput("timeSelectUI")
    )
    
  })
  
  #add selection UI------------------
  output$addSelectionUI <- renderUI({

    wellPanel(
      tagList(
        fluidRow(
          shinyjs::useShinyjs(),
          column(6,
                 downloadButton(outputId = "download"),
                 br(),
                 checkboxInput(inputId = "addSelect", label = "Compare additional location(s)", value = FALSE),
                 br(),
                 actionButton("clear", "Clear selection(s)")),
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

    req(input$filterSpace2 == "Site" & input$addSelect == TRUE)
    tagList(
      h5("Find second site on the map and click the site marker"),
      uiOutput("siteID2")
    )
  })
  
  output$siteID2 <- renderUI({
    
    req(sid2())
    textOutput("sideID2text")
    
  })
  
  output$sideID2text <- renderText({
    
    #description <- unique(sidData2()$categoryString)
    #paste0("Your site is ", sid2(), ": ", description) 
    paste0("Your second site is ", sid2()) 
    
  })
  
  ## update map for second site selection-----------------
  observeEvent(input$filterSpace2, {
    
    rv$dat2 <- NULL
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
        
      }, ignoreInit = TRUE)
      
      cover2(NULL)

    } else if(input$filterSpace2 == "Land cover") {
      
      leafletProxy("map") %>%
        clearGroup("siteData")
      
      sid2(NULL)
      
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
      isTruthy(sid()) || isTruthy(cover())
      )
    
    tagList(
      uiOutput("yearSelect"),
      uiOutput("monthSelect"),
      uiOutput("daySelect"),
      uiOutput("dateRange")
    )
   
  })
  
  output$yearSelect <- renderUI({
    
    req(input$filterTime == "Year" || input$filterTime == "Month" || input$filterTime == "Day")
    if(spaceFilter() == "site") {
      yearChoices <- unique(sidData()$year)
    } else {
      yearChoices <- c(2012:2023)
    }
    req(input$filterTime != " ")
    selectInput(inputId = "year", label = "Select year", choices = c("", sort(yearChoices, decreasing = TRUE)))
    
  })
  
  output$monthSelect <- renderUI({
    
    req(input$year)
    req(input$filterTime == "Month" || input$filterTime == "Day")
    if(spaceFilter() == "site") {
      monthChoices <- sidData() %>% filter(year == input$year)
      monthChoices <- month.abb[sort(unique(monthChoices$month))]
    } else {
      if(input$year == 2012) {
        monthChoices <- month.abb[4:12] 
      } else{
        monthChoices <- month.abb[1:12]
      }
    }
    
    selectInput(inputId = "month", label = "Select month", choices = c("", monthChoices))
  
  })
  
  output$daySelect <- renderUI({
    
    req(input$year)
    req(input$month)
    req(input$filterTime == "Day")
    if(spaceFilter() == "site") {
      dayChoices <- siteMonthData(sidData(), yearSelect = input$year, monthSelect = match(input$month, month.abb))
      dayChoices <- sort(unique(dayChoices$day))
    } else {
      dayChoices <- days %>% filter(year == input$year, month == input$month)
      dayChoices <- 1:dayChoices$day
    }
    
    selectInput(inputId = "day", label = "Select day", choices = c("", dayChoices))
    
  })
  
  output$dateRange <- renderUI ({
    
    req(input$filterTime == "Date range")
    
    tagList(
      dateRangeInput(inputId = "dateRangeSelect", label = "Date range", start = "2012-04-01", end = "2023-12-31"),
      helpText("Select date range less than one month")
    )
    
  })
  
  # create data-----------------
  rv <- reactiveValues(

    dat1 = NULL,
    dat2 = NULL

  )
  # make filterData button---------------
  # shinyjs::disable("filterData")
  # observeEvent(ignoreInit = TRUE, list(input$year, input$month, input$day, input$dateRangeSelect), {
  #   
  #   if(input$filterTime == "Year") {
  #     
  #     req(input$year)
  #     shinyjs::enable("filterData")
  #     
  #   } else if(input$filterTime == "Month") {
  #     
  #     req(input$month)
  #     shinyjs::enable("filterData")
  #     
  #   } else if(input$filterTime == "Day") {
  #     
  #     req(input$day)
  #     shinyjs::enable("filterData")
  #     
  #   } else if(input$filterTime == "Date range") {
  #     
  #     print("date range")
  #     validate(
  #       need(difftime(input$dateRangeSelect[2], input$dateRangeSelect[1], "days") < 31, "Date range is greater than one month"
  #       ))
  #     shinyjs::enable("filterData")
  #     
  #   }
  #   
  # })
  ## dat1---------------
  observeEvent(ignoreInit = TRUE, list(input$year, input$month, input$day, input$dateRangeSelect), {
  #observeEvent(ignoreInit = TRUE, input$filterData, {
    
    print("creating dat 1")
    
    if(input$filterSpace == "Site") {
      landLabel1 = sid()
    } else if(input$filterSpace == "Land cover") {
      landLabel1 = input$landCover
    }

    if(input$filterTime == "Year") {
      
      req(input$year)
      rv$dat1 <- createSiteYearData(siteType = input$filterSpace, dat = dat, yearSelect = input$year, landLabel = landLabel1)

    } else if(input$filterTime == "Month") {
      
      req(input$month)
      month = which(input$month == month.abb)[[1]]
      rv$dat1 <- createSiteMonthData(siteType = input$filterSpace, dat = dat, yearSelect = input$year, monthSelect = month, landLabel = landLabel1)
      
    } else if(input$filterTime == "Day") {
      
      req(input$day)
      month = which(input$month == month.abb)[[1]]
      rv$dat1 <- createSiteDayData(siteType = input$filterSpace, dat = dat, yearSelect = input$year, monthSelect = month, daySelect = input$day, landLabel = landLabel1)
      
    } else if(input$filterTime == "Date range") {
      
      req(input$dateRangeSelect)
      rv$dat1 <- createDateRangeData(siteType = input$filterSpace, dat = dat, startDate = input$dateRangeSelect[1],
                                     endDate = input$dateRangeSelect[2], landLabel = landLabel1)
    }
    
    #print("dat1")
    print(head(rv$dat1))
  
  })
  
  ##dat2-----------------
  
  observeEvent(ignoreInit = TRUE, list(input$year, rv$dat1, input$landCover2, sid2()), {
    
    req(
        isTruthy(rv$dat1),
        isTruthy(sid2()) || isTruthy(cover2())
      )
    print("creating dat2")
    #print(input$filterSpace2)

      if(input$filterSpace2 == "Site") {
        landLabel2 <- sid2()
      } else if(input$filterSpace2 == "Land cover") {
        landLabel2 <- cover2()
      }
    #print(landLabel2)
      
      if(input$filterTime == "Year") {
        
        rv$dat2 <- createSiteYearData(siteType = input$filterSpace2, dat = dat, yearSelect = input$year, landLabel = landLabel2)
        
      } else if(input$filterTime == "Month") {
        
        req(input$month)
        month = which(input$month == month.abb)[[1]]
        rv$dat2 <- createSiteMonthData(siteType = input$filterSpace2, dat = dat, yearSelect = input$year, monthSelect = month, landLabel = landLabel2)
        
      } else if(input$filterTime == "Day") {
        
        req(input$day)
        month = which(input$month == month.abb)[[1]]
        rv$dat2 <- createSiteDayData(siteType = input$filterSpace2, dat = dat, yearSelect = input$year, monthSelect = month, daySelect = input$day, landLabel = landLabel2)
        
      } else if(input$filterTime == "Date range") {
        
        req(input$dateRangeSelect)
        rv$dat2 <- createDateRangeData(siteType = input$filterSpace2, dat = dat, startDate = input$dateRangeSelect[1],
                                       endDate = input$dateRangeSelect[2], landLabel = landLabel2)
      }
    
    #print("dat2")
    print(head(rv$dat2))
  })
 
  
  #plotUI---------------------------
  
  output$plotUI <- renderUI({
    
    req(rv$dat1)
    print("plotUI")
   
    tagList(
      fluidRow(
        column(6,
               radioButtons("tempLabel", "Degree units display", choices = c("Celsius", "Fahrenheit"), selected = "Celsius", inline = TRUE))
        ),
      if(input$filterTime == "Year") {
        #uiOutput("yearFig")
        withSpinner(plotlyOutput("yearFig"), type = 8)
      } else if(input$filterTime == "Month") {
        #uiOutput("monthFig")
        withSpinner(plotlyOutput("monthFig"), type = 8)
      } else if(input$filterTime == "Day") {
        #uiOutput("dayFigure")
        withSpinner(plotlyOutput("dayFig"), type = 8)
      } else if(input$filterTime == "Date range") {
        print("plotly range fig")
        withSpinner(plotlyOutput("rangeFig"), type = 8)
      },
      br(),
      uiOutput("addSelectionUI")
    )
    
  })
  
  output$dateRangeText <- renderUI({
    
    req(input$filterTime == "Date range")
    
    print(input$dateRangeSelect[2])
    print(class(input$dateRangeSelect[2]))
    
    validate(
      need(difftime(input$dateRangeSelect[2], input$dateRangeSelect[1], "days") < 31, "Date range is greater than one month"
      ))
    
    #print(difftime(input$dateRangeSelect[2], input$dateRangeSelect[1], "days"))
    
  })
  

  ### year plot ---------
  output$yearFig <- renderPlotly({
    
      req(input$filterTime == "Year")
    req(input$year)
      req(rv$dat1)
      print("yearFig")
      dat1df <- rv$dat1
      
      deg <- req(input$tempLabel)
      
      if(is.null(rv$dat2)) {
        #print("dat 2 is null")
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
          title = paste("Temperatures at", sid(), "in", input$year)
          landLabel1 = sid()
        } else if(input$filterSpace == "Land cover") {
          title = paste("Temperatures in", input$landCover, "areas in", input$year)
          landLabel1 = input$landCover
        }
        
        yearPlot <- completeYearPlot(dat1 = convertDat1, title = title, landLabel1 = landLabel1, yLabel = yLabel, compare = FALSE)
      } else {
        #print("dat 2 is not null")
        dat2df <- req(rv$dat2)
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

          title = paste("Temperatures in", input$year)

          yearPlot <- completeYearPlot(dat1 = convertDat1, dat2 = convertDat2, title = title,
                           landLabel1 = landLabel1, landLabel2 = landLabel2, yLabel = yLabel, compare = TRUE)

      }
      
      return(yearPlot)
    
  })
  # output$yearFig <- renderUI({
  # 
  #   req(input$filterTime == "Year")
  #   req(rv$dat1)
  #   print("yearFig")
  # 
  #   if(isTruthy(sid2()) || isTruthy(cover2())) {
  #     withSpinner(plotlyOutput("compareDatYearFig"), type = 8)
  #   } else {
  #     withSpinner(plotlyOutput("singleDatYearFig"), type = 8)
  #   }
  # 
  # })
  # 
  # #output$yearFig <- renderPlotly({
  # output$singleDatYearFig <- renderPlotly({
  #    
  #   print("inside single year")
  #   dat1df <- req(rv$dat1)
  #   deg <- req(input$tempLabel)
  # 
  #   if(deg == "Celsius") {
  #     convertDat1 <- dat1df %>%
  #       mutate(temp = round(tempC,1))
  #     yLabel = "Mean temp (°C)"
  #   } else if(deg == "Fahrenheit") {
  #     yLabel = "Mean temp (°F)"
  #     convertDat1 <- dat1df %>%
  #       mutate(temp = round(CtoF(tempC),1))
  #   }
  # 
  #   if(input$filterSpace == "Site") {
  #     title = paste("Temperatures at", sid(), "in", input$year)
  #     landLabel1 = sid()
  #   } else if(input$filterSpace == "Land cover") {
  #     title = paste("Temperatures in", input$landCover, "areas in", input$year)
  #     landLabel1 = input$landCover
  #   }
  # 
  #   completeYearPlot(dat1 = convertDat1, title = title, landLabel1 = landLabel1, yLabel = yLabel, compare = FALSE)
  # 
  # })
  # 
  # output$compareDatYearFig <- renderPlotly({
  #   
  #   print("inside compare year")
  #   req(rv$dat1)
  #   dat1df <- rv$dat1
  #   print(head(dat1df))
  #   req(rv$dat2)
  #   dat2df <- rv$dat2
  #   print(head(dat2df))
  #   deg <- req(input$tempLabel) 
  #   
  #   if(deg == "Celsius") {
  #     convertDat1 <- dat1df %>%
  #       mutate(temp = round(tempC,1))
  #     convertDat2 <- dat2df %>%
  #       mutate(temp = round(tempC,1))
  #     yLabel = "Mean temp (°C)"
  #   } else if(deg == "Fahrenheit") {
  #     convertDat1 <- dat1df %>%
  #       mutate(temp = round(CtoF(tempC),1))
  #     convertDat2 <- dat2df %>%
  #       mutate(temp = round(CtoF(tempC),1))
  #     yLabel = "Mean temp (°F)"
  #   }
  #   
  #   if(input$filterSpace == "Site") {
  #     landLabel1 = sid() 
  #   } else if(input$filterSpace == "Land cover") {
  #     landLabel1 = input$landCover
  #   }
  #   
  #   if(input$filterSpace2 == "Site") {
  #     landLabel2 = sid2() 
  #   } else if(input$filterSpace2 == "Land cover") {
  #     landLabel2 = input$landCover2
  #   }
  #   
  #   title = paste("Temperatures in", input$year)
  #   
  #   completeYearPlot(dat1 = convertDat1, dat2 = convertDat2, title = title, 
  #                    landLabel1 = landLabel1, landLabel2 = landLabel2, yLabel = yLabel, compare = TRUE)
  #   
  # })
  
  ### month plot ---------
  output$monthFig <- renderPlotly({
    
    req(input$filterTime == "Month")
    req(input$month)
    req(rv$dat1)
    print("monthFig")
    dat1df <- rv$dat1
    
    deg <- req(input$tempLabel)
    
    if(is.null(rv$dat2)) {
      #print("dat 2 is null")
      if(deg == "Celsius") {
        convertDat1 <- dat1df 
        yLabel = "Temp (°C)"
      } else if(deg == "Fahrenheit") {
        yLabel = "Temp (°F)"
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
      
      monthPlot <- completeMonthPlot(dat1 = convertDat1, title = title, landLabel1 = landLabel1, yLabel = yLabel, compare = FALSE)
      
    } else {
      #print("dat 2 is not null")
      dat2df <- req(rv$dat2)
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
      
      monthPlot <- completeMonthPlot(dat1 = convertDat1, dat2 = convertDat2, title = title,
                        landLabel1 = landLabel1, landLabel2 = landLabel2, yLabel = yLabel, compare = TRUE)
      
    }
    
    return(monthPlot)
    
  })
  
  # output$monthFig <- renderUI({
  #   
  #   req(input$filterTime == "Month")
  #   print("monthFig")
  #   
  #   if(isTruthy(sid2()) || isTruthy(cover2())) {
  #     withSpinner(plotlyOutput("compareDatMonthFig"), type = 8)
  #   } else {
  #     withSpinner(plotlyOutput("singleDatMonthFig"), type = 8)
  #   }
  #   
  # })
  
  # output$singleDatMonthFig <- renderPlotly({
  #   
  #   dat1df <- req(rv$dat1)
  #   deg <- req(input$tempLabel) 
  #   
  #   if(deg == "Celsius") {
  #     convertDat1 <- dat1df 
  #     yLabel = "Temp (°C)"
  #   } else if(deg == "Fahrenheit") {
  #     yLabel = "Temp (°F)"
  #     convertDat1 <- dat1df %>%
  #       mutate(meanTemp = round(CtoF(meanTemp),2),
  #              minTemp = round(CtoF(minTemp), 2),
  #              maxTemp = round(CtoF(maxTemp), 2))
  #   }
  #   
  #   if(input$filterSpace == "Site") {
  #     title = paste0("Temperatures at ", sid(), " in ", input$month, ", ", input$year)
  #     landLabel1 = sid() 
  #   } else if(input$filterSpace == "Land cover") {
  #     title = paste0("Temperatures in ", input$landCover, " areas in ", input$month, ", ", input$year)
  #     landLabel1 = input$landCover
  #   }
  #   
  #   completeMonthPlot(dat1 = convertDat1, title = title, landLabel1 = landLabel1, yLabel = yLabel, compare = FALSE)
  #   
  # })
  # 
  # output$compareDatMonthFig <- renderPlotly({
  # 
  #   dat1df <- req(rv$dat1)
  #   dat2df <- req(rv$dat2)
  #   deg <- req(input$tempLabel)
  # 
  #   if(deg == "Celsius") {
  #     convertDat1 <- dat1df 
  #     convertDat2 <- dat2df 
  #     yLabel = "Mean temp (°C)"
  #   } else if(deg == "Fahrenheit") {
  #     convertDat1 <- dat1df %>%
  #       mutate(meanTemp = round(CtoF(meanTemp),2),
  #              minTemp = round(CtoF(minTemp), 2),
  #              maxTemp = round(CtoF(maxTemp), 2))
  #     convertDat2 <- dat2df %>%
  #       mutate(meanTemp = round(CtoF(meanTemp),2),
  #              minTemp = round(CtoF(minTemp), 2),
  #              maxTemp = round(CtoF(maxTemp), 2))
  #     yLabel = "Mean temp (°F)"
  #   }
  # 
  #   if(input$filterSpace == "Site") {
  #     landLabel1 = sid()
  #   } else if(input$filterSpace == "Land cover") {
  #     landLabel1 = input$landCover
  #   }
  # 
  #   if(input$filterSpace2 == "Site") {
  #     landLabel2 = sid2()
  #   } else if(input$filterSpace2 == "Land cover") {
  #     landLabel2 = input$landCover2
  #   }
  # 
  #   title = paste0("Temperatures in ", input$month, ", ", input$year)
  # 
  #   completeMonthPlot(dat1 = convertDat1, dat2 = convertDat2, title = title,
  #                    landLabel1 = landLabel1, landLabel2 = landLabel2, yLabel = yLabel, compare = TRUE)
  # 
  # })
  
  ### day plot ---------
  output$dayFig <- renderPlotly({
    
    req(input$filterTime == "Day")
    req(rv$dat1)
    req(input$day)
    print("dayFig")
    dat1df <- rv$dat1
    
    deg <- req(input$tempLabel)
    
    if(is.null(rv$dat2)) {
      #print("dat 2 is null")
      if(deg == "Celsius") {
        convertDat1 <- dat1df 
        yLabel = "Mean temp (°C)"
      } else if(deg == "Fahrenheit") {
        yLabel = "Mean temp (°F)"
        ##TODO how should I handle the se? I can't convert it with CtoF
        convertDat1 <- dat1df %>%
          mutate(meanTemp = round(CtoF(meanTemp),2))
      }
      
      if(input$filterSpace == "Site") {
        title = paste0("Temperatures at ", sid(), " on ", input$month, " ", input$day,", ", input$year)
        landLabel1 = sid() 
      } else if(input$filterSpace == "Land cover") {
        title = paste0("Temperatures in ", input$landCover, " areas on ", input$day, " ", input$month, ", ", input$year)
        landLabel1 = input$landCover
      }
      
      dayPlot <- completeDayPlot(dat1 = convertDat1, datType1 = input$filterSpace, title = title, landLabel1 = landLabel1, yLabel = yLabel, compare = FALSE)
      
    } else {
      #print("dat 2 is not null")
      dat2df <- req(rv$dat2)
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
      
      title = paste0("Temperatures on ", input$month, " ", input$day, ", ", input$year)
      
      dayPlot <- completeDayPlot(dat1 = convertDat1, dat2 = convertDat2, datType1 = input$filterSpace, datType2 = input$filterSpace2,
                      title = title, landLabel1 = landLabel1, landLabel2 = landLabel2, yLabel = yLabel, compare = TRUE)
      
    }
    
    return(dayPlot)
    
  })
  
  ## range plot------------
  output$rangeFig <- renderPlotly({
    
    validate(
      need(difftime(input$dateRangeSelect[2], input$dateRangeSelect[1], "days") < 31, "Date range is greater than one month"
      ))
    
    print(difftime(input$dateRangeSelect[2], input$dateRangeSelect[1], "days"))
    
    req(input$filterTime == "Date range")
    req(rv$dat1)
    print("rangeFig")
    dat1df <- rv$dat1
    
    deg <- req(input$tempLabel)
    
    if(is.null(rv$dat2)) {
      #print("dat 2 is null")
      if(deg == "Celsius") {
        convertDat1 <- dat1df 
        yLabel = "Temp (°C)"
      } else if(deg == "Fahrenheit") {
        yLabel = "Temp (°F)"
        ##TODO how should I handle the se? I can't convert it with CtoF
        convertDat1 <- dat1df %>%
          mutate(meanTemp = round(CtoF(meanTemp),2))
      }
      
      if(input$filterSpace == "Site") {
        title = paste0("Temperatures at ", sid(), " from ", input$dateRangeSelect[1], " to ", input$dateRangeSelect[2])
        landLabel1 = sid() 
      } else if(input$filterSpace == "Land cover") {
        title = paste0("Temperatures in ", input$landCover, " areas from ", input$dateRangeSelect[1], " to ", input$dateRangeSelect[2])
        landLabel1 = input$landCover
      }
      
      rangePlot <- dateRangeCompletePlot(dat1 = convertDat1, title = title, landLabel1 = landLabel1, yLabel = yLabel, compare = FALSE)
      
    } else {
      #print("dat 2 is not null")
      dat2df <- req(rv$dat2)
      if(deg == "Celsius") {
        convertDat1 <- dat1df 
        convertDat2 <- dat2df 
        yLabel = "Temp (°C)"
      } else if(deg == "Fahrenheit") {
        ##TODO how should I handle the se? I can't convert it with CtoF
        convertDat1 <- dat1df %>%
          mutate(meanTemp = round(CtoF(meanTemp),2))
        convertDat2 <- dat2df %>%
          mutate(meanTemp = round(CtoF(meanTemp),2))
        yLabel = "Temp (°F)"
      }
      
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
      
      title = paste0("Temperatures from ", input$dateRangeSelect[1], " to ", input$dateRangeSelect[2])
      
      rangePlot <- dateRangeCompletePlot(dat1 = convertDat1, dat2 = convertDat2, title = title, 
                                         landLabel1 = landLabel1, landLabel2 = landLabel2, yLabel = yLabel, 
                                         compare = TRUE)
      
    }
    
    return(rangePlot)
    
    
  })
  # output$dayFigure <- renderUI({
  #   
  #   req(input$filterTime == "Day")
  #   print("dayFig")
  #   
  #   if(isTruthy(sid2()) || isTruthy(cover2())) {
  #     withSpinner(plotlyOutput("compareDatDayFig"), type = 8)
  #   } else {
  #     withSpinner(plotlyOutput("singleDatDayFig"), type = 8)
  #   }
  #   
  # })
  
  # output$singleDatDayFig <- renderPlotly({
  #   
  #   dat1df <- req(dat1())
  #   dat1df <- req(rv$dat1)
  #   deg <- req(input$tempLabel)
  # 
  #   if(deg == "Celsius") {
  #     convertDat1 <- dat1df
  #     yLabel = "Mean temp (°C)"
  #   } else if(deg == "Fahrenheit") {
  #     yLabel = "Mean temp (°F)"
  #     ##TODO how should I handle the se? I can't convert it with CtoF
  #     convertDat1 <- dat1df %>%
  #       mutate(meanTemp = round(CtoF(meanTemp),2))
  #   }
  # 
  #   if(input$filterSpace == "Site") {
  #     title = paste0("Temperatures at ", sid(), " on ", input$month, " ", input$day,", ", input$year)
  #     landLabel1 = sid() 
  #   } else if(input$filterSpace == "Land cover") {
  #     title = paste0("Temperatures in ", input$landCover, " areas on ", input$day, " ", input$month, ", ", input$year)
  #     landLabel1 = input$landCover
  #   }
  #   
  #   completeDayPlot(dat1 = convertDat1, datType1 = input$filterSpace, title = title, landLabel1 = landLabel1, yLabel = yLabel, compare = FALSE)
  #   
  # })
  # 
  # output$compareDatDayFig <- renderPlotly({
  #   
  #   dat1df <- req(rv$dat1)
  #   dat2df <- req(rv$dat2)
  #   deg <- req(input$tempLabel)
  # 
  #   if(deg == "Celsius") {
  #     convertDat1 <- dat1df 
  #     convertDat2 <- dat2df 
  #     yLabel = "Mean temp (°C)"
  #   } else if(deg == "Fahrenheit") {
  #     ##TODO how should I handle the se? I can't convert it with CtoF
  #     convertDat1 <- dat1df %>%
  #       mutate(meanTemp = round(CtoF(meanTemp),2))
  #     convertDat2 <- dat2df %>%
  #       mutate(meanTemp = round(CtoF(meanTemp),2))
  #     yLabel = "Mean temp (°F)"
  #   }
  #   
  #   if(input$filterSpace == "Site") {
  #     landLabel1 = sid()
  #   } else if(input$filterSpace == "Land cover") {
  #     landLabel1 = input$landCover
  #   }
  #   
  #   if(input$filterSpace2 == "Site") {
  #     landLabel2 = sid2()
  #   } else if(input$filterSpace2 == "Land cover") {
  #     landLabel2 = input$landCover2
  #   }
  #   
  #   title = paste0("Temperatures on ", input$month, " ", input$day, ", ", input$year)
  #   
  #   completeDayPlot(dat1 = convertDat1, dat2 = convertDat2, datType1 = input$filterSpace, datType2 = input$filterSpace2,
  #                   title = title, landLabel1 = landLabel1, landLabel2 = landLabel2, yLabel = yLabel, compare = TRUE)
  #   
  # })

  # clear--------------------
  observeEvent(input$clear, {
    
    #session$reload()
    rv$dat1 <- NULL
    rv$dat2 <- NULL
    sid(NULL)
    sid2(NULL)
    cover(NULL)
    cover2(NULL)
    updateSelectInput(inputId = "filterSpace", selected = " ")
    updateSelectInput(inputId = "filterTime", selected = " ")
    updateSelectInput(inputId = "landCover", selected = " ")
    updateSelectInput(inputId = "year", selected = " ")
    updateSelectInput(inputId = "month", selected = " ")
    updateSelectInput(inputId = "day", selected = " ")
    updateSelectInput(inputId = "filterSpace2", selected = " ")
    updateCheckboxInput(inputId = "addSelect", value = FALSE)
    leafletProxy("map") %>%
      clearGroup("siteData") %>%
      setView(lat = 43.08, lng = -89.37, zoom = 10)
    
  
  })

}
