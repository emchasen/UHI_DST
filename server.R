server <- function(input, output, session) {
  
  # reactiveVal for the map object, and corresponding output object.
  react_map <- reactiveVal(base_map())
  
  # map-------------------------
  output$map <- renderLeaflet({
    react_map()
  })
  
  # view map layers based on user selection--------------------
  observe({
    leafletProxy("map") %>%
      clearImages() %>%
      clearControls()
      
    if("Off" %in% input$cover) {
    #if ("None" %in% input$layers) {
      leafletProxy("map") 
    }
    
    if ("Site" %in% input$filterSpace) {
      leafletProxy("map") %>%
        addLegend("bottomleft", pal = palSite, values = c("Urban", "Suburban", "Rural"),
                  title = "Site land cover",
                  opacity = 1)
    }
    
    if ("On" %in% input$cover) {
      leafletProxy("map") %>%
        addRasterImage(landCover, group = "landCover",
                       colors = c("#b50101", "#e8d1d2",  "#cb9147", "darkgreen", "skyblue1", "steelblue3", "wheat")) %>%
        addLegend("bottomright", pal = palCover, values = c("Urban", "Suburban", "Rural", "Forest", "Open water", "Wetland", "Barren/shrubland"),
                  title = "Land cover",
                  opacity = 1)
    }
    
    if ("Jan. '23 day" %in% input$uhi2023) {
      addHeatLayer(dat = jan_day)
    }
    
    if ("Jan. '23 night" %in% input$uhi2023) {
      addHeatLayer(dat = jan_night)
    }
    
    if ("Feb. '23 day" %in% input$uhi2023) {
      addHeatLayer(dat = feb_day)
    }
    
    if ("Feb. '23 night" %in% input$uhi2023) {
      addHeatLayer(feb_night)
    }
    
    if ("Mar. '23 day" %in% input$uhi2023) {
      addHeatLayer(mar_day)
    }
    
    if ("Mar. '23 night" %in% input$uhi2023) {
      addHeatLayer(mar_night)
    }
    
    if ("Apr. '23 day" %in% input$uhi2023) {
      addHeatLayer(apr_day)
    }
    
    if ("Apr. '23 night" %in% input$uhi2023) {
      addHeatLayer(apr_night)
    }
    
    if ("May '23 day" %in% input$uhi2023) {
      addHeatLayer(may_day)
    }
    
    if ("May '23 night" %in% input$uhi2023) {
      addHeatLayer(may_night)
    }
    
    if ("Jun. '23 day" %in% input$uhi2023) {
      addHeatLayer(jun_day)
    }
    
    if ("Jun. '23 night" %in% input$uhi2023) {
      addHeatLayer(jun_night)
    }
    
    if ("Jul. '23 day" %in% input$uhi2023) {
      addHeatLayer(jul_day)
    }
    
    if ("Jul. '23 night" %in% input$uhi2023) {
      addHeatLayer(jul_night)
    }
    
    if ("Aug. '23 day" %in% input$uhi2023) {
      addHeatLayer(aug_day)
    }
    
    if ("Aug. '23 night" %in% input$uhi2023) {
      addHeatLayer(aug_night)
    }
    
    if ("Sep. '23 day" %in% input$uhi2023) {
      addHeatLayer(sep_day)
    }
    
    if ("Sep. '23 night" %in% input$uhi2023) {
      addHeatLayer(sep_night)
    }
    
    if ("Oct. '23 day" %in% input$uhi2023) {
      addHeatLayer(oct_day)
    }
    
    if ("Oct. '23 night" %in% input$uhi2023) {
      addHeatLayer(oct_night)
    }
    
    if ("Nov. '23 day" %in% input$uhi2023) {
      addHeatLayer(nov_day)
    }
    
    if ("Nov. '23 night" %in% input$uhi2023) {
      addHeatLayer(nov_night)
    }
    
    if ("Dec. '23 day" %in% input$uhi2023) {
      addHeatLayer(dec_day)
    }
    
    if ("Dec. '23 night" %in% input$uhi2023) {
      addHeatLayer(dec_night)
    }
    
    if ("Jul. '22 day" %in% input$uhiJuly) {
      addHeatLayer(day22)
    }
    
    if ("Jul. '22 night" %in% input$uhiJuly) {
      addHeatLayer(night22)
    }
    
    if ("Jul. '21 day" %in% input$uhiJuly) {
      addHeatLayer(day21)
    }
    
    if ("Jul. '21 night" %in% input$uhiJuly) {
      addHeatLayer(night21)
    }
    
    if ("Jul. '20 day" %in% input$uhiJuly) {
      addHeatLayer(day20)
    }
    
    if ("Jul. '20 night" %in% input$uhiJuly) {
      addHeatLayer(night20)
    }
    
    if ("Jul. '19 day" %in% input$uhiJuly) {
      addHeatLayer(day19)
    }
    
    if ("Jul. '19 night" %in% input$uhiJuly) {
      addHeatLayer(night19)
    }
    
    if ("Jul. '18 day" %in% input$uhiJuly) {
      addHeatLayer(day18)
    }
    
    if ("Jul. '18 night" %in% input$uhiJuly) {
      addHeatLayer(night18)
    }
    
    if ("Jul. '17 day" %in% input$uhiJuly) {
      addHeatLayer(day17)
    }
    
    if ("Jul. '17 night" %in% input$uhiJuly) {
      addHeatLayer(night17)
    }
    
    if ("Jul. '16 day" %in% input$uhiJuly) {
      addHeatLayer(day16)
    }
    
    if ("Jul. '16 night" %in% input$uhiJuly) {
      addHeatLayer(night16)
    }
    
    if ("Jul. '15 day" %in% input$uhiJuly) {
      addHeatLayer(day15)
    }
    
    if ("Jul. '15 night" %in% input$uhiJuly) {
      addHeatLayer(night15)
    }
    
    if ("Jul. '14 day" %in% input$uhiJuly) {
      addHeatLayer(day14)
    }
    
    if ("Jul. '14 night" %in% input$uhiJuly) {
      addHeatLayer(night14)
    }
    
    if ("Jul. '13 day" %in% input$uhiJuly) {
      addHeatLayer(day13)
    }
    
    if ("Jul. '13 night" %in% input$uhiJuly) {
      addHeatLayer(night13)
    }
    
    if ("Jul. '12 day" %in% input$uhiJuly) {
      addHeatLayer(day12)
    }
    
    if ("Jul. '12 night" %in% input$uhiJuly) {
      addHeatLayer(night12)
    }
    
  })
  
  # if one of the above layers is on, the others should not-------------
  observeEvent(input$cover, {
    
    if(input$cover == "On") {
      updateSelectInput(inputId = "uhi2024", selected = "None")
      updateSelectInput(inputId = "uhiJuly", selected = "None")
    }
    
  })
  
  observeEvent(input$uhi2024, {
    
    if(input$uhi2024 != "None") {
      updateSelectInput(inputId = "cover", selected = "Off")
      updateSelectInput(inputId = "uhiJuly", selected = "None")
    }
    
  })
  
  observeEvent(input$uhiJuly, {
    
    if(input$uhiJuly != "None") {
      updateSelectInput(inputId = "cover", selected = "Off")
      updateSelectInput(inputId = "uhi2024", selected = "None")
    }
    
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
          # clearImages() %>%
          # clearControls() %>%
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
       updateDateRangeInput(inputId = "dateRangeSelect", label = "Date range", min = "2012-04-01", max = "2023-12-31",
                            start = "2023-01-01", end = "2023-12-31")
       #updateSelectInput(inputId = "filterSpace2", selected = " ")
       
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
       updateDateRangeInput(inputId = "dateRangeSelect", label = "Date range", min = "2012-04-01", max = "2023-12-31",
                            start = "2023-01-01", end = "2023-12-31")
       #updateSelectInput(inputId = "filterSpace2", selected = " ")

     }
     
   })
  
   
  # create sid-------------------
  observeEvent(input$map_marker_click, {
    
    if(!isTruthy(rv$dat1)) { ##TODO how else can I define this? 
    #if(is.null(input$filterSpace2)) {
      #print(input$filterSpace2)
      #print(is.null(input$filterSpace2))
      click <- input$map_marker_click
      sid(click$id)
      # lat <- as.numeric(click$lat)
      # lon <- as.numeric(click$lng)
      # 
      #return sidData needed to filter time
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
    #req(input$filterTime != " ")
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
      dateRangeInput(inputId = "dateRangeSelect", label = "Date range", min = "2012-04-01", max = "2023-12-31",
                     start = "2023-01-01", end = "2023-12-31"),
      helpText("Select date range less than one month")
    )
    
  })
  
  #add selection UI------------------
  output$addSelectionUI <- renderUI({

    wellPanel(
      tagList(
        fluidRow(
          column(6,
                 selectInput(inputId = "filterSpace2", label = "Filter comparison data by:", choices = c(" ", "Site", "Land cover"), selected = " "),
                 #uiOutput("filterSpace2UI"),
                 uiOutput("siteSelect2UI"),
                 uiOutput("landCoverSelect2UI")),
          column(6,
                 #checkboxInput(inputId = "addSelect", label = "Compare additional location(s)", value = FALSE),
                 #br(),
                 downloadButton(outputId = "download", label = "Download data"),
                 br(),
                 br(),
                 actionButton("clear", "Clear selection(s)"))
        ),
        uiOutput("sliderValsUI")
      )
    )

  })

  # output$filterSpace2UI <- renderUI({
  #   
  #   req(input$addSelect)
  #   selectInput(inputId = "filterSpace2", label = "Filter comparison data by:", choices = c(" ", "Site", "Land cover"), selected = " ")
  #   
  # })
  
  output$siteSelect2UI <- renderUI({

    req(input$filterSpace2 == "Site")
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
      sid2(NULL)

    } else if(input$filterSpace2 == "Land cover") {
      
      leafletProxy("map") %>%
        clearGroup("siteData")
      
      cover2(NULL)
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
  
  
  
  # create data-----------------
  rv <- reactiveValues(
    
    dat1 = NULL,
    dat2 = NULL
    
  )
  
  ## dat1 ---------------
  ###year----------
  observeEvent(input$year, {
  #observeEvent(input$filterData, {
    req(input$filterTime == "Year")
    req(input$year)
    print("creating year dat 1")
    
    if(input$filterSpace == "Site") {
      landLabel1 = sid()
    } else if(input$filterSpace == "Land cover") {
      landLabel1 = input$landCover
    }

    withProgress(message = "Creating data", value = 0, detail = "0%", {
      rv$dat1$year <- createSiteYearData(siteType = input$filterSpace, dat = dat, yearSelect = input$year, landLabel = landLabel1)
    })
    
    print(head(rv$dat1$year))
    
  })
  
  ###month--------------
  observeEvent(ignoreInit = TRUE, input$month, {
    
    req(input$filterTime == "Month")
    req(input$month)
    print("creating month dat 1")
    
    if(input$filterSpace == "Site") {
      landLabel1 = sid()
    } else if(input$filterSpace == "Land cover") {
      landLabel1 = input$landCover
    }
    
    withProgress(message = "Creating data", value = 0, detail = "0%", {
      month = which(input$month == month.abb)[[1]]
      rv$dat1$month <- createSiteMonthData(siteType = input$filterSpace, dat = dat, yearSelect = input$year, monthSelect = month, landLabel = landLabel1)
    })
    
    print(head(rv$dat1$month))
  })
  
  ###day-----------
  observeEvent(ignoreInit = TRUE, input$day, {
    
    print("creating day dat 1")
    req(input$filterTime == "Day")
    req(input$day)
    
    if(input$filterSpace == "Site") {
      landLabel1 = sid()
    } else if(input$filterSpace == "Land cover") {
      landLabel1 = input$landCover
    }
    
    withProgress(message = "Creating data", value = 0, detail = "0%", {
      month = which(input$month == month.abb)[[1]]
      rv$dat1$day <- createSiteDayData(siteType = input$filterSpace, dat = dat, yearSelect = input$year, monthSelect = month, daySelect = input$day, landLabel = landLabel1)
    })
    
    print(head(rv$dat1$day))
    return(rv$dat1$dat)
    
  })
      
  ###range----------
  observeEvent(ignoreInit = TRUE, input$dateRangeSelect, {
    
    print("creating range dat 1")
    req(input$filterTime == "Date range")
    
    if(input$filterSpace == "Site") {
      landLabel1 = sid()
    } else if(input$filterSpace == "Land cover") {
      landLabel1 = input$landCover
    }
    
    validate(
      need(difftime(input$dateRangeSelect[2], input$dateRangeSelect[1], "days") < 31, "Date range is greater than one month")
      )
    validate(
      need(difftime(input$dateRangeSelect[2], input$dateRangeSelect[1], "days") >1, "End date is before start date")
      )
    
    withProgress(message = "Creating data", value = 0, detail = "0%", {
      rv$dat1$range <- createDateRangeData(siteType = input$filterSpace, dat = dat, startDate = input$dateRangeSelect[1],
                                     endDate = input$dateRangeSelect[2], landLabel = landLabel1)
    })
    
    print(head(rv$dat1$range))
    return(rv$dat1$range)
    
  })
    
  
  ##dat2-----------------
  #observeEvent(ignoreInit = TRUE, list(input$year, input$month, input$day, input$dateRangeSelect, rv$dat1, input$landCover2, sid2()), {
  ###year-------------------
  observeEvent(ignoreInit = TRUE, list(input$year, sid2(), input$landCover2), {
    
    #req(rv$dat1$year)
    req(input$year)
    req(isTruthy(sid2()) || isTruthy(cover2()))
    #req(isTruthy(sid2()) || isTruthy(cover2())) ##TODO play around with the requirements for dat2
    req(input$filterTime == "Year")
    req(input$filterSpace2 != " ") ##TODO play around with the requirements for dat2
    
    print("creating year dat2")
    print(input$filterSpace2)
    ##TODO why is input$filterSpace2 getting cleared out?
    #print(input$filterSpace2)

      if(input$filterSpace2 == "Site") {
        landLabel2 <- sid2()
      } else if(input$filterSpace2 == "Land cover") {
        landLabel2 <- cover2()
      }

    withProgress(message = "Creating second data set", value = 0, detail = "0%", {
      rv$dat2$year <- createSiteYearData(siteType = input$filterSpace2, dat = dat, yearSelect = input$year, landLabel = landLabel2)
    })
    
    print(head(rv$dat2$year))
    return(rv$dat2$year)
    
  })
  
  ###month-------------
  observeEvent(ignoreInit = TRUE, list(input$month, sid2(), cover2()), {
    
    req(rv$dat1$month)
    req(input$month)
    req(isTruthy(sid2()) || isTruthy(cover2()))
    req(input$filterTime == "Month")
    req(input$filterSpace2 != " ")
    
    print("creating month dat2")
    print(input$filterSpace2)
    ##TODO why is input$filterSpace2 getting cleared out?
    #print(input$filterSpace2)
    
    if(input$filterSpace2 == "Site") {
      landLabel2 <- sid2()
    } else if(input$filterSpace2 == "Land cover") {
      landLabel2 <- cover2()
    }
    
    withProgress(message = "Creating second data set", value = 0, detail = "0%", {
      month = which(input$month == month.abb)[[1]]
      rv$dat2$month <- createSiteMonthData(siteType = input$filterSpace2, dat = dat, yearSelect = input$year, monthSelect = month, landLabel = landLabel2)
    })
    
    print(head(rv$dat2$month))
    return(rv$dat2$month)

  })
     
  ###day------------------   
  observeEvent(ignoreInit = TRUE, list(input$day, sid2(), cover2()), {
    
    req(rv$dat1$day)
    req(input$day)
    req(isTruthy(sid2()) || isTruthy(cover2()))
    req(input$filterTime == "Day")
    req(input$filterSpace2 != " ")
    
    print("creating day dat2")
    print(input$filterSpace2)
    ##TODO why is input$filterSpace2 getting cleared out?
    #print(input$filterSpace2)
    
    if(input$filterSpace2 == "Site") {
      landLabel2 <- sid2()
    } else if(input$filterSpace2 == "Land cover") {
      landLabel2 <- cover2()
    }
    
    withProgress(message = "Creating second data set", value = 0, detail = "0%", {
      month = which(input$month == month.abb)[[1]]
      rv$dat2$day <- createSiteDayData(siteType = input$filterSpace2, dat = dat, yearSelect = input$year, monthSelect = month, daySelect = input$day, landLabel = landLabel2)
    })
    
    print(head(rv$dat2$day))
    return(rv$dat2$dat)
    
  })
  
  ###range-----------
  observeEvent(ignoreInit = TRUE, list(input$dateRangeSelect, sid2(), cover2()), {
    
    req(rv$dat1$range)
    req(input$dateRangeSelect)
    req(isTruthy(sid2()) || isTruthy(cover2()))
    req(input$filterTime == "Date range")
    req(input$filterSpace2 != " ")
    
    print("creating range dat2")
    print(input$filterSpace2)
    ##TODO why is input$filterSpace2 getting cleared out?
    #print(input$filterSpace2)
    
    if(input$filterSpace2 == "Site") {
      landLabel2 <- sid2()
    } else if(input$filterSpace2 == "Land cover") {
      landLabel2 <- cover2()
    }
    
    req(input$dateRangeSelect)
    withProgress(message = "Creating second data set", value = 0, detail = "0%", {
      rv$dat2$range <- createDateRangeData(siteType = input$filterSpace2, dat = dat, startDate = input$dateRangeSelect[1],
                                     endDate = input$dateRangeSelect[2], landLabel = landLabel2)
    })
    
    print(head(rv$dat2$range))
    return(rv$dat2$range)
    
  })
  
  #plotUI---------------------------
  
  output$plotUI <- renderUI({
    
    #req(rv$dat1)
    req(input$filterTime)
    ##TODO with input$filterTime as the req, the second site stays selected as long as input$filterTime doesn't change
    ## If input$filterTime changes, select landCover2 needs to be hit twice to trigger data creation
   
    tagList(
      fluidRow(
        column(8,
               fluidRow(
                 column(6,
                        radioButtons("tempLabel", "Degree units display", choices = c("Celsius", "Fahrenheit"),
                                     selected = "Celsius", inline = TRUE))),
               if(input$filterTime == "Year") {
                 withSpinner(plotlyOutput("yearFig"), type = 8)
               } else if(input$filterTime == "Month") {
                 withSpinner(plotlyOutput("monthFig"), type = 8)
               } else if(input$filterTime == "Day") {
                 withSpinner(plotlyOutput("dayFig"), type = 8)
               } else if(input$filterTime == "Date range") {
                 withSpinner(plotlyOutput("rangeFig"), type = 8)
               },
               uiOutput("plotHelpTextUI")),
        column(4,
               uiOutput("addSelectionUI"))
      )
    )
    
  })
  
  # plot help text---------------
  output$plotHelpTextUI <- renderUI({
    
    req(input$filterTime == "Month")
    helpText("Click on legend items to remove data from plot.")
    
  })
  

  ### year plot --------
  output$yearFig <- renderPlotly({
      
    req(input$filterTime == "Year")
    req(input$year)
    dat1df <- req(rv$dat1$year)
    deg <- input$tempLabel
      
      if(is.null(rv$dat2$year)) {
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
        dat2df <- req(rv$dat2$year)
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
  
  ### month plot ---------
  output$monthFig <- renderPlotly({
    
    req(input$filterTime == "Month")
    req(input$month)
    dat1df <- req(rv$dat1$month)
    
    deg <- input$tempLabel
    
    if(is.null(rv$dat2$month)) {
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
      dat2df <- req(rv$dat2$month)
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
  
  ### day plot ---------
  output$dayFig <- renderPlotly({
    
    req(input$filterTime == "Day")
    req(input$day)
    print("dayFig")
    dat1df <- req(rv$dat1$day)
    
    deg <- input$tempLabel
    
    if(is.null(rv$dat2$day)) {
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
      dat2df <- req(rv$dat2$day)
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
    validate(
      need(difftime(input$dateRangeSelect[2], input$dateRangeSelect[1], "days") >1, "Make sure end date is after start date")
    )
    
    req(input$filterTime == "Date range")
    req(input$dateRangeSelect)
    dat1df <- req(rv$dat1$range)
    print("rangeFig")
    
    deg <- input$tempLabel
    date1 <- format(input$dateRangeSelect[1], "%m/%d/%Y")
    date2 <- format(input$dateRangeSelect[2], "%m/%d/%Y")
    
    if(is.null(rv$dat2$range)) {
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
        title = paste0("Temperatures at ", sid(), " from ", date1, " to ", date2)
        landLabel1 = sid() 
      } else if(input$filterSpace == "Land cover") {
        title = paste0("Temperatures in ", input$landCover, " areas from ", date1, " to ", date2)
        landLabel1 = input$landCover
      }
      
      rangePlot <- dateRangeCompletePlot(dat1 = convertDat1, title = title, landLabel1 = landLabel1, yLabel = yLabel, compare = FALSE)
      
    } else {
      #print("dat 2 is not null")
      dat2df <- req(rv$dat2$range)
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
      
      title = paste0("Temperatures from ", date1, " to ", date2)
      
      rangePlot <- dateRangeCompletePlot(dat1 = convertDat1, dat2 = convertDat2, title = title, 
                                         landLabel1 = landLabel1, landLabel2 = landLabel2, yLabel = yLabel, 
                                         compare = TRUE)
      
    }
    
    return(rangePlot)
    
    
  })
  
  # sliderVals ----------
  
  output$sliderValsUI <- renderUI({
    
    req(rv$dat2)
    
    tagList(
      h5("Use the slider below to view the difference in temperatures between your two locations at your selected time."),
      if(input$filterTime == "Year") {
        
        months <- unique(rv$dat2$year$monthName)
        
        sliderTextInput(
          inputId = "yearSlider",
          label = "Select month",
          choices = months,
          selected = months[1],
          grid = TRUE
        )
        
      } else if(input$filterTime == "Month") {
        
        dayChoices <- days %>% filter(year == input$year, month == input$month)
        dayChoices <- 1:dayChoices$day
        
        sliderInput(
          inputId = "monthSlider",
          label = "Select day",
          min = 1, max = max(dayChoices), value = 1, step = 1
        )
        
      } else if(input$filterTime == "Day") {
        
        sliderTextInput(
          inputId = "daySlider",
          label = "Select time",
          choices = time_labels,
          selected = "00:00"
        )
        
      } else if(input$filterTime == "Date range") {
        
        tagList(
          sliderInput(
            inputId = "rangeDaySlider",
            label = "Select day",
            min = input$dateRangeSelect[1],
            max = input$dateRangeSelect[2],
            value = input$dateRangeSelect[1]
          ),
          sliderTextInput(
            inputId = "rangeTimeSlider",
            label = "Select time",
            choices = time_labels,
            selected = "00:00"
          )
        )
      },
      uiOutput("diffTableUI1"),
      h5("View the full set of times and temperature differences below. Darker shades of red indicate a greater difference."),
      uiOutput("diffTableUI")
    )
  })
  
  # diff output table1---------------
  output$diffTableUI1 <- render_gt({
    
    req(rv$dat2)
    deg <- input$tempLabel
    
    if(deg == "Celsius") {
      label = "(°C)"
    } else if(deg == "Fahrenheit") {
      label = "(°F)"
    }
    
    if(input$filterTime == "Year") {
      
      if(deg == "Fahrenheit") {
        yearDat1 <- rv$dat1$year %>%
          mutate(temp = round(CtoF(tempC),1))
        yearDat2 <- rv$dat2$year %>%
          mutate(temp = round(CtoF(tempC),1))
      } else {
        yearDat1 <- rv$dat1$year %>%
          mutate(temp = round(tempC,1))
        yearDat2 <- rv$dat2$year %>%
          mutate(temp = round(tempC,1))
      }
      
      newDat <- datDif(yearDat1, yearDat2, "Year")
      
      newDat %>%
        filter(Month == input$yearSlider) %>%
        gt() %>%
        cols_label(
          minDif = paste("\u0394 Min temp", label),
          maxDif = paste("\u0394 Max temp", label)) 
      
    } else if(input$filterTime == "Month") {
      
      if(deg == "Celsius") {
        monthDat1 <- rv$dat1$month
        monthDat2 <- rv$dat2$month
      } else if(deg == "Fahrenheit") {
        monthDat1 <- rv$dat1$month %>%
          mutate(minTemp = round(CtoF(minTemp), 2),
                 maxTemp = round(CtoF(maxTemp), 2))
        monthDat2 <- rv$dat2$month %>%
          mutate(minTemp = round(CtoF(minTemp), 2),
                 maxTemp = round(CtoF(maxTemp), 2))
      }
      
      newDat <- datDif(monthDat1, monthDat2, "Month")
      
      newDat %>%
        filter(day == input$monthSlider) %>%
        gt() %>%
        cols_label(
          day = "Day of month",
          minDif = paste("\u0394 Min temp", label),
          maxDif = paste("\u0394 Max temp", label))
      
    } else if(input$filterTime == "Day") {
      
      if(deg == "Celsius") {
        dayDat1 <- rv$dat1$day
        dayDat2 <- rv$dat2$day
      } else if(deg == "Fahrenheit") {
        dayDat1 <- rv$dat1$day %>%
          mutate(meanTemp = round(CtoF(meanTemp),2))
        dayDat2 <- rv$dat2$day %>%
          mutate(meanTemp = round(CtoF(meanTemp),2))
      }
      
      newDat <- datDif(dayDat1, dayDat2, "Day")
  
      newDat %>%
        filter(Time == input$daySlider) %>%
        gt() %>%
        cols_label(
          meanDif = paste("\u0394 Mean temp", label)
        )
      
    } else if(input$filterTime == "Date range") {
      
      if(deg == "Celsius") {
        rangeDat1 <- rv$dat1$range 
        rangeDat2 <- rv$dat2$range 
      } else if(deg == "Fahrenheit") {
        rangeDat1 <- rv$dat1$range %>%
          mutate(meanTemp = round(CtoF(meanTemp),2))
        rangeDat2 <- rv$dat2$range %>%
          mutate(meanTemp = round(CtoF(meanTemp),2))
      }
      
      newDat <- datDif(rangeDat1, rangeDat2, "Date range") 

      dayTime <- as.POSIXct((paste(input$rangeDaySlider, input$rangeTimeSlider)), tz = "UTC")
      
      newDat %>%
        filter(DateTime == dayTime) %>%
        gt() %>%
        cols_label(
          DateTime = "Date Time",
          meanDif = paste("\u0394 Mean temp", label)
        )
    }
    
    
  })
    
  # diff Output table--------------
  output$diffTableUI <- render_gt({
    
    req(rv$dat2)
    deg <- input$tempLabel
    if(deg == "Celsius") {
      label = "(°C)"
    } else if(deg == "Fahrenheit") {
      label = "(°F)"
    }
    
    if(input$filterTime == "Year") {
      
      if(deg == "Fahrenheit") {
        yearDat1 <- rv$dat1$year %>%
          mutate(temp = round(CtoF(tempC),1))
        yearDat2 <- rv$dat2$year %>%
          mutate(temp = round(CtoF(tempC),1))
      } else {
        yearDat1 <- rv$dat1$year %>%
          mutate(temp = round(tempC,1))
        yearDat2 <- rv$dat2$year %>%
          mutate(temp = round(tempC,1))
      }
      
      newDat <- datDif(yearDat1, yearDat2, "Year")
      
      newDat %>%
        gt() %>%
        data_color(columns = c(minDif, maxDif), palette = "YlOrRd") %>%
        cols_label(
          minDif = paste("\u0394 Min temp", label),
          maxDif = paste("\u0394 Max temp", label)) 
      
    } else if(input$filterTime == "Month") {
      
      if(deg == "Celsius") {
        monthDat1 <- rv$dat1$month
        monthDat2 <- rv$dat2$month
      } else if(deg == "Fahrenheit") {
        monthDat1 <- rv$dat1$month %>%
          mutate(minTemp = round(CtoF(minTemp), 2),
                 maxTemp = round(CtoF(maxTemp), 2))
        monthDat2 <- rv$dat2$month %>%
          mutate(minTemp = round(CtoF(minTemp), 2),
                 maxTemp = round(CtoF(maxTemp), 2))
      }
      
      newDat <- datDif(monthDat1, monthDat2, "Month")
      
      newDat %>%
        gt() %>%
        data_color(columns = c(minDif, maxDif), palette = "YlOrRd") %>%
        cols_label(
          day = "Day of month",
          minDif = paste("\u0394 Min temp", label),
          maxDif = paste("\u0394 Max temp", label))
      
    } else if(input$filterTime == "Day") {
      
      if(deg == "Celsius") {
        dayDat1 <- rv$dat1$day
        dayDat2 <- rv$dat2$day
      } else if(deg == "Fahrenheit") {
        dayDat1 <- rv$dat1$day %>%
          mutate(meanTemp = round(CtoF(meanTemp),2))
        dayDat2 <- rv$dat2$day %>%
          mutate(meanTemp = round(CtoF(meanTemp),2))
      }
      
      newDat <- datDif(dayDat1, dayDat2, "Day")
      
      newDat %>%
        gt() %>%
        data_color(columns = c(meanDif), palette = "YlOrRd") %>%
        cols_label(
          meanDif = paste("\u0394 Mean temp", label)
        )
      
    } else if(input$filterTime == "Date range") {
      
      if(deg == "Celsius") {
        rangeDat1 <- rv$dat1$range 
        rangeDat2 <- rv$dat2$range 
      } else if(deg == "Fahrenheit") {
        rangeDat1 <- rv$dat1$range %>%
          mutate(meanTemp = round(CtoF(meanTemp),2))
        rangeDat2 <- rv$dat2$range %>%
          mutate(meanTemp = round(CtoF(meanTemp),2))
      }
      
      newDat <- datDif(rangeDat1, rangeDat2, "Date range") 
      
      newDat %>%
        gt() %>%
        data_color(columns = c(meanDif), palette = "YlOrRd") %>%
        cols_label(
          DateTime = "Date Time",
          meanDif = paste("\u0394 Mean temp", label)
        )
    }
    
  })
  
  # download data--------------
  output$download <- downloadHandler(
    
    filename = "UHI_Data_DegC.csv",
    
    content = function(file) {
      
      if(input$filterTime == "Year") { 
        df1 <- as.data.frame(rv$dat1$year)
        print(head(df1))
        df1 <- df1 %>%
          mutate(data = "set1") %>%
          rename(id = sid,
                 landcover = cat)
        
        if(is.null(rv$dat2$year)) {
          
          write.csv(df1, file, row.names = FALSE)
          
        } else {
          
          df2 <- as.data.frame(rv$dat2$year)
          df2 <- df2 %>%
            mutate(data = "set2") %>%
            rename(id = sid,
                   landcover = cat) %>%
            bind_rows(df1)
          
          write.csv(df2, file, row.names = FALSE)
        }
      } else if(input$filterTime == "Month") {
        
        df1 <- as.data.frame(rv$dat1$month)
        df1 <- df1 %>%
          mutate(data = "set1")
        
        if(is.null(rv$dat2$month)) {
          
          write.csv(df1, file, row.names = FALSE)
          
        } else {
          
          df2 <- as.data.frame(rv$dat2$month)
          df2 <- df2 %>%
            mutate(data = "set2") %>%
            bind_rows(df1)
          
          write.csv(df2, file, row.names = FALSE)
        }
        
      } else if(input$filterTime == "Day") {
        df1 <- as.data.frame(rv$dat1$day)
        df1 <- df1 %>%
          mutate(data = "set1")
        
        if(is.null(rv$dat2$day)) {
          
          write.csv(df1, file, row.names = FALSE)
          
        } else {
          
          df2 <- as.data.frame(rv$dat2$day)
          df2 <- df2 %>%
            mutate(data = "set2") %>%
            bind_rows(df1)
          
          write.csv(df2, file, row.names = FALSE)
        }
      } else if(input$filterTime == "Date range") {
        df1 <- as.data.frame(rv$dat1$range)
        df1 <- df1 %>%
          mutate(data = "set1")
        
        if(is.null(rv$dat2$range)) {
          
          write.csv(df1, file, row.names = FALSE)
          
        } else {
          
          df2 <- as.data.frame(rv$dat2$range)
          df2 <- df2 %>%
            mutate(data = "set2") %>%
            bind_rows(df1)
          
          write.csv(df2, file, row.names = FALSE)
        }
      }
    }
  )
  
  
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
    updateSelectInput(inputId = "landCover2", selected = " ")
    updateSelectInput(inputId = "year", selected = " ")
    updateSelectInput(inputId = "month", selected = " ")
    updateSelectInput(inputId = "day", selected = " ")
    updateSelectInput(inputId = "filterSpace2", selected = " ")
    updateDateRangeInput(inputId = "dateRangeSelect", label = "Date range", min = "2012-04-01", max = "2023-12-31",
                         start = "2023-01-01", end = "2023-12-31")
    updateCheckboxInput(inputId = "addSelect", value = FALSE)
    leafletProxy("map") %>%
      clearGroup("siteData") %>%
      setView(lat = 43.08, lng = -89.37, zoom = 10)
    
  
  })

}
