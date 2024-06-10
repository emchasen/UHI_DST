server <- function(input, output, session) {
  
  # reactiveVal for the map object, and corresponding output object.
  react_map <- reactiveVal(base_map())
  
  # map-------------------------
  output$map <- renderLeaflet({
    react_map()
  })
  
   observeEvent(input$filterSpace, {
     
     if(input$filterSpace == "Site") {
       print("site")
       
       leafletProxy("map") %>%
         clearGroup("circles") %>%
         addCircleMarkers(data = sites,
                          lng = ~lon, lat = ~lat,
                          layerId = ~sid,
                          group = "sites",
                          #clusterOptions = markerClusterOptions(),
                          color = "black",
                          weight = 1,
                          opacity = 0.6,
                          fill = TRUE,
                          fillColor = ~palSite(cat),
                          fillOpacity = 0.6,
                          popup = ~categoryString) %>%
         addLegend("topleft", pal = palSite, values = sites$cat,
                   title = "Land groups",
                   layerId = "landLegend",
                   #labFormat = labelFormat(prefix = "$"),
                   opacity = 1)
     }# else if(input$filterSpace == "Land cover") {
       
     #   leafletProxy("map") %>%
     #     #clearGroup("sites") %>%
     #     clearGroup("circles") %>%
     #     #addSpinner() %>%
     #     #startSpinner(options = list("lines" = 7, "length" = 10)) %>%
     #     addRasterImage(landCover,
     #                    colors = c("#b50101", "#e8d1d2",  "#cb9147", "darkgreen", "skyblue1", "steelblue3", "wheat")) %>%
     #     addLegend("topleft", pal = palCover, values = c("Urban", "Suburban", "Rural/Ag", "Forest", "Open water", "Wetland", "Barren/shrubland"),
     #               title = "Land groups",
     #               layerId = "landLegend",
     #               #labFormat = labelFormat(prefix = "$"),
     #               opacity = 1) #%>%
     #     #stopSpinner() 
     # }
     
   })
  
  observeEvent(input$map_marker_click, {
    
    print(input$map_marker_click)
  })
  
  output$siteSelectUI <- renderUI({
    
    req(input$filterSpace != " ")
    
    #if(input$filterSpace == "Site") {
     # selectInput(inputId = "location", label = "Select site", choices = sites)
    #} else {
    if(input$filterSpace == "Land cover") {
      selectInput(inputId = "groups", label = "Select land cover", choices = c("Rural", "Suburban", "Urban"))
    }
    
  })
  
  output$timeSelectUI <- renderUI({
    
    req(input$filterTime != " ")
    
    if(input$filterTime == "Year") {
      selectInput(inputId = "year", label = "Select year", choices = seq(from = 2012, to = 2023, by = 1))
    } else if(input$filterTime == "Month") {
      dateInput(inputId = "month", label = "Select month", min = "2012-01", max = "2023-12", format = "yyyy-mm", startview = "year")
      #selectInput(inputId = "month", label = "Select month", choices = month.name)
    } else if(input$filterTime == "Day") {
      dateInput(inputId = "day", label = "Select date", min = "2012-01-01", max = "2023-12-31", startview = "year")
    }
  })
  

  # site_df <- reactive({
  #   
  #   print(input$location)
  #   dat %>%
  #     select(c(Time1, monthName, input$location))
  #   
  # })
  # 
  # output$plotUI <- renderUI({
  #   
  #   req(site_df())
  #   siteData <- site_df() %>%
  #     mutate(monthName = fct_relevel(monthName, 
  #                               "January", "February", "March", "April", 
  #                               "May", "June", "July", "August", "September", 
  #                               "October", "November", "December"))
  #     
  #   y_name <- input$location
  #   print(head(siteData))
  #   
  #   renderPlot({
  #     ggplot(siteData, aes(x = Time1, y = .data[[y_name]])) +
  #       geom_point(aes(color = monthName))
  #   })
  #  
  # })
  
}
