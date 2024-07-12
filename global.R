#install libraries
library(shiny)
library(shinyjs)
library(tidyverse)
library(sf)
library(leaflet)
library(plotly)
library(terra)
#library(lubridate)
library(shinycssloaders)


# load data
sites <- readxl::read_excel("data/sensorAttributes.xlsx") %>%
  mutate(categoryString = case_when(is.na(category2) & is.na(category3) ~ paste(cat, category1, sep = ", "),
                                    is.na(category3) ~ paste(cat, category1, category2, sep = ", "),
                                    TRUE ~ paste(cat, category1, category2, category3, sep = ", ")))

dat <- read_csv("data/partialDataLongDate.csv.gz") %>%
  left_join(sites)

# dat <- read_csv("data/sampleDat.csv.gz") %>%
#   left_join(sites)

days <- read_csv("data/days.csv")

# # make a sample dat
# datSample <- dat %>%
#   group_by(sid, year) %>%
#   slice_sample(n = 250) 
# 
# write_csv(datSample, "data/sampleDat.csv.gz")

sites_sf <- sites %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = F) 

##TODO remake raster without barren/shrubland. Probably also include more counties
landCover <- rast("data/daneCountyLandCover.tif")

CtoF <- function(x) {
  F = (x * (9/5)) + 32
  return(F)
}


# map functions--------------------
base_map <- function() {
  leaflet() %>%
    addTiles() %>%
    addMapPane("sites", 450) %>%
    addMapPane("landCover", 423) %>%
    addCircles(data = sites_sf, 
               lng = ~lon, lat = ~lat,
               opacity = 0.7,
               color = "black",
               group = "circles") %>%
    setView(lat = 43.08, lng = -89.37, zoom = 10) %>%
    addProviderTiles("USGS.USImageryTopo")  %>%
    addRasterImage(landCover, group = "Land Cover",
                   colors = c("#b50101", "#e8d1d2",  "#cb9147", "darkgreen", "skyblue1", "steelblue3", "wheat")) %>%
    #colors = c("#b50101", "#e8d1d2",  "#cb9147", "darkgreen", "skyblue1", "steelblue3", "wheat")) %>%
    addLegend("bottomright", pal = palCover, values = c("Urban", "Suburban", "Rural", "Forest", "Open water", "Wetland", "Barren/shrubland"),
              #values = c("Urban", "Suburban", "Rural/Ag", "Forest", "Open water", "Wetland", "Barren/shrubland"),
              title = "Land cover",
              layerId = "Land Cover",
              #labFormat = labelFormat(prefix = "$"),
              opacity = 1) %>%
    addLayersControl(overlayGroups = "Land Cover",
                     options = layersControlOptions(collapsed = FALSE))
}

palSite <- colorFactor(palette = c("#cb9147", "#e8d1d2", "#b50101"), domain = sites$cat)
palCover <- colorFactor(palette = c("#b50101", "#e8d1d2",  "#cb9147", "darkgreen", "skyblue1", "steelblue3", "wheat"),
                        levels = c("Urban", "Suburban", "Rural", "Forest", "Open water", "Wetland",  "Barren/shrubland"))

layers = c(landCover = "Land cover")

# filter data----------------

siteData <- function(dat, site) {
  newDat <- dat %>%
    filter(sid == site)
  return(newDat)
} 

siteYearData <- function(dat, yearSelect) {
  newDat <- dat %>%
    filter(year == yearSelect)
  return(newDat)
}

siteMonthData <- function(dat, yearSelect, monthSelect) {
  newDat <- dat %>%
    filter(year == yearSelect,
           month == monthSelect)
  return(newDat)
}

siteDayData <- function(dat, yearSelect, monthSelect, daySelect) {
  newDat <- dat %>%
    filter(year == yearSelect,
           month == monthSelect,
           day == daySelect)
  return(newDat)
}

coverData <- function(dat, cover) {
  newDat <- dat %>%
    filter(cat == input$landCover)
  return(newDat)
}

# plot functions--------------

yearPlot <- function(dat, title, degree) {
  
  if(degree == "Celsius") {
    dat <- dat %>%
      mutate(monthName = month.abb[month],
             temp = round(tempC, 1))
    yTitle = paste0("Mean temp (°C)")
  } else if(degree == "Fahrenheit") {
    dat <- dat %>%
      mutate(monthName = month.abb[month],
             temp = round(CtoF(tempC), 1))
    yTitle = paste0("Mean temp (°F)")
  }
  
  
  dat$monthName = fct_relevel(dat$monthName, "Jan", "Feb", "Mar", "Apr", "May",
                              "Jun", "Jul", "Aug", "Sep", "Oct",
                              "Nov", "Dec")
  
   
  
  
  
  p <- plot_ly(data = dat) %>%
    add_trace(y = ~temp, x = ~monthName, type = "box",
              color=I("slateblue")) %>%
    layout(title = list(text = title, font = list(size = 20), y = 0.95),
           xaxis = list(title = list(text = "Month", font = list(size = 20))),
           yaxis = list(title = list(text = yTitle, font = list(size = 20))))
    
  
  return(p)
  
}

monthPlot <- function(dat, title, degree) {
  
  if(degree == "Celsius") {

    dat <- dat %>%
      group_by(day) %>%
      summarise(meanTemp = mean(tempC, na.rm = TRUE),
                minTemp = min(tempC, na.rm = TRUE),
                maxTemp = max(tempC, na.rm = TRUE))
    
    yTitle = "Mean temp (min and max) (°C)"
    
  } else if(degree == "Fahrenheit") {
    
    dat <- dat %>%
      group_by(day) %>%
      summarise(meanTemp = mean(CtoF(tempC), na.rm = TRUE),
                minTemp = min(CtoF(tempC), na.rm = TRUE),
                maxTemp = max(CtoF(tempC), na.rm = TRUE))
    
    yTitle = "Mean temp (min and max) (°F)"
  }
  
  
  p <- plot_ly(data = dat, x = ~day) %>%
    add_trace(y =~meanTemp, type = 'scatter', mode = 'lines+markers', 
              hoverinfo = "text", 
              line = list(color = "#000000"),
              marker = list(color = "#000000"),
              hovertext = ~paste("Max temp:", round(maxTemp, 1), "<br>",
                                 "Mean temp:", round(meanTemp, 1), "<br>",
                                 "Min temp:", round(minTemp,1)),
              error_y = ~list(array = c(maxTemp - meanTemp),
                              arrayminus = c(meanTemp - minTemp),
                              color = '#000000')) %>%
    layout(title = list(text = title, font = list(size = 20), y = 0.95),
           xaxis = list(title = list(text = "Day of month", font = list(size = 20))),
           yaxis = list(title = list(text = yTitle, font = list(size = 20))))
  
  return(p)
  
}

dayPlot <- function(dat, title, degree, datType) {
  
  dat$Time <- substr(as.POSIXct(sprintf("%04.0f", dat$Time1), format='%H%M'), 12, 16)
  
  if(degree == "Celsius") {
    
    dat <- dat %>%
      mutate(temp = tempC)
    
    yTitle = "Temp (°C)"
    
  } else if(degree == "Fahrenheit") {
    
    dat <- dat %>%
      mutate(temp = CtoF(tempC))
    
    yTitle = "Temp (°F)"
  }
  
  if(datType == "site") {
    p <- plot_ly(data = dat) %>%
      add_trace(x = ~Time, y = ~temp, type = 'scatter', mode = 'lines+markers',
                line = list(color = "#000000"),
                marker = list(color = "#000000")) %>%
      layout(title = list(text = title, font = list(size = 20), y = 0.95),
             xaxis = list(title = list(text = "Time of day", font = list(size = 20))),
             yaxis = list(title = list(text = yTitle, font = list(size = 20))))
    
  } else if(datType == "landcover") { 
    
    dat <- dat %>%
      group_by(Time) %>%
      summarise(meanTemp = mean(temp, na.rm = TRUE),
                sdTemp = sd(temp, na.rm = TRUE),
                count = n(),
                seTemp = sdTemp/sqrt(count))
    
    p <- plot_ly(data = dat) %>%
      add_trace(x = ~Time, y = ~meanTemp, 
                type = 'scatter', mode = 'lines+markers',
                line = list(color = "#000000"),
                marker = list(color = "#000000"),
                hoverinfo = "text",
                hovertext = ~paste("Mean temp:", round(meanTemp, 1)),
                error_y = ~list(array = c(seTemp),
                                color = '#000000')) %>%
      layout(title = list(text = title, font = list(size = 20), y = 0.95),
             xaxis = list(title = list(text = "Time of day", font = list(size = 20))),
             yaxis = list(title = list(text = paste(yTitle, "+/- se"), font = list(size = 20))))
      
  }
  
  return(p)
  
}

