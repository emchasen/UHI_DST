#install libraries
library(shiny)
library(tidyverse)
library(sf)
library(leaflet)
library(plotly)
library(terra)
library(lubridate)


# load data
sites <- readxl::read_excel("data/WSC_Station_locations-06-03-2024.xlsx", sheet = 2) %>%
  mutate(categoryString = case_when(is.na(category2) & is.na(category3) ~ paste(cat, category1, sep = ", "),
                                    is.na(category3) ~ paste(cat, category1, category2, sep = ", "),
                                    TRUE ~ paste(cat, category1, category2, category3, sep = ", ")))

dat <- read_csv("data/partialDataLongDate.csv.gz") %>%
  left_join(sites)

days <- read_csv("data/days.csv")

# # make a sample dat
# datSample <- dat %>%
#   group_by(sid, year) %>%
#   slice_sample(n = 250) 
# 
# write_csv(datSample, "data/sampleDat.csv.gz")

sites_sf <- sites %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = F) 

landCover <- rast("data/daneCountyLandCover.tif")



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
    # addLayersControl(overlayGroups = "landCover",
    #                  options = layersControlOptions(collapsed = FALSE)) %>%
    addProviderTiles("USGS.USImageryTopo") # %>%
    # addRasterImage(landCover, group = "landCover",
    #                colors = c("#b50101", "#e8d1d2",  "#cb9147", "darkgreen", "skyblue1", "steelblue3", "wheat")) %>%
    # addLegend("bottomright", pal = palCover, values = c("Urban", "Suburban", "Rural/Ag", "Forest", "Open water", "Wetland", "Barren/shrubland"),
    #           title = "Land cover",
    #           layerId = "landLegend",
    #           #labFormat = labelFormat(prefix = "$"),
    #           opacity = 1) 
}

palSite <- colorFactor(palette = c("#cb9147", "#e8d1d2", "#b50101"), domain = sites$cat)
palCover <- colorFactor(palette = c("#b50101", "#e8d1d2",  "#cb9147", "darkgreen", "skyblue1", "steelblue3", "wheat"),
                        levels = c("Urban", "Suburban", "Rural/Ag", "Forest", "Open water", "Wetland", "Barren/shrubland"))

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

yearPlot <- function(dat, title) {
  
  dat <- dat %>%
    mutate(monthName = month.abb[month]) 
  
  dat$monthName = fct_relevel(dat$monthName, "Jan", "Feb", "Mar", "Apr", "May",
                              "Jun", "Jul", "Aug", "Sep", "Oct",
                              "Nov", "Dec")
  
  #title = paste0("Temperatures at ", dat$sid, " in ", dat$year, ": ", dat$categoryString)
  
  ggplot(data = dat, aes(x = monthName, y = tempC)) +
    geom_boxplot(fill="slateblue", alpha=0.5) +
    geom_jitter(color="black", size=0.4, alpha=0.4) +
    xlab("Month") +
    ylab("Temp (C)") +
    ggtitle(title)
  
}

monthPlot <- function(dat, title) {
  
  dat <- dat %>%
    group_by(day) %>%
    summarise(meanTemp = mean(tempC, na.rm = TRUE),
              minTemp = min(tempC, na.rm = TRUE),
              maxTemp = max(tempC, na.rm = TRUE))
  
  print(summary(dat))
  
  ggplot(data = dat, aes(x = day, y = meanTemp)) +
    geom_point() +
    geom_line() +
    geom_errorbar(aes(ymin = minTemp, ymax = maxTemp, alpha = 0.5, width = 0.5)) +
    xlab("Day of month") +
    ylab("Min, mean and max temp (C)") + 
    ggtitle(title) +
    theme(legend.position="none")
}

dayPlot <- function(dat, title, datType) {
  
  if(datType == "site") {
    p <- ggplot(data = dat, aes(x = Time1, y = tempC)) +
      geom_point() +
      geom_line() +
      #geom_errorbar(aes(ymin = minTemp, ymax = maxTemp, alpha = 0.5, width = 0.5)) +
      xlab("Time of day") +
      ylab("Temp (C)") + 
      ggtitle(title) 
  } else if(datType == "landcover") { 
    
    print("inside land cover day plot")
    dat <- dat %>%
      group_by(Time1) %>%
      summarise(meanTemp = mean(tempC, na.rm = TRUE),
                sdTemp = sd(tempC, na.rm = TRUE),
                count = n(),
                seTemp = sdTemp/sqrt(count))
    
    print(summary(dat))
    
    p <- ggplot(data = dat, aes(x = Time1, y = meanTemp)) +
      geom_point() +
      geom_line() +
      geom_errorbar(aes(ymin = meanTemp - seTemp, ymax = meanTemp + seTemp)) +
      xlab("Time of day") +
      ylab("Mean temp (C) +/- se") +
      ggtitle(title)
  }
  
  return(p)
  
}

