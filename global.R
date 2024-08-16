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

# dat <- read_csv("data/partialDataLongDate.csv.gz") %>%
#   left_join(sites)

dat <- read_csv("data/sampleDat.csv.gz") %>%
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
               group = "Sites") %>%
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
    addLayersControl(overlayGroups = c("Land Cover", "Sites"),
                     options = layersControlOptions(collapsed = TRUE))
}

palSite <- colorFactor(palette = c("#cb9147", "#e8d1d2", "#b50101"), domain = sites$cat)
palCover <- colorFactor(palette = c("#b50101", "#e8d1d2",  "#cb9147", "darkgreen", "skyblue1", "steelblue3", "wheat"),
                        levels = c("Urban", "Suburban", "Rural", "Forest", "Open water", "Wetland",  "Barren/shrubland"))

layers = c(landCover = "Land cover")

# create data frames----------------

siteData <- function(dat, site) {
  newDat <- dat %>%
    filter(sid == site) %>%
    drop_na(tempC)
  return(newDat)
}

createSiteYearData <- function(siteType, dat, yearSelect, landLabel) {
  
  # filter data and configure months
  if(siteType == "Site") {
    filteredDat <- dat %>% filter(sid == landLabel,
                          year == yearSelect) %>%
      mutate(monthName = month.abb[month])
    filteredDat$monthName = fct_relevel(filteredDat$monthName, "Jan", "Feb", "Mar", "Apr", "May",
                                 "Jun", "Jul", "Aug", "Sep", "Oct",
                                 "Nov", "Dec")
    #return(filteredDat)
  } else if(siteType == "Land cover") {
    filteredDat <- dat %>% filter(cat == landLabel,
                          year == yearSelect) %>%
      mutate(monthName = month.abb[month])
    filteredDat$monthName = fct_relevel(filteredDat$monthName, "Jan", "Feb", "Mar", "Apr", "May",
                                "Jun", "Jul", "Aug", "Sep", "Oct",
                                "Nov", "Dec")
  }
  
  filteredDat %>% drop_na(tempC)

}


createSiteMonthData <- function(siteType, dat, yearSelect, monthSelect, landLabel) {
  
  # filter data and configure months
  if(siteType == "Site") {
    filteredDat <- dat %>% 
      filter(sid == landLabel,
             year == yearSelect,
             month == monthSelect) 
    #return(filteredDat)
  } else if(siteType == "Land cover") {
    filteredDat <- dat %>% 
      filter(cat == landLabel,
             year == yearSelect,
             month == monthSelect) 
  }
  
  filteredDat %>% 
    group_by(day) %>%
    summarise(meanTemp = round(mean(tempC, na.rm = TRUE),2),
              minTemp = round(min(tempC, na.rm = TRUE),2),
              maxTemp = round(max(tempC, na.rm = TRUE),2)) 
  
}

createSiteDayData <- function(siteType, dat, yearSelect, monthSelect, daySelect, landLabel) {
  
  # filter data and configure months
  if(siteType == "Site") {
    filteredDat <- dat %>% 
      filter(sid == landLabel)
    #return(filteredDat)
  } else if(siteType == "Land cover") {
    filteredDat <- dat %>% 
      filter(cat == landLabel,
             year == yearSelect,
             month == monthSelect,
             day == daySelect) 
  }
  
  filteredDat <- filteredDat %>% 
    filter(year == yearSelect,
           month == monthSelect, 
           day == daySelect) %>%
    group_by(Time1) %>%
    summarise(meanTemp = mean(tempC, na.rm = TRUE),
              sdTemp = sd(tempC, na.rm = TRUE),
              count = n(),
              seTemp = sdTemp/sqrt(count))
    
  filteredDat$Time <- substr(as.POSIXct(sprintf("%04.0f", filteredDat$Time1), format='%H%M'), 12, 16)
  
  return(filteredDat)
  
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

yearPlotSingle <- function(dat, title, landLabel, yLabel) {
  
  p <- plot_ly(data = dat) %>%
    add_trace(y = ~temp, x = ~monthName, type = "box",
              color=I("slateblue"), name = landLabel) %>%
    layout(title = list(text = title, font = list(size = 20), y = 0.95),
           xaxis = list(title = list(text = "Month", font = list(size = 20))),
           yaxis = list(title = list(text = yLabel, font = list(size = 20))))
  
  return(p)
  
}

completeYearPlot <- function(dat1, dat2 = NULL, title, landLabel1, landLabel2, yLabel, compare) {
  
  base_plot <- yearPlotSingle(dat = dat1, title = title, landLabel = landLabel1, yLabel = yLabel)
  
  if(compare == FALSE) {
    plt <- base_plot
  } else if(compare == TRUE) {
    if(nrow(dat2) > 1) {
      plt <- base_plot %>%
        add_trace(data = dat2, y = ~ temp, x = ~ monthName, 
                  type = "box", name = landLabel2) %>%
        layout(boxmode = "group", hovermode = "x unified",
               title = list(text = title, font = list(size = 20), y = 0.95)) 
    } else if(nrow(dat2) < 1) {
      plt <- base_plot %>%
        layout(title = list(text = paste0(title, ": ", "<span style='color:red'>Choose another site
                                          (mising data)</span>")))
        
    }
    
  }
  
  return(plt)
  
}


monthPlotSingle <- function(dat, title, landLabel, yLabel) {
  
  p <- plot_ly(data = dat, x = ~day) %>%
    add_trace(y =~minTemp, type = 'scatter', mode = 'lines+markers', 
              hoverinfo = "text",
              name = paste(landLabel, "min temp"),
              line = list(color = "#6a5acd", dash = "dash"),
              marker = list(color = "#6a5acd"),
              hovertext = ~paste(landLabel, "min temp:", round(minTemp, 1))) %>%
    add_trace(y = ~meanTemp, type = 'scatter', mode = 'lines+markers', 
              hoverinfo = "text",
              name = paste(landLabel, "mean temp"),
              line = list(color = "#6a5acd", dash = "solid"),
              marker = list(color = "#6a5acd"),
              hovertext = ~paste(landLabel, "mean temp:", round(meanTemp, 1))) %>%
    add_trace(y = ~maxTemp, type = 'scatter', mode = 'lines+markers', 
              hoverinfo = "text",
              name = paste(landLabel, "max temp"),
              line = list(color = "#6a5acd", dash = "dot"),
              marker = list(color = "#6a5acd"),
              hovertext = ~paste(landLabel, "max temp:", round(maxTemp, 1))) %>%
    layout(title = list(text = title, font = list(size = 20), y = 0.95),
           xaxis = list(title = list(text = "Day of month", font = list(size = 20))),
           yaxis = list(title = list(text = yLabel, font = list(size = 20))),
           hovermode = "x unified")
  
  return(p)
  
}

completeMonthPlot <- function(dat1, dat2 = NULL, title, landLabel1, landLabel2, yLabel, compare) {
  
  base_plot <- monthPlotSingle(dat = dat1, title = title, landLabel = landLabel1, yLabel = yLabel)
  
  if(compare == FALSE) {
    plt <- base_plot
  } else if(compare == TRUE) {
    if(nrow(dat2) > 1) {
      dat2 <- dat2 %>%
        mutate(day = day + 0.2)
      plt <- base_plot %>%
        add_trace(data = dat2, y =~minTemp, x = ~day, type = 'scatter', mode = 'lines+markers', 
                  hoverinfo = "text", 
                  name = paste(landLabel2, "min temp"),
                  line = list(color = "#CA2C92", dash = "dash"),
                  marker = list(color = "#CA2C92"),
                  hovertext = ~paste(landLabel2, "min temp:", round(minTemp,1))) %>%
        add_trace(y = ~meanTemp, type = 'scatter', mode = 'lines+markers', 
                  hoverinfo = "text", 
                  name = paste(landLabel2, "mean temp"),
                  line = list(color = "#CA2C92", dash = "solid"),
                  marker = list(color = "#CA2C92"),
                  hovertext = ~paste(landLabel2, "mean temp:", round(meanTemp,1))) %>%
        add_trace(y = ~maxTemp, type = 'scatter', mode = 'lines+markers', 
                  hoverinfo = "text", 
                  name = paste(landLabel2, "max temp"),
                  line = list(color = "#CA2C92", dash = "dot"),
                  marker = list(color = "#CA2C92"),
                  hovertext = ~paste(landLabel2, "max temp:", round(maxTemp,1)))
    } else if(nrow(dat2) < 1) {
      plt <- base_plot %>%
        layout(title = list(text = paste0(title, ": ", "<span style='color:red'>Choose another site
                                          (mising data)</span>")))
      
    }
    
  }
  
  return(plt)
  
}


dayPlotSingle <- function(datType, dat, title, landLabel, yLabel) {

  if(datType == "Site") {
    p <- plot_ly(data = dat) %>%
      add_trace(x = ~Time, y = ~meanTemp, type = 'scatter', mode = 'lines+markers',
                name = landLabel,
                line = list(color = "#6a5acd"),
                marker = list(color = "#6a5acd"),
                hoverinfo = "text",
                #hovertext = ~paste("Temp:", round(meanTemp, 1))
                hovertext = ~ round(meanTemp, 1)) %>%
      layout(title = list(text = title, font = list(size = 20), y = 0.95),
             xaxis = list(title = list(text = "Time of day", font = list(size = 20))),
             yaxis = list(title = list(text = yLabel, font = list(size = 20))))
    
  } else if(datType == "Land cover") { 
    
    p <- plot_ly(data = dat) %>%
      add_trace(x = ~Time, y = ~meanTemp, name = landLabel,
                type = 'scatter', mode = 'lines+markers',
                line = list(color = "#6a5acd"),
                marker = list(color = "#6a5acd"),
                hoverinfo = "text",
                hovertext = ~paste("Mean temp:", round(meanTemp, 1)),
                error_y = ~list(array = c(seTemp),
                                color = "#6a5acd")) %>%
      layout(title = list(text = title, font = list(size = 20), y = 0.95),
             xaxis = list(title = list(text = "Time of day", font = list(size = 20))),
             yaxis = list(title = list(text = paste(yLabel, "+/- se"), font = list(size = 20))))
      
  }
  
  return(p)
  
}

completeDayPlot <- function(dat1, dat2 = NULL, datType1, datType2, title, landLabel1, landLabel2, yLabel, compare) {
  
  base_plot <- dayPlotSingle(dat = dat1, datType = datType1, title = title, landLabel = landLabel1, yLabel = yLabel)

  if(compare == FALSE) {
    plt <- base_plot
  } else if(compare == TRUE) {
    if(nrow(dat2) > 1) {
      ##TODO handle the different datTypes
      if(datType2 == "Site") {
        plt <- base_plot %>%
          add_trace(data = dat2, y = ~meanTemp, x = ~Time, name = landLabel2,
                    type = 'scatter', mode = 'lines+markers',
                    hoverinfo = "text",
                    hovertext = ~ round(meanTemp, 1))
      } else if(datType2 == "Land cover") {
        plt <- base_plot %>%
          add_trace(data = dat2, y = ~meanTemp, x = ~Time, name = landLabel2,
                    type = 'scatter', mode = 'lines+markers',
                    hoverinfo = "text",
                    hovertext = ~paste("Mean temp:", round(meanTemp, 1)),
                    error_y = ~list(array = c(seTemp)))
      }
    } else if(nrow(dat2) < 1) {
      plt <- base_plot %>%
        layout(title = list(text = paste0(title, ": ", "<span style='color:red'>Choose another site
                                          (mising data)</span>")))
    }
  }
  
  return(plt)
  
}

