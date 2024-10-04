#install libraries
library(shiny)
library(shinyjs)
library(tidyverse)
library(sf)
library(leaflet)
library(plotly)
library(terra)
library(fst)
library(shinyjs)
library(shinycssloaders)
library(shinyWidgets)
library(gt)
#library(reactlog)

#reactlog::reactlog_enable()

# load data
sites <- readxl::read_excel("data/sensorAttributes.xlsx") %>%
  mutate(categoryString = case_when(is.na(category2) & is.na(category3) ~ paste(cat, category1, sep = ", "),
                                    is.na(category3) ~ paste(cat, category1, category2, sep = ", "),
                                    TRUE ~ paste(cat, category1, category2, category3, sep = ", ")))

# dat <- read_csv("data/partialDataLongDate.csv.gz") %>%
#   left_join(sites)

dat <- read_fst("data/fast_dat.fst") %>%
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
landCover <- rast("data/landCoverAgg.tif")
jan_day <- rast("data/january2023_daytime_degF.tif")
jan_night <- rast("data/january2023_nighttime_degF.tif")
feb_day <- rast("data/february2023_daytime_degF.tif")
feb_night <- rast("data/february2023_nighttime_degF.tif")
mar_night <- rast("data/march2023_nighttime_degF.tif")
mar_day <- rast("data/march2023_daytime_degF.tif")
apr_day <- rast("data/april2023_daytime_degF.tif")
apr_night <- rast("data/april2023_nighttime_degF.tif")
may_day <- rast("data/may2023_daytime_degF.tif")
may_night <- rast("data/may2023_nighttime_degF.tif")
jun_day <- rast("data/june2023_daytime_degF.tif")
jun_night <- rast("data/june2023_nighttime_degF.tif")
jul_day <- rast("data/july2023_daytime_degF.tif")
jul_night <- rast("data/july2023_nighttime_degF.tif")
aug_day <- rast("data/august2023_daytime_degF.tif")
aug_night <- rast("data/august2023_nighttime_degF.tif")
sep_day <- rast("data/september2023_daytime_degF.tif")
sep_night <- rast("data/september2023_nighttime_degF.tif")
oct_day <- rast("data/october2023_daytime_degF.tif")
oct_night <- rast("data/october2023_nighttime_degF.tif")
nov_day <- rast("data/november2023_daytime_degF.tif")
nov_night <- rast("data/november2023_nighttime_degF.tif")
dec_day <- rast("data/december2023_daytime_degF.tif")
dec_night <- rast("data/december2023_nighttime_degF.tif")

layerChoices <- c("None", "Land cover", "Jan. day", "Jan. night", "Feb. day", "Feb. night", "Mar. day", "Mar. night",
                  "Apr. day", "Apr. night", "May day", "May night", "Jun. day", "Jun. night",
                  "Jul. day", "Jul. night", "Aug. day", "Aug. night", "Sep. day", "Sep. night",
                  "Oct. day", "Oct. night", "Nov. day", "Nov. night", "Dec. day", "Dec. night")

CtoF <- function(x) {
  F = (x * (9/5)) + 32
  return(F)
}

time_labels = expand.grid(
  hours = 0:24,
  minutes = 0
) %>%
  mutate_at(1:2, ~ifelse(nchar(.) == 1, paste0('0', .), .)) %>%
  mutate(time_label = paste0(hours, ':', minutes)) %>%
  arrange(time_label) %>%
  pull(time_label)

# map functions--------------------
base_map <- function() {
  leaflet() %>%
    addTiles() %>%
    addMapPane("sites", 450) %>%
    addMapPane("landCover", 449) %>%
    addMapPane("temps", 430) %>%
    addCircles(data = sites_sf, 
               lng = ~lon, lat = ~lat,
               opacity = 0.7,
               color = "black",
               group = "Sites") %>%
    # addRasterImage(landCover, group = "Land Cover",
    #                colors = c("#b50101", "#e8d1d2",  "#cb9147", "darkgreen", "skyblue1", "steelblue3", "wheat")) %>%
    setView(lat = 43.08, lng = -89.37, zoom = 10) %>%
    # addLegend("bottomright", pal = palCover, values = c("Urban", "Suburban", "Rural", "Forest", "Open water", "Wetland", "Barren/shrubland"),
    #           title = "Land cover",
    #           layerId = "Land Cover",
    #           opacity = 1) %>%
    # hideGroup(c('Jan. 2023, avg. night temps', "Jan. 2023, avg. day temps",
    #             "Jul. 2023, avg. night temps", "Jul. 2023, avg. day temps")) %>%
    # addLayersControl(overlayGroups = c("Land Cover", "Sites"), 
    #                  options = layersControlOptions(collapsed = TRUE)) %>%
    addProviderTiles("USGS.USImageryTopo")  
}


palSite <- colorFactor(palette = c("#cb9147", "#e8d1d2", "#b50101"), domain = sites$cat)
palCover <- colorFactor(palette = c("#b50101", "#e8d1d2",  "#cb9147", "darkgreen", "skyblue1", "steelblue3", "wheat"),
                        levels = c("Urban", "Suburban", "Rural", "Forest", "Open water", "Wetland",  "Barren/shrubland"))
palTemp <- c("#191970", "#007BFF",  "#2E86C1","#FFEC8B", "#ff7f00", "#D25223", "#FF5733")

layers = c(landCover = "Land cover")

# raster layer functions
addHeatLayer <- function(dat) {
  
  leafletProxy("map") %>%
    addRasterImage(dat, group = "temps", colorNumeric(palTemp, values(dat),  na.color = "transparent")) %>%
    addLegend("bottomright", pal = colorNumeric(palTemp, values(dat),  na.color = "transparent"), 
              values = values(dat), title = "Avg. temps (°F)")
  
}

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
    incProgress(1/4, detail = "25%")
    filteredDat <- dat %>% filter(sid == landLabel,
                          year == yearSelect) %>%
      mutate(monthName = month.abb[month]) 
    incProgress(3/4, detail = "75%")
    filteredDat$monthName = fct_relevel(filteredDat$monthName, "Jan", "Feb", "Mar", "Apr", "May",
                                 "Jun", "Jul", "Aug", "Sep", "Oct",
                                 "Nov", "Dec")
  } else if(siteType == "Land cover") {
    incProgress(1/4, detail = "25%")
    filteredDat <- dat %>% filter(cat == landLabel,
                          year == yearSelect) %>%
      mutate(monthName = month.abb[month])
    incProgress(3/4, detail = "75%")
    filteredDat$monthName = fct_relevel(filteredDat$monthName, "Jan", "Feb", "Mar", "Apr", "May",
                                "Jun", "Jul", "Aug", "Sep", "Oct",
                                "Nov", "Dec")
  }
  
  filteredDat %>% 
    drop_na(tempC) %>%
    select(c(sid, cat, lat, lon, monthName, year, tempC))

}


createSiteMonthData <- function(siteType, dat, yearSelect, monthSelect, landLabel) {
  
  # filter data and configure months
  if(siteType == "Site") {
    filteredDat <- dat %>% 
      filter(sid == landLabel)
    incProgress(1/4, detail = "25%")
    filteredDat <- filteredDat %>%
      filter(year == yearSelect,
             month == monthSelect) %>%
      group_by(day, month, year, sid) %>%
      summarise(meanTemp = round(mean(tempC, na.rm = TRUE),2),
                minTemp = round(min(tempC, na.rm = TRUE),2),
                maxTemp = round(max(tempC, na.rm = TRUE),2)) %>%
      rename(location = sid)
    incProgress(3/4, detail = "75%")
    #return(filteredDat)
  } else if(siteType == "Land cover") {
    filteredDat <- dat %>% 
      filter(cat == landLabel)
    incProgress(1/4, detail = "25%")
    filteredDat <- filteredDat %>%
      filter(year == yearSelect,
             month == monthSelect) %>%
      group_by(day, month, year, cat) %>%
      summarise(meanTemp = round(mean(tempC, na.rm = TRUE),2),
                minTemp = round(min(tempC, na.rm = TRUE),2),
                maxTemp = round(max(tempC, na.rm = TRUE),2)) %>%
      rename(location = cat)
    incProgress(3/4, detail = "75%")
  }
  
  return(filteredDat) 
  
}

createSiteDayData <- function(siteType, dat, yearSelect, monthSelect, daySelect, landLabel) {
  
  # filter data and configure months
  if(siteType == "Site") {
    incProgress(1/4, detail = "25%")
    filteredDat <- dat %>% 
      filter(sid == landLabel)
    incProgress(2/4, detail = "50%")
    filteredDat <- filteredDat %>% 
      filter(year == yearSelect,
             month == monthSelect, 
             day == daySelect) 
    incProgress(3/4, detail = "75%")
    filteredDat <- filteredDat %>%
      group_by(Time1, sid, month, day, year) %>%
      summarise(meanTemp = mean(tempC, na.rm = TRUE),
                sdTemp = sd(tempC, na.rm = TRUE),
                count = n(),
                seTemp = sdTemp/sqrt(count)) %>%
      rename(location = sid) %>%
      select(location, month, day, year, Time1, meanTemp, sdTemp, seTemp)
  } else if(siteType == "Land cover") {
    incProgress(1/4, detail = "25%")
    filteredDat <- dat %>% 
      filter(cat == landLabel)
    incProgress(2/4, detail = "50%")
    filteredDat <- filteredDat %>% 
      filter(year == yearSelect,
             month == monthSelect, 
             day == daySelect) 
    incProgress(3/4, detail = "75%")
    filteredDat <- filteredDat %>%
      group_by(Time1, cat, month, day, year) %>%
      summarise(meanTemp = mean(tempC, na.rm = TRUE),
                sdTemp = sd(tempC, na.rm = TRUE),
                count = n(),
                seTemp = sdTemp/sqrt(count)) %>%
      rename(location = cat) %>%
      select(location, month, day, year, Time1, meanTemp, sdTemp, seTemp)
  }
  
  incProgress(7/8, detail = "85%")
  filteredDat$Time <- substr(as.POSIXct(sprintf("%04.0f", filteredDat$Time1), format='%H%M'), 12, 16)
  
  return(filteredDat)
  
}

createDateRangeData <- function(siteType, dat, startDate, endDate, landLabel) {
  
  if(siteType == "Site") {
    filteredDat <- dat %>% 
      filter(sid == landLabel)
    incProgress(1/4, detail = "25%")
    filteredDat <- filteredDat %>%
      filter(between(date, startDate, endDate))
    incProgress(2/4, detail = "50%")
    sumDat <- filteredDat %>%
      group_by(time, sid, month, day, year, Time1) %>%
      summarise(meanTemp = mean(tempC, na.rm = TRUE)) %>%
      rename(location = sid)
  } else if(siteType == "Land cover") {
    filteredDat <- dat %>% 
      filter(cat == landLabel) 
    incProgress(1/4, detail = "25%")
    filteredDat <- filteredDat %>%
      filter(between(date, startDate, endDate))
    incProgress(2/4, detail = "50%")
    sumDat <- filteredDat %>%
      group_by(time, cat, month, day, year, Time1) %>%
      summarise(meanTemp = mean(tempC, na.rm = TRUE)) %>%
      rename(location = cat)
  }
  
  return(data.frame(sumDat))
  
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
    layout(font = list(family = "Helvetica"), title = list(text = title, font = list(size = 20), y = 0.95),
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
        layout(title = list(family = "Helvetica", text = paste0(title, ": ", "<span style='color:red'>Choose another site
                                          (mising data)</span>")))
        
    }
    
  }
  
  return(plt)
  
}


monthPlotSingle <- function(dat, title, landLabel, yLabel) {
  
  p <- plot_ly(data = data.frame(dat), x = ~day) %>%
    add_trace(y =~minTemp, type = "scatter", mode = 'lines+markers', 
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
    layout(font = list(family = "Helvetica"),
           title = list(text = title, font = list(size = 20), y = 0.95),
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
      plt <- base_plot %>%
        add_trace(data = data.frame(dat2), y =~minTemp, x = ~day, type = 'scatter', mode = 'lines+markers', 
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
        layout(title = list(family = "Helvetica", text = paste0(title, ": ", "<span style='color:red'>Choose another site
                                          (mising data)</span>")))
      
    }
    
  }
  
  return(plt)
  
}


dayPlotSingle <- function(datType, dat, title, landLabel, yLabel) {

  if(datType == "Site") {
    p <- plot_ly(data = data.frame(dat)) %>%
      add_trace(x = ~Time, y = ~meanTemp, type = 'scatter', mode = 'lines+markers',
                name = landLabel,
                line = list(color = "#6a5acd"),
                marker = list(color = "#6a5acd"),
                hoverinfo = "text",
                #hovertext = ~paste("Temp:", round(meanTemp, 1))
                hovertext = ~ round(meanTemp, 1)) %>%
      layout(font = list(family = "Helvetica"),
             title = list(text = title, font = list(size = 20), y = 0.95),
             xaxis = list(title = list(text = "Time of day", font = list(size = 20))),
             yaxis = list(title = list(text = yLabel, font = list(size = 20))))
    
  } else if(datType == "Land cover") { 
    
    p <- plot_ly(data = data.frame(dat)) %>%
      add_trace(x = ~Time, y = ~meanTemp, name = landLabel,
                type = 'scatter', mode = 'lines+markers',
                line = list(color = "#6a5acd"),
                marker = list(color = "#6a5acd"),
                hoverinfo = "text",
                hovertext = ~paste("Mean temp:", round(meanTemp, 1)),
                error_y = ~list(array = c(seTemp),
                                color = "#6a5acd")) %>%
      layout(font = list(family = "Helvetica"),
             title = list(text = title, font = list(size = 20), y = 0.95),
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
          add_trace(data = data.frame(dat2), y = ~meanTemp, x = ~Time, name = landLabel2,
                    type = 'scatter', mode = 'lines+markers',
                    hoverinfo = "text",
                    hovertext = ~ round(meanTemp, 1))
      } else if(datType2 == "Land cover") {
        plt <- base_plot %>%
          add_trace(data = data.frame(dat2), y = ~meanTemp, x = ~Time, name = landLabel2,
                    type = 'scatter', mode = 'lines+markers',
                    hoverinfo = "text",
                    hovertext = ~paste("Mean temp:", round(meanTemp, 1)),
                    error_y = ~list(array = c(seTemp)))
      }
    } else if(nrow(dat2) < 1) {
      plt <- base_plot %>%
        layout(title = list(family = "Helvetica", text = paste0(title, ": ", "<span style='color:red'>Choose another site
                                          (mising data)</span>")))
    }
  }
  
  return(plt)
  
}

dateRangeSinglePlot <- function(dat, title, landLabel, yLabel) {
  
  plot_ly(data = dat, x = ~time) %>%
    add_trace(y = ~meanTemp, type = "scatter", mode = "lines+markers",
              name = landLabel,
              line = list(color = "#6a5acd"),
              marker = list(color = "#6a5acd"),
              hoverinfo = "text",
              hovertext = ~paste0(landLabel, ": ", round(meanTemp, 1))) %>%
    layout(font = list(family = "Helvetica"),
           title = list(text = title, font = list(size = 20), y = 0.95),
           xaxis = list(title = list(text = "Day", font = list(size = 20))),
           yaxis = list(title = list(text = yLabel, font = list(size = 20))),
           hovermode = "x unified")
  
}

dateRangeCompletePlot <- function(dat1, dat2 = NULL, title, landLabel1, landLabel2, yLabel, compare) {
  
  base_plot <- dateRangeSinglePlot(dat = dat1, title = title, landLabel = landLabel1, yLabel = yLabel)
  
  if(compare == FALSE) {
    plt <- base_plot
  } else if(compare == TRUE) {
    if(nrow(dat2) > 1) {
      
      dat2$difference <- abs(dat1$meanTemp - dat2$meanTemp)
      
      plt <- base_plot %>%
        add_trace(data = dat2, y =~meanTemp, x = ~time, type = 'scatter', mode = 'lines+markers', 
                  hoverinfo = "text", 
                  name = landLabel2,
                  line = list(color = "#CA2C92"),
                  marker = list(color = "#CA2C92"),
                  hovertext = ~paste0(landLabel2, ": ", round(meanTemp,1))) %>%
        add_trace(y = ~ difference, type = 'scatter', mode = 'lines+markers',
                  hoverinfo = "text",
                  name = "Temperature difference", 
                  line = list(color = "#0096C7"),
                  marker = list(color = "#0096C7"),
                  hovertext = ~paste("Temperature difference:", round(difference,1)))
      
    } else if(nrow(dat2) < 1) {
      plt <- base_plot %>%
        layout(title = list(family = "Helvetica", text = paste0(title, ": ", "<span style='color:red'>Choose another site
                                          (mising data)</span>")))
    }
  }
  
  plt
  
}

# data differences-------------------

datDif <- function(dat1, dat2, timeSelect) {
  
  if(timeSelect == "Year") {
    
    # get min, mean, max for each month
    sum1 <- dat1 %>%
      group_by(monthName) %>%
      summarise(minTemp = round(min(temp, na.rm = TRUE),2),
                #meanTemp = mean(tempC, na.rm = TRUE),
                maxTemp = round(max(temp, na.rm = TRUE),2))
    
    sum2 <- dat2 %>%
      group_by(monthName) %>%
      summarise(minTemp = round(min(temp, na.rm = TRUE),2),
                #meanTemp = mean(tempC, na.rm = TRUE),
                maxTemp = round(max(temp, na.rm = TRUE),2))
    
    newDat <- data.frame(Month = month.abb, minDif = round(abs(sum1$minTemp - sum2$minTemp),2), 
                         maxDif = round(abs(sum1$maxTemp - sum2$maxTemp),2))
    
  } else if(timeSelect == "Month") {
    
    newDat <- data.frame(day = dat1$day, minDif = round(abs(dat1$minTemp - dat2$minTemp),2),
                         maxDif = round(abs(dat1$maxTemp - dat2$maxTemp),2))
    
   } else if(timeSelect == "Day") {
     
     newDat <- data.frame(Time = dat1$Time, meanDif = round(abs(dat1$meanTemp - dat2$meanTemp),2))
     
   } else if(timeSelect == "Date range") {
     
     newDat <- data.frame(DateTime = dat1$time, 
                          meanDif = round(abs(dat1$meanTemp - dat2$meanTemp),2))
     
   }
   
}


