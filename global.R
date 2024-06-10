#install libraries
library(shiny)
library(tidyverse)
library(sf)
library(leaflet)
library(leaflet.extras2)
library(shinycssloaders)
library(terra)


# load data
sites <- readxl::read_excel("data/WSC_Station_locations-06-03-2024.xlsx", sheet = 2) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = F) %>%
  mutate(categoryString = case_when(is.na(category2) & is.na(category3) ~ paste(cat, category1, sep = ", "),
                                    is.na(category3) ~ paste(cat, category1, category2, sep = ", "),
                                    TRUE ~ paste(cat, category1, category2, category3, sep = ", ")))

dat <- read_csv("../data/!TempC-selected/!TempC-2023v2.csv")

landCover <- rast("data/daneCountyLandCover.tif")

siteNames <- sites$sid

# map functions--------------------
base_map <- function() {
  leaflet() %>%
    addTiles() %>%
    addMapPane("sites", 450) %>%
    addMapPane("landCover", 423) %>%
    addCircles(data = sites, 
               lng = ~lon, lat = ~lat,
               opacity = 0.7,
               color = "black",
               group = "circles") %>%
    setView(lat = 43.08, lng = -89.37, zoom = 10) %>%
    addLayersControl(overlayGroups = "landCover",
                     options = layersControlOptions(collapsed = FALSE)) %>%
    addProviderTiles("USGS.USImageryTopo") %>%
    addRasterImage(landCover, group = "landCover",
                   colors = c("#b50101", "#e8d1d2",  "#cb9147", "darkgreen", "skyblue1", "steelblue3", "wheat")) 
}

palSite <- colorFactor(palette = c("#cb9147", "#e8d1d2", "#b50101"), domain = sites$cat)
palCover <- colorFactor(palette = c("#b50101", "#e8d1d2",  "#cb9147", "darkgreen", "skyblue1", "steelblue3", "wheat"),
                        levels = c("Urban", "Suburban", "Rural/Ag", "Forest", "Open water", "Wetland", "Barren/shrubland"))

layers = c(landCover = "Land cover")




