library(tidycensus)
library(tidyverse)
library(leaflet)
library(stringr)
library(sf)

options(tigris_use_cache = TRUE)

readRenviron("~/.Renviron")

states <- get_decennial(geography = "state", variables = "H043A001", year = 1990, geometry = TRUE)

pal <- colorQuantile(palette = c("blue", "red"), domain = states$value, n = 10)

states %>%
  st_transform(crs = "+init=epsg:4326") %>%
  leaflet(width = "100%") %>%
  addProviderTiles(provider = "CartoDB.Positron") %>%
  addPolygons(popup = ~ str_extract(NAME, "^([^,]*)"),
              stroke = FALSE,
              smoothFactor = 0,
              fillOpacity = 0.7,
              color = ~ pal(value)) 
