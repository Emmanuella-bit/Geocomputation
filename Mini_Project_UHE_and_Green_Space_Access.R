###############My Personal Project########################
##Mini Project: Urban Heat Exposure & Green Space Access in Accra

library(sf)
library(terra)
library(osmdata)
library(tmap)
library(dplyr)


accra <- st_read("") %>%
  filter(NAME_2 == "Accra Metropolitan")
