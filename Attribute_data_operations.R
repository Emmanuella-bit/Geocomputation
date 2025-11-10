###########Attribute data operations###########

library(sf)      # vector data package introduced in Chapter 2
library(terra)   # raster data package introduced in Chapter 2
library(dplyr)   # tidyverse package for data frame manipulation
library(spData)








##vector attribute manipulation 
methods(class = "sf")
class(world)
dim(world)

world_df = st_drop_geometry(world)
class(world_df)

ncol(world_df)
world[1:6, ]    # subset rows by position
world[, 1:3]    # subset columns by position
world[1:6, 1:3] # subset rows and columns by position
world[, c("name_long", "pop")] # columns by name
world[, c(T, T, F, F, F, F, F, T, T, F, F)] # by logical indices
world[, 888] # an index representing a non-existent column

i_small = world$area_km2 < 10000
summary(i_small) 
small_countries = world [i_small, ]

small_countries = world[world$area_km2 < 10000, ]
small_countries = subset(world, area_km2 < 10000)
