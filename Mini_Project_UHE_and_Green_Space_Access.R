###############My Personal Project########################
##Mini Project: Urban Heat Exposure & Green Space Access in Accra


####loading packages
install.packages("rgee")
library(sf)
library(terra)
library(osmdata)
library(tmap)
library(dplyr)
library(rgee)

###setting a working directory
setwd("D:/UHE_Mini_Project/gadm41_GHA_shp")

## loading my study area boundary ie Accra Boundary filtering only Accra
accra <- st_read("Accra_Shapefile.shp")

nrow(ghana)          # how many features total?
unique(ghana$NAME_2) # see all admin-2 names

accra <- ghana |>
  filter(NAME_2 == "Accra")

nrow(accra)          # should now be 1 (or >0)
plot(accra["NAME_2"])

#now lets download Green space (parks) from Open Street Map
q <- opq("Accra") %>% #opq("Accra") tells OSM what city you want.
  add_osm_feature("leisure", "park") #add_osm_feature() retrieves parks (leisure = park).

parks <- osmdata_sf(q)$osm_polygons %>% #osmdata_sf() loads them into R.
  st_transform(st_crs(accra))  ##st_transform() fixes CRS so both datasets match.

plot(st_geometry(accra))
plot(st_geometry(parks), col = "green", add = TRUE)

####loading the land surface Temperature (LST) 
lst <- rast("D:/UHE_Mini_Project/Accra_LST_2023.tif")
plot(lst)

lst

### clipping Raster to Accra
lst_accra <- crop(lst, vect(accra))
lst_accra <- mask(lst_accra, vect(accra))

####trying to fix clipping issues
plot(accra)
st_is_empty(accra)
st_is_valid(accra)
crs(accra)
crs(lst)
vect(accra)
crop(lst, vect(accra))

#############using alternative solutions
rtemplate <- rast(lst)  # same extent/resolution as LST
accra_r <- rasterize(accra, rtemplate, field=1)
lst_accra <- mask(lst, accra_r)
plot(lst_accra)

library(raster)

lst_r <- raster(lst)
accra_sp <- as(accra, "Spatial")

lst_accra <- mask(crop(lst_r, accra_sp), accra_sp)
plot(lst_accra)

accra$mean_LST <- terra::extract(
  rast(lst_accra),
  vect(accra),
  fun = mean,
  na.rm = TRUE
)[,2]



#####computing Park accessibility (buffers)
###urban planning uses accessibility buffers: 300m ie walkable access
parks <- st_make_valid(parks)
park_buff <- st_buffer(parks, dist = 300)

#### lets plot
plot(st_geometry(accra))
plot(st_geometry(park_buff), col = rgb(0,1,0,0.3), add = TRUE)

#####computting % Green Access in Accra
### so we have a formular here which is green access % = area covered by park buffers/total area *100
## representing this here:
rtemp <- rast(lst_accra)  # raster template
park_r <- terra::rasterize(vect(parks), rtemp, field=1)
buff_r <- terra::rasterize(vect(park_buff), rtemp, field=1)

accra$green_access_pct <- (global(buff_r, "sum", na.rm=TRUE) / 
                             ncell(buff_r)) * 100

parks <- st_make_valid(parks)
park_buff <- st_buffer(parks, dist = 300)
park_buff <- st_make_valid(park_buff)

accra$green_access_pct <- 
  (st_area(st_union(park_buff)) / st_area(accra)) * 100


#####classifying heat levels
accra$heat_level <- cut(
  accra$mean_LST,
  breaks = quantile(accra$mean_LST, c(0, 0.33, 0.66, 1)),
  labels = c("Low", "Medium", "High"),
  include.lowest = TRUE
)

#####
accra$risk_category <- dplyr::case_when(
  accra$heat_level == "High" & accra$green_access_pct < 20 ~ "HIGH RISK",
  accra$heat_level == "High" ~ "Heat Risk",
  accra$green_access_pct < 20 ~ "Low Green Access",
  TRUE ~ "Normal"
)


############visualization
library(tmap)
tmap_mode("plot")

tm_shape(accra) +
  tm_fill("mean_LST", palette = "inferno") +
  tm_borders() +
  tm_layout(title = "Mean Land Surface Temperature — Accra")


##########risk map####
tm_shape(accra) +
  tm_fill("risk_category", palette = "Set2") +
  tm_borders() +
  tm_layout(title = "Urban Heat Risk Categories — Accra")

