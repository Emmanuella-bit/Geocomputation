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

##### heat and green access into risk category
accra$risk_category <- dplyr::case_when(
  accra$heat_level == "High" & accra$green_access_pct < 20 ~ "HIGH RISK",
  accra$heat_level == "High" ~ "Heat Risk",
  accra$green_access_pct < 20 ~ "Low Green Access",
  TRUE ~ "Normal"
)


############visualization, we visualiye the map
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


####now lets summarize everything
accra_summary <- accra %>%
  st_drop_geometry() %>%
  select(mean_LST, green_access_pct, heat_level, risk_category)

accra_summary





##########loading  shiny app###################
###Loading the LST Raster & Clip to Accra

library(terra)

lst <- rast("Accra_LST_example.tif")

# Clip and mask
lst_accra <- crop(lst, vect(accra))
lst_accra <- mask(lst_accra, vect(accra))

###let check
plot(lst_accra)

#################Preparing the Parks + Buffer Layers#########
library(sf)

# Parks (already downloaded in earlier step)
parks <- st_transform(parks, st_crs(accra))

# 300m buffer
park_buff <- st_buffer(parks, dist = 300)

##### now we convert Raster → Dataframe For Leaflet RasterLayer
library(leaflet)
library(leaflet.extras)

pal <- colorNumeric("inferno", values(lst_accra), na.color = "transparent")



####### now lets build the shiny dashboard 
library(shiny)
library(leaflet)
library(sf)
library(terra)
library(dplyr)

############### lets load the data
# DATA LOADING
##############

# Load Accra boundary
accra <- st_read("gadm41_GHA_2.shp") %>%
  filter(NAME_2 == "Accra Metropolitan")

# Load parks
q <- opq("Accra") %>% add_osm_feature("leisure", "park")
parks <- osmdata_sf(q)$osm_polygons
parks <- st_transform(parks, st_crs(accra))

# Park buffers (300m)
park_buff <- st_buffer(parks, dist = 300)

# Load raster
lst <- rast("Accra_LST_example.tif")
lst_accra <- crop(lst, vect(accra))
lst_accra <- mask(lst_accra, vect(accra))

# Convert raster for Leaflet
pal <- colorNumeric("inferno", values(lst_accra), na.color = "transparent")

# Extract mean LST
accra$mean_LST <- terra::extract(lst_accra, vect(accra), fun = mean, na.rm = TRUE)[,2]

# Green space percentage
accra$green_access_pct <- (st_area(st_union(park_buff)) / st_area(accra)) * 100

# Heat category
accra$heat_level <- cut(accra$mean_LST,
                        breaks = quantile(accra$mean_LST, c(0, 0.33, 0.66, 1)),
                        labels = c("Low", "Medium", "High"),
                        include.lowest = TRUE)

# Risk category
accra$risk_category <- case_when(
  accra$heat_level == "High" & accra$green_access_pct < 20 ~ "HIGH RISK",
  accra$heat_level == "High" ~ "Heat Risk",
  accra$green_access_pct < 20 ~ "Low Green Access",
  TRUE ~ "Normal"
)

#########
# SHINY UI
###########

ui <- fluidPage(
  titlePanel("Urban Heat & Green Space Access – Accra"),
  sidebarLayout(
    sidebarPanel(
      h4("Map Controls"),
      checkboxInput("show_parks", "Show Parks", TRUE),
      checkboxInput("show_buffers", "Show 300m Buffers", TRUE),
      checkboxInput("show_heat", "Show Heat Map (LST)", TRUE),
      checkboxInput("show_risk", "Show Risk Map", FALSE)
    ),
    mainPanel(
      leafletOutput("map", height = 650)
    )
  )
)

###############
# SHINY SERVER
###############

server <- function(input, output, session) {
  
  output$map <- renderLeaflet({
    
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = -0.2, lat = 5.55, zoom = 12)
  })
  
  observe({
    map <- leafletProxy("map")
    
    map %>% clearGroup("parks") %>% clearGroup("buffers") %>%
      clearGroup("heat") %>% clearGroup("risk")
    
    # Parks
    if (input$show_parks) {
      map %>% addPolygons(data = parks, color = "green",
                          group = "parks", fillOpacity = 0.4)
    }
    
    # Buffers
    if (input$show_buffers) {
      map %>% addPolygons(data = park_buff, color = "blue",
                          group = "buffers", fillOpacity = 0.2)
    }
    
    # Heat (LST)
    if (input$show_heat) {
      map %>% addRasterImage(lst_accra, colors = pal,
                             group = "heat", opacity = 0.8)
    }
    
    # Risk map
    if (input$show_risk) {
      pal2 <- colorFactor(c("red","orange","grey","green"),
                          accra$risk_category)
      
      map %>% addPolygons(data = accra, fillColor = pal2(accra$risk_category),
                          color = "black", fillOpacity = 0.6, group = "risk",
                          popup = ~paste(
                            "<b>Risk Category:</b>", risk_category, "<br>",
                            "<b>Mean LST:</b>", round(mean_LST,2), "<br>",
                            "<b>Green Access:</b>", round(green_access_pct,1), "%"
                          ))
    }
  })
}

shinyApp(ui, server)


