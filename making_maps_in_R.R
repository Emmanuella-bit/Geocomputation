###########Making maps in R###################################
library(sf)
library(terra)
library(dplyr)
library(spData)
library(spDataLarge)
library(terra)

install.packages("tmap", repos = c("https://r-tmap.r-universe.dev",
                                   "https://cloud.r-project.org"))
install.packages("ggplot2")

library(tmap)    # for static and interactive maps
library(leaflet) # for interactive maps
library(ggplot2) # tidyverse data visualization package

nz_elev = rast(system.file("raster/nz_elev.tif", package = "spDataLarge"))

# Load New Zealand polygon
data("nz")
nz

##tmap basics
# Add fill layer to nz shape
tm_shape(nz) +
  tm_fill() 
# Add border layer to nz shape
tm_shape(nz) +
  tm_borders() 
# Add fill and border layers to nz shape
tm_shape(nz) +
  tm_fill() +
  tm_borders() 

###### we have others like
tm_polygons()
tm_lines()
tm_symbols()
tm_raster() colored cells of raster data (there is also tm_rgb() for rasters with three layers)
tm_text()



####mapping objects 
map_nz = tm_shape(nz) + tm_polygons()
class(map_nz)
print(map_nz)

map_nz1 = map_nz +
  tm_shape(nz_elev) + tm_raster(col_alpha = 0.7)

nz_water = st_union(nz) |>
  st_buffer(22200) |> 
  st_cast(to = "LINESTRING")
map_nz2 = map_nz1 +
  tm_shape(nz_water) + tm_lines()
plot(nz_water)

map_nz3 = map_nz2 +
  tm_shape(nz_height) + tm_symbols()
map_nz3

tmap_arrange(map_nz1, map_nz2, map_nz3)





############colorful visuals##############
#fill: fill color of a polygon
#col: color of a polygon border, line, point, or raster
#lwd: line width
#lty: line type
#size: size of a symbol
#shape: shape of a symbol
#we may customize the fill and border color transparency using fill_alpha and col_alpha.

map1 = tm_shape(nz) + tm_polygons(fill = "red")
map2 = tm_shape(nz) + tm_polygons(fill = "red", fill_alpha = 0.3)
map3 = tm_shape(nz) + tm_polygons(col = "blue")
map4 = tm_shape(nz) + tm_polygons(lwd = 3)
map5 = tm_shape(nz) + tm_polygons(lty = 2)
map6 = tm_shape(nz) + tm_polygons(fill = "red", fill_alpha = 0.3,
                                 col = "blue", lwd = 3, lty = 2)
tmap_arrange(map1, map2, map3, map4, map5, map6)

plot(st_geometry(nz), col = nz$Land_area)  # works
tm_shape(nz) + tm_fill(fill = nz$Land_area) 

tm_shape(nz) + tm_fill(fill = "Land_area")



########scales: Scales control how the values are represented on the map and in the legend, and they largely depend on the selected visual variable.
tm_shape(nz) + tm_polygons(fill = "Median_income")
tm_shape(nz) + tm_polygons(fill = "Median_income",
                           fill.scale = tm_scale(breaks = c(0, 30000, 40000, 50000)))
tm_shape(nz) + tm_polygons(fill = "Median_income",
                           fill.scale = tm_scale(n = 10))
tm_shape(nz) + tm_polygons(fill = "Median_income",
                           fill.scale = tm_scale(values = "BuGn"))

tm_shape(nz) + 
  tm_polygons("Median_income", fill.scale = tm_scale(values = "greens"))
tm_shape(nz) + 
  tm_polygons("Median_income", fill.scale = tm_scale(values = "yl_gn_bu"))


tm_shape(nz) + 
  tm_polygons("Median_income",
              fill.scale = tm_scale_continuous(values = "pu_gn_div", 
                                               midpoint = 28000))



###########legends###################
legend_title = expression("Area (km"^2*")")
tm_shape(nz) +
  tm_polygons(fill = "Land_area", fill.legend = tm_legend(title = legend_title))

tm_shape(nz) +
  tm_polygons(fill = "Land_area",
              fill.legend = tm_legend(title = legend_title,
                                      orientation = "landscape",
                                      position = tm_pos_out("center", "bottom")))


#################layouts###################
map_nz + 
  tm_graticules() +
  tm_compass(type = "8star", position = c("left", "top")) +
  tm_scalebar(breaks = c(0, 100, 200), text.size = 1, position = c("left", "top")) +
  tm_title("New Zealand")

map_nz + tm_layout(scale = 4)
map_nz + tm_layout(bg.color = "lightblue")
map_nz + tm_layout(frame = FALSE)



##########Faceted maps###################
urb_1970_2030 = urban_agglomerations |> 
  filter(year %in% c(1970, 1990, 2010, 2030))

tm_shape(world) +
  tm_polygons() +
  tm_shape(urb_1970_2030) +
  tm_symbols(fill = "black", col = "white", size = "population_millions") +
  tm_facets_wrap(by = "year", nrow = 2)


##############Inset maps######################
nz_region = st_bbox(c(xmin = 1340000, xmax = 1450000,
                      ymin = 5130000, ymax = 5210000),
                    crs = st_crs(nz_height)) |> 
  st_as_sfc()

nz_height_map = tm_shape(nz_elev, bbox = nz_region) +
  tm_raster(col.scale = tm_scale_continuous(values = "YlGn"),
            col.legend = tm_legend(position = c("left", "top"))) +
  tm_shape(nz_height) + tm_symbols(shape = 2, col = "red", size = 1) +
  tm_scalebar(position = c("left", "bottom"))

nz_map = tm_shape(nz) + tm_polygons() +
  tm_shape(nz_height) + tm_symbols(shape = 2, col = "red", size = 0.1) + 
  tm_shape(nz_region) + tm_borders(lwd = 3) +
  tm_layout(bg.color = "lightblue")

library(grid)
norm_dim = function(obj){
  bbox = st_bbox(obj)
  width = bbox[["xmax"]] - bbox[["xmin"]]
  height = bbox[["ymax"]] - bbox[["ymin"]]
  w = width / max(width, height)
  h = height / max(width, height)
  return(unit(c(w, h), "snpc"))
}
main_dim = norm_dim(nz_region)
ins_dim = norm_dim(nz)

main_vp = viewport(width = main_dim[1], height = main_dim[2])

ins_vp = viewport(width = ins_dim[1] * 0.5, height = ins_dim[2] * 0.5,
                  x = unit(1, "npc") - unit(0.5, "cm"), y = unit(0.5, "cm"),
                  just = c("right", "bottom"))

grid.newpage()
print(nz_height_map, vp = main_vp)
pushViewport(main_vp)
print(nz_map, vp = ins_vp)

#########united states###############
us_states_map = tm_shape(us_states, crs = "EPSG:9311") + 
  tm_polygons() + 
  tm_layout(frame = FALSE)

#### alaska and hawaii
hawaii_map = tm_shape(hawaii) +
  tm_polygons() + 
  tm_title("Hawaii") +
  tm_layout(frame = FALSE, bg.color = NA, 
            title.position = c("LEFT", "BOTTOM"))
alaska_map = tm_shape(alaska) +
  tm_polygons() + 
  tm_title("Alaska") +
  tm_layout(frame = FALSE, bg.color = NA)

###final map
us_states_map
print(hawaii_map, vp = grid::viewport(0.35, 0.1, width = 0.2, height = 0.1))
print(alaska_map, vp = grid::viewport(0.15, 0.15, width = 0.3, height = 0.3))


############making animated maps######
urb_anim = tm_shape(world) + tm_polygons() + 
  tm_shape(urban_agglomerations) + tm_symbols(size = "population_millions") +
  tm_facets_wrap(by = "year", nrow = 1, ncol = 1, free.coords = FALSE)

tmap_animation(urb_anim, filename = "urb_anim.gif", delay = 25)


##############interactive map#################
tmap_mode("view")
map_nz

map_nz + tm_basemap(server = "OpenTopoMap")

####faceted plots in tmaps
world_coffee = left_join(world, coffee_data, by = "name_long")
facets = c("coffee_production_2016", "coffee_production_2017")
tm_shape(world_coffee) + tm_polygons(facets) + 
  tm_facets_wrap(nrow = 1, sync = TRUE)

tmap_mode("plot")

####alternative to tmap for interactive maps view in R is mapview
mapviewOptions()
mapview::mapview(nz)
library(mapview)
oberfranken = subset(franconia, district == "Oberfranken")
trails |>
  st_transform(st_crs(oberfranken)) |>
  st_intersection(oberfranken) |>
  st_collection_extract("LINESTRING") |>
  mapview(color = "red", lwd = 3, layer.name = "trails") +
  mapview(franconia, zcol = "district") +
  breweries

#### another alternative is mapdeck (2.5/3D perspective)
install.packages("mapdeck")

library(mapdeck)
set_token(Sys.getenv("MAPBOX"))
crash_data = read.csv("https://git.io/geocompr-mapdeck")
crash_data = na.omit(crash_data)
ms = mapdeck_style("dark")
mapdeck(style = ms, pitch = 45, location = c(0, 52), zoom = 4) |>
  add_grid(data = crash_data, lat = "lat", lon = "lng", cell_size = 1000,
           elevation_scale = 50, colour_range = hcl.colors(6, "plasma"))

pal = colorNumeric("RdYlBu", domain = cycle_hire$nbikes)
leaflet(data = cycle_hire) |> 
  addProviderTiles(providers$CartoDB.Positron) |>
  addCircles(col = ~pal(nbikes), opacity = 0.9) |> 
  addPolygons(data = lnd, fill = FALSE) |> 
  addLegend(pal = pal, values = ~nbikes) |> 
  setView(lng = -0.1, 51.5, zoom = 12) |> 
  addMiniMap()



###########mapping applications############
library(shiny)    # for shiny apps
library(leaflet)  # renderLeaflet function
library(spData)   # loads the world dataset 
ui = fluidPage(
  sliderInput(inputId = "life", "Life expectancy", 49, 84, value = 80),
  leafletOutput(outputId = "map")
)
server = function(input, output) {
  output$map = renderLeaflet({
    leaflet() |> 
      # addProviderTiles("OpenStreetMap.BlackAndWhite") |>
      addPolygons(data = world[world$lifeExp < input$life, ])})
}
shinyApp(ui, server)


####other mapping applications
g = st_graticule(nz, lon = c(170, 175), lat = c(-45, -40, -35))
plot(nz_water, graticule = g, axes = TRUE, col = "blue")
terra::plot(nz_elev / 1000, add = TRUE, axes = FALSE)
plot(st_geometry(nz), add = TRUE)

library(ggplot2)
g1 = ggplot() + geom_sf(data = nz, aes(fill = Median_income)) +
  geom_sf(data = nz_height) +
  scale_x_continuous(breaks = c(170, 175))
g1

install.packages("ggspatial")
library(ggspatial)
ggplot() + 
  layer_spatial(nz_elev) +
  geom_sf(data = nz, fill = NA) +
  annotation_scale() +
  scale_x_continuous(breaks = c(170, 175)) +
  scale_fill_continuous(na.value = NA)

###### cartogram #############
install.packages("cartogram")
library(cartogram)
nz_carto = cartogram_cont(nz, "Median_income", itermax = 5)
tm_shape(nz_carto) + tm_polygons("Median_income")

#########using cartogram
us_states9311 = st_transform(us_states, "EPSG:9311")
us_states9311_ncont = cartogram_ncont(us_states9311, "total_pop_15")
us_states9311_dorling = cartogram_dorling(us_states9311, "total_pop_15")

