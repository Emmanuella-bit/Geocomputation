#######################Bridges to GIS software#############################
library(sf)
library(terra)
library(qgisprocess)
library(Rsagacmd)
library(rgrass)
library(rstac)
library(gdalcubes)

install.packages("qgisprocess")
install.packages("Rsagacmd")
install.packages("rgrass")
install.packages("rstac")
install.packages("gdalcubes")

library(qgisprocess)

qgis_plugins()

data("incongruent", "aggregating_zones", package = "spData")
incongr_wgs = st_transform(incongruent, "EPSG:4326")
aggzone_wgs = st_transform(aggregating_zones, "EPSG:4326")

# output not shown
qgis_algorithms()

qgis_search_algorithms("union")

alg = "native:union"
union_arguments = qgis_get_argument_specs(alg)
union_arguments

union = qgis_run_algorithm(alg,
                           INPUT = incongr_wgs, OVERLAY = aggzone_wgs
)
union

union_sf = st_as_sf(union)

library(qgisprocess)

qgis_algorithms() |> 
  dplyr::filter(grepl("clean", algorithm, ignore.case = TRUE))

qgis_search_algorithms("clean")

######grass does not work, will look into it later with my qgis installation

qgis_show_help("grass:v.clean")

qgis_get_argument_specs("grass:v.clean") |>
  select(name, description) |>
  slice_head(n = 4)

clean = qgis_run_algorithm("grass:v.clean",
                           input = union_sf, 
                           tool = "rmarea", threshold = 25000
)
clean_sf = st_as_sf(clean)



#######################rasta data##########################
library(qgisprocess)
library(terra)
dem = system.file("raster/dem.tif", package = "spDataLarge")

qgis_search_algorithms("wetness") |>
  dplyr::select(provider_title, algorithm) |>
  head(2)

qgis_show_help("sagang:sagawetnessindex")

#Before running the SAGA algorithm from within QGIS, we change the default raster output format from .tif to SAGA’s native raster format .sdat
options(qgisprocess.tmp_raster_ext = ".sdat")
dem_wetness = qgis_run_algorithm("sagang:sagawetnessindex",
                                 DEM = dem
)

dem_wetness_twi = qgis_as_terra(dem_wetness$TWI)
# plot(dem_wetness_twi)
options(qgisprocess.tmp_raster_ext = ".tif")

qgis_search_algorithms("geomorphon")
#> [1] "grass:r.geomorphon" "sagang:geomorphons" 
qgis_show_help("grass:r.geomorphon")

dem_geomorph = qgis_run_algorithm("grass:r.geomorphon",
                                  elevation = dem,
                                  `-m` = TRUE, search = 120
)

dem_geomorph_terra = qgis_as_terra(dem_geomorph$forms)



####################SAGA#####################
ndvi = rast(system.file("raster/ndvi.tif", package = "spDataLarge"))

######i could not also install SAGA
install.packages("RSAGA")
library(RSAGA)

library(Rsagacmd)
saga = saga_gis(raster_backend = "terra", vector_backend = "sf")

sg = saga$imagery_segmentation$seed_generation

ndvi_seeds = sg(ndvi, band_width = 2)

srg = saga$imagery_segmentation$seeded_region_growing
ndvi_srg = srg(ndvi_seeds$seed_grid, ndvi, method = 1)
plot(ndvi_srg$segments)


ndvi_segments = ndvi_srg$segments |>
  as.polygons() |>
  st_as_sf()


########################GRASS######################
data("cycle_hire", package = "spData")
points = cycle_hire[1:25, ]

library(osmdata)
b_box = st_bbox(points)
london_streets = opq(b_box) |>
  add_osm_feature(key = "highway") |>
  osmdata_sf()
london_streets = london_streets[["osm_lines"]]
london_streets = select(london_streets, osm_id)

library(rgrass)
link2GI::linkGRASS(london_streets, ver_select = TRUE)

write_VECT(terra::vect(london_streets), vname = "london_streets")
write_VECT(terra::vect(points[, 1]), vname = "points")


##to perform our network analysis, we need a topologically clean street network. GRASS GIS’s "v.clean" takes care of the removal of duplicates, small angles and dangles, among others. 

execGRASS(
  cmd = "v.clean", input = "london_streets", output = "streets_clean",
  tool = "break", flags = "overwrite"
)

execGRASS(
  cmd = "v.net", input = "streets_clean", output = "streets_points_con",
  points = "points", operation = "connect", threshold = 0.001,
  flags = c("overwrite", "c")
)

execGRASS(
  cmd = "v.net.salesman", input = "streets_points_con",
  output = "shortest_route", center_cats = paste0("1-", nrow(points)),
  flags = "overwrite"
)

route = read_VECT("shortest_route") |>
  st_as_sf() |>
  st_geometry()
mapview::mapview(route) + points



#########when to use what?###################






###########bridges to GDAL##############
link2GI::linkGDAL()

our_filepath = system.file("shapes/world.gpkg", package = "spData")
cmd = paste("ogrinfo -al -so", our_filepath)
system(cmd)





































