####################Reprojecting geographic data #################################
library(sf)
library(terra)
library(dplyr)
library(spData)
library(spDataLarge)


####Coordinate reference systems######
st_crs("EPSG:4326")

st_crs("ESRI:54030")

#### Querying and setting coordinate systems
vector_filepath = system.file("shapes/world.gpkg", package = "spData")
new_vector = read_sf(vector_filepath)

st_crs(new_vector)

new_vector = st_set_crs(new_vector, "EPSG:4326") # set CRS

raster_filepath = system.file("raster/srtm.tif", package = "spDataLarge")
my_rast = rast(raster_filepath)
cat(crs(my_rast))

crs(my_rast) = "EPSG:26912" # set CRS

london = data.frame(lon = -0.1, lat = 51.5) |> 
  st_as_sf(coords = c("lon", "lat"))
st_is_longlat(london)

london_geo = st_set_crs(london, "EPSG:4326")
st_is_longlat(london_geo)


###################Geometry operations on projected and unprojected data##########################
london_buff_no_crs = st_buffer(london, dist = 1)   # incorrect: no CRS
london_buff_s2 = st_buffer(london_geo, dist = 100000) # silent use of s2
london_buff_s2_100_cells = st_buffer(london_geo, dist = 100000, max_cells = 100)

sf::sf_use_s2(FALSE)
london_buff_lonlat = st_buffer(london_geo, dist = 1) # incorrect result
sf::sf_use_s2(TRUE)

london_proj = data.frame(x = 530000, y = 180000) |> 
  st_as_sf(coords = c("x", "y"), crs = "EPSG:27700")

st_crs(london_proj)

london_buff_projected = st_buffer(london_proj, 100000)
plot(london_buff_projected)


#####when to project
st_distance(london_geo, london_proj)
#### which CRS to use?
lonlat2UTM = function(lonlat) {
  utm = (floor((lonlat[1] + 180) / 6) %% 60) + 1
  if (lonlat[2] > 0) {
    utm + 32600
  } else{
    utm + 32700
  }
}

lonlat2UTM(c(174.7, -36.9))
lonlat2UTM(st_coordinates(london))


### Reprojecting vector geometries
london2 = st_transform(london_geo, "EPSG:27700")

st_distance(london2, london_proj)

st_crs(cycle_hire_osm)

crs_lnd = st_crs(london_geo)
class(crs_lnd)
names(crs_lnd)

crs_lnd$Name
crs_lnd$proj4string 
crs_lnd$epsg

cycle_hire_osm_project = st_transform(cycle_hire_osm, "EPSG:27700")
st_crs(cycle_hire_osm_projected)

crs_lnd_new = st_crs("EPSG:27700")
crs_lnd_new$Name
crs_lnd_new$epsg



#########################Reprojecting raster geometries##############################
cat_raster = rast(system.file("raster/nlcd.tif", package = "spDataLarge"))
crs(cat_raster)

unique(cat_raster)
cat_raster_wgs84 = project(cat_raster, "EPSG:4326", method = "near")

con_raster = rast(system.file("raster/srtm.tif", package = "spDataLarge"))
cat(crs(con_raster))

con_raster_ea = project(con_raster, "EPSG:32612", method = "bilinear")
cat(crs(con_raster_ea))


#######################custom map project#############################
zion = read_sf(system.file("vector/zion.gpkg", package = "spDataLarge"))

zion_centr = st_centroid(zion)
zion_centr_wgs84 = st_transform(zion_centr, "EPSG:4326")
st_as_text(st_geometry(zion_centr_wgs84))

my_wkt = 'PROJCS["Custom_AEQD",
 GEOGCS["GCS_WGS_1984",
  DATUM["WGS_1984",
   SPHEROID["WGS_1984",6378137.0,298.257223563]],
  PRIMEM["Greenwich",0.0],
  UNIT["Degree",0.0174532925199433]],
 PROJECTION["Azimuthal_Equidistant"],
 PARAMETER["Central_Meridian",-113.0263],
 PARAMETER["Latitude_Of_Origin",37.29818],
 UNIT["Meter",1.0]]'

zion_aeqd = st_transform(zion, my_wkt)

world_mollweide = st_transform(world, crs = "+proj=moll")

world_wintri = st_transform(world, crs = "+proj=wintri")

world_laea2 = st_transform(world,
                           crs = "+proj=laea +x_0=0 +y_0=0 +lon_0=-74 +lat_0=40")
plot(world_wintri)

