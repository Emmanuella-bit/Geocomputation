################ Geographic data I/O################################
library(sf)
library(terra)
library(dplyr)
library(spData)

#####Data input
sf_drivers = st_drivers()
head(sf_drivers, n = 3)
summary(sf_drivers[-c(1:2)])

f = system.file("shapes/world.gpkg", package = "spData")
world = read_sf(f)

tanzania = read_sf(f, query = 'SELECT * FROM world WHERE name_long = "Tanzania"')

tanzania_buf = st_buffer(tanzania, 50000)
tanzania_buf_geom = st_geometry(tanzania_buf)
tanzania_buf_wkt = st_as_text(tanzania_buf_geom)

tanzania_neigh = read_sf(f, wkt_filter = tanzania_buf_wkt)
plot(tanzania_neigh)

cycle_hire_txt = system.file("misc/cycle_hire_xy.csv", package = "spData")
cycle_hire_xy = read_sf(cycle_hire_txt,
                        options = c("X_POSSIBLE_NAMES=X", "Y_POSSIBLE_NAMES=Y"))

world_txt = system.file("misc/world_wkt.csv", package = "spData")
world_wkt = read_sf(world_txt, options = "GEOM_POSSIBLE_NAMES=WKT")

u = "https://developers.google.com/kml/documentation/KML_Samples.kml"
download.file(u, "KML_Samples.kml")
st_layers("KML_Samples.kml")

kml = read_sf("KML_Samples.kml", layer = "Placemarks")

##########################Raster data#####################
raster_filepath = system.file("raster/srtm.tif", package = "spDataLarge")
single_layer = rast(raster_filepath)

multilayer_filepath = system.file("raster/landsat.tif", package = "spDataLarge")
multilayer_rast = rast(multilayer_filepath)

myurl = paste0("/vsicurl/https://zenodo.org/record/5774954/files/",
               "clm_snow.prob_esacci.dec_p.90_500m_s0..0cm_2000..2012_v2.0.tif")
snow = rast(myurl)
snow

rey = data.frame(lon = -21.94, lat = 64.15)
snow_rey = extract(snow, rey)
snow_rey

###Data output#Vector data#####
write_sf(obj = world, dsn = "world.gpkg")
write_sf(obj = world, dsn = "world.gpkg")
write_sf(obj = world, dsn = "world_many_layers.gpkg", layer = "second_layer")

st_write(obj = world, dsn = "world2.gpkg")

write_sf(cycle_hire_xy, "cycle_hire_xy.csv", layer_options = "GEOMETRY=AS_XY")
write_sf(world_wkt, "world_wkt.csv", layer_options = "GEOMETRY=AS_WKT")

#######################raster data###############
writeRaster(single_layer, filename = "my_raster.tif", datatype = "INT2U")
writeRaster(x = single_layer, filename = "my_raster.tif",
            gdal = c("COMPRESS=NONE"), overwrite = TRUE)


#########Geoportals
download.file(url = "https://hs.pangaea.de/Maps/PeRL/PeRL_permafrost_landscapes.zip",
              destfile = "PeRL_permafrost_landscapes.zip", 
              mode = "wb")
unzip("PeRL_permafrost_landscapes.zip")
canada_perma_land = read_sf("PeRL_permafrost_landscapes/canada_perma_land.shp")

library(rnaturalearth)
usa_sf = ne_countries(country = "United States of America", returnclass = "sf")

library(geodata)
worldclim_prec = worldclim_global("prec", res = 10, path = tempdir())
class(worldclim_prec)

library(osmdata)
parks = opq(bbox = "leeds uk") |> 
  add_osm_feature(key = "leisure", value = "park") |> 
  osmdata_sf()

world2 = spData::world
world3 = read_sf(system.file("shapes/world.gpkg", package = "spData"))

library(tidygeocoder)
geo_df = data.frame(address = "54 Frith St, London W1D 4SJ, UK")
geo_df = geocode(geo_df, address, method = "osm")
geo_df

geo_sf = st_as_sf(geo_df, coords = c("long", "lat"), crs = "EPSG:4326")
plot(geo_sf)


##########Geographic metadata###########
install.packages("geometa")
library(geometa)
# create a metadata
md = ISOMetadata$new()
#... fill the metadata 'md' object
# validate metadata
md$validate()
# XML representation of the ISOMetadata
xml = md$encode()
# save metadata
md$save("my_metadata.xml")
# read a metadata from an XML file
md = readISO19139("my_metadata.xml")



####### Geographic web services##########
library(httr)
base_url = "https://www.fao.org"
endpoint = "/fishery/geoserver/wfs"
q = list(request = "GetCapabilities")
res = GET(url = modify_url(base_url, path = endpoint), query = q)
res$url

txt = content(res, "text")
xml = xml2::read_xml(txt)
xml

library(sf)
sf::sf_use_s2(FALSE)
qf = list(request = "GetFeature", typeName = "fifao:FAO_MAJOR")
file = tempfile(fileext = ".gml")
GET(url = base_url, path = endpoint, query = qf, write_disk(file))
fao_areas = read_sf(file)

install.packages("ows4R")
library(ows4R)
WFS = WFSClient$new(
  url = "https://www.fao.org/fishery/geoserver/wfs",
  serviceVersion = "1.0.0",
  logger = "INFO"
)


#######Visual output######
png(filename = "lifeExp.png", width = 500, height = 350)
plot(world["lifeExp"])
dev.off()

install.packages("tmap")
library(tmap)
tmap_obj = tm_shape(world) + tm_polygons(col = "lifeExp")
tmap_save(tmap_obj, filename = "lifeExp_tmap.png")

install.packages("mapview")
install.packages("webshot2")
library(mapview)
mapview_obj = mapview(world, zcol = "lifeExp", legend = TRUE)
mapshot2(mapview_obj, url = "my_interactive_map.html")

