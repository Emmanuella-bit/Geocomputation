########## Raster_Vector interactions###################
library(sf)
library(terra)
library(dplyr)

srtm = rast(system.file("raster/srtm.tif", package = "spDataLarge"))
zion = read_sf(system.file("vector/zion.gpkg", package = "spDataLarge"))
zion = st_transform(zion, st_crs(srtm))

srtm_cropped = crop(srtm, zion)

srtm_masked = mask(srtm, zion)

srtm_cropped = crop(srtm, zion)
srtm_final = mask(srtm_cropped, zion)

srtm_inv_masked = mask(srtm, zion, inverse = TRUE)
plot (srtm_inv_masked)


#### Raster extraction###
data("zion_points", package = "spDataLarge")
elevation = terra::extract(srtm, zion_points)
zion_points = cbind(zion_points, elevation)
plot(zion_points)


zion_transect = cbind(c(-113.2, -112.9), c(37.45, 37.2)) |>
  st_linestring() |> 
  st_sfc(crs = crs(srtm)) |>
  st_sf(geometry = _)

##using sr_segmentize()
zion_transect$id = 1:nrow(zion_transect)
zion_transect = st_segmentize(zion_transect, dfMaxLength = 250)
zion_transect = st_cast(zion_transect, "POINT")

zion_transect = zion_transect |> 
  group_by(id) |> 
  mutate(dist = st_distance(geometry)[, 1])

zion_elev = terra::extract(srtm, zion_transect)
zion_transect = cbind(zion_transect, zion_elev)
plot(zion_elev)
plot(zion_transect)

zion_srtm_values = terra::extract(x = srtm, y = zion)

group_by(zion_srtm_values, ID) |> 
  summarize(across(srtm, list(min = min, mean = mean, max = max)))

nlcd = rast(system.file("raster/nlcd.tif", package = "spDataLarge"))
zion2 = st_transform(zion, st_crs(nlcd))
zion_nlcd = terra::extract(nlcd, zion2)
zion_nlcd |> 
  group_by(ID, levels) |>
  count()
plot(zion_nlcd)

cycle_hire_osm = spData::cycle_hire_osm
cycle_hire_osm_projected = st_transform(cycle_hire_osm, "EPSG:27700")
raster_template = rast(ext(cycle_hire_osm_projected), resolution = 1000,
                       crs = crs(cycle_hire_osm_projected))

ch_raster1 = rasterize(cycle_hire_osm_projected, raster_template)
plot(ch_raster1)
ch_raster2 = rasterize(cycle_hire_osm_projected, raster_template, 
                       fun = "length")
plot(ch_raster2)
ch_raster3 = rasterize(cycle_hire_osm_projected, raster_template, 
                       field = "capacity", fun = sum, na.rm = TRUE)
plot(ch_raster3)

####
california = dplyr::filter(us_states, NAME == "California")
california_borders = st_cast(california, "MULTILINESTRING")
raster_template2 = rast(ext(california), resolution = 0.5,
                        crs = st_crs(california)$wkt)
plot(california)

california_raster1 = rasterize(california_borders, raster_template2,
                               touches = TRUE)
plot(california_raster1)

california_raster2 = rasterize(california, raster_template2)
plot(california_raster2)


############Spatial vectorization###########
# spatial vectorization is the counterpart of rasterization but in the opposite direction.  it involves converting spatially continuous raster data into spatially discrete v3ector data such as points, lines or polygons
elev = rast(system.file("raster/elev.tif", package = "spData"))
elev_point = as.points(elev) |> 
  st_as_sf()
plot(elev)
elev = rast(system.file("raster/elev.tif", package = "spData"))
elev_point = as.points(elev) |> 
  st_as_sf()
plot(elev_point)

##creating contour  lines can be created  with the terra function as.contour(),filled.contour() 
dem = rast(system.file("raster/dem.tif", package = "spDataLarge"))
cl = as.contour(dem) |> 
  st_as_sf()
plot(dem, axes = FALSE)
plot(cl, add = TRUE)

grain = rast(system.file("raster/grain.tif", package = "spData"))
grain_poly = as.polygons(grain) |> 
  st_as_sf()
plot(grain)
plot(grain_poly)


#####################################summary of learning outcome####################################