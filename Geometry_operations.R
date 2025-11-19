######################### Geometry Operations######################################
library(sf)
library(terra)
library(dplyr)
library(spData)
library(spDataLarge)

#### Geometric operations on vector data
### simplification
#using st_simplify ()

seine_simp = st_simplify(seine, dTolerance = 2000)  # 2000 m

object.size(seine)
object.size(seine_simp)

us_states_simp2 = rmapshaper::ms_simplify(us_states, keep = 0.01,
                                          keep_shapes = TRUE)

us_states_simp3 = smoothr::smooth(us_states, method = "ksmooth", smoothness = 6)

nz_centroid = st_centroid(nz)
seine_centroid = st_centroid(seine)

nz_pos = st_point_on_surface(nz)
seine_pos = st_point_on_surface(seine)


##### Buffers
seine_buff_5km = st_buffer(seine, dist = 5000)
seine_buff_50km = st_buffer(seine, dist = 50000)


########### Affine transformations
nz_sfc = st_geometry(nz)

nz_shift = nz_sfc + c(0, 100000)

nz_centroid_sfc = st_centroid(nz_sfc)
nz_scale = (nz_sfc - nz_centroid_sfc) * 0.5 + nz_centroid_sfc

rotation = function(a){
  r = a * pi / 180 #degrees to radians
  matrix(c(cos(r), sin(r), -sin(r), cos(r)), nrow = 2, ncol = 2)
} 

nz_rotate = (nz_sfc - nz_centroid_sfc) * rotation(30) + nz_centroid_sfc
plot(nz_rotate)

nz_scale_sf = st_set_geometry(nz, nz_scale)


####### clipping#########
b = st_sfc(st_point(c(0, 1)), st_point(c(1, 1))) # create 2 points
b = st_buffer(b, dist = 1) # convert points to circles
plot(b, border = "gray")
text(x = c(-0.5, 1.5), y = 1, labels = c("x", "y"), cex = 3) # add text

x = b[1]
y = b[2]
x_and_y = st_intersection(x, y)
plot(b, border = "gray")
plot(x_and_y, col = "lightgray", border = "gray", add = TRUE) # intersecting area



############ Subsetting and clipping #####
bb = st_bbox(st_union(x, y))
box = st_as_sfc(bb)
set.seed(2024)
p = st_sample(x = box, size = 10)
p_xy1 = p[x_and_y]
plot(box, border = "gray", lty = 2)
plot(x, add = TRUE, border = "gray")
plot(y, add = TRUE, border = "gray")
plot(p, add = TRUE, cex = 3.5)
plot(p_xy1, cex = 5, col = "red", add = TRUE)
text(x = c(-0.5, 1.5), y = 1, labels = c("x", "y"), cex = 3)

bb = st_bbox(st_union(x, y))
box = st_as_sfc(bb)
set.seed(2024)
p = st_sample(x = box, size = 10)
x_and_y = st_intersection(x, y)

# way #1
p_xy1 = p[x_and_y]
# way #2
p_xy2 = st_intersection(p, x_and_y)
# way #3
sel_p_xy = st_intersects(p, x, sparse = FALSE)[, 1] & 
  st_intersects(p, y, sparse = FALSE)[, 1]
p_xy3 = p[sel_p_xy]

####### Geometry unions
regions = aggregate(x = us_states[, "total_pop_15"], by = list(us_states$REGION),
                    FUN = sum, na.rm = TRUE)
regions2 = us_states |> 
  group_by(REGION) |>
  summarize(pop = sum(total_pop_15, na.rm = TRUE))

us_west = us_states[us_states$REGION == "West", ]
us_west_union = st_union(us_west)

texas = us_states[us_states$NAME == "Texas", ]
texas_union = st_union(us_west_union, texas)


##### Type transformations
multipoint = st_multipoint(matrix(c(1, 3, 5, 1, 3, 1), ncol = 2))

linestring = st_cast(multipoint, "LINESTRING")
polyg = st_cast(multipoint, "POLYGON")

multipoint_2 = st_cast(linestring, "MULTIPOINT")
multipoint_3 = st_cast(polyg, "MULTIPOINT")

all.equal(multipoint, multipoint_2)

all.equal(multipoint, multipoint_3)


multilinestring_list = list(matrix(c(1, 4, 5, 3), ncol = 2), 
                            matrix(c(4, 4, 4, 1), ncol = 2),
                            matrix(c(2, 4, 2, 2), ncol = 2))
multilinestring = st_multilinestring(multilinestring_list)
multilinestring_sf = st_sf(geom = st_sfc(multilinestring))
multilinestring_sf

linestring_sf2 = st_cast(multilinestring_sf, "LINESTRING")
linestring_sf2

linestring_sf2$name = c("Riddle Rd", "Marshall Ave", "Foulke St")
linestring_sf2$length = st_length(linestring_sf2)
linestring_sf2


########### Geometric operations on raster data####################
elev = rast(system.file("raster/elev.tif", package = "spData"))
clip = rast(xmin = 0.9, xmax = 1.8, ymin = -0.45, ymax = 0.45,
            resolution = 0.3, vals = rep(1, 9))
elev[clip, drop = FALSE]
plot(elev)

###extent and origin
elev = rast(system.file("raster/elev.tif", package = "spData"))
elev_2 = extend(elev, c(1, 2))
plot(elev_2)

elev_3 = elev + elev_2
elev_4 = extend(elev, elev_2)

origin(elev_4)

# change the origin
origin(elev_4) = c(0.25, 0.25)
plot(elev_4)


##### Aggregation and disaggregation ###########
dem = rast(system.file("raster/dem.tif", package = "spDataLarge"))
dem_agg = aggregate(dem, fact = 5, fun = mean)
plot(dem_agg)

dem_disagg = disagg(dem_agg, fact = 5, method = "bilinear")
identical(dem, dem_disagg)

########### Resampling
target_rast = rast(xmin = 794650, xmax = 798250, 
                   ymin = 8931750, ymax = 8935350,
                   resolution = 300, crs = "EPSG:32717")

dem_resampl = resample(dem, y = target_rast, method = "bilinear")




##########################################summary of learning outcome###################################
#1. st_buffer () creates a region around a point, line, or pol
