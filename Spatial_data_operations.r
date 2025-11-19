######################Spatial data operations###############################################
library(sf)
library(terra)
library(dplyr)
library(spData)

##### spatial operations on vector data (using the st_intersect alternatoively filter () command)
###### spatial subsetting: the process of taking a spatial object  and returning a new object containing only 

canterbury = nz |> filter(Name == "Canterbury")
canterbury_height = nz_height[canterbury, ]
print(canterbury_height)

nz_height[canterbury, , op = st_disjoint]

sel_sgbp = st_intersects(x = nz_height, y = canterbury)
class(sel_sgbp)

sel_sgbp


sel_logical = lengths(sel_sgbp) > 0
canterbury_height2 = nz_height[sel_logical, ]

canterbury_height3 = nz_height |>
  st_filter(y = canterbury, .predicate = st_intersects)

######Topological relations (st_sfc () or st_as_sf ())

polygon_matrix = cbind(
  x = c(0, 0, 1, 1,   0),
  y = c(0, 1, 1, 0.5, 0)
)
polygon_sfc = st_sfc(st_polygon(list(polygon_matrix)))

point_df = data.frame(
  x = c(0.2, 0.7, 0.4),
  y = c(0.1, 0.2, 0.8)
)
point_sf = st_as_sf(point_df, coords = c("x", "y"))

st_intersects(point_sf, polygon_sfc)

st_intersects(point_sf, polygon_sfc, sparse = FALSE)


###using st_within ()
###using st_touches ()
st_within(point_sf, polygon_sfc)
st_touches(point_sf, polygon_sfc)

## using disjoint ()
st_disjoint(point_sf, polygon_sfc, sparse = FALSE)[, 1]

st_is_within_distance(point_sf, polygon_sfc, dist = 0.2, sparse = FALSE)[, 1]

####Distance relations
## Using st_distance
nz_highest = nz_height |> slice_max(n = 1, order_by = elevation)
canterbury_centroid = st_centroid(canterbury)
st_distance(nz_highest, canterbury_centroid)

co = filter(nz, grepl("Canter|Otag", Name))
st_distance(nz_height[1:3, ], co)

## Using st_geometry 
plot(st_geometry(co)[2])
plot(st_geometry(nz_height)[2:3], add = TRUE)



###### DE-9IM string
##using st_relate ()
xy2sfc = function(x, y) st_sfc(st_polygon(list(cbind(x, y))))
x = xy2sfc(x = c(0, 0, 1, 1, 0), y = c(0, 1, 1, 0.5, 0))
y = xy2sfc(x = c(0.7, 0.7, 0.9, 0.7), y = c(0.8, 0.5, 0.5, 0.8))
st_relate(x, y)

st_queen = function(x, y) st_relate(x, y, pattern = "F***T****")
st_rook = function(x, y) st_relate(x, y, pattern = "F***1****")

###finding the elements in the grid in relation to the middle square of the grid
grid = st_make_grid(x, n = 3)
grid_sf = st_sf(grid)
grid_sf$queens = lengths(st_queen(grid, grid[5])) > 0
plot(grid, col = grid_sf$queens)
grid_sf$rooks = lengths(st_rook(grid, grid[5])) > 0
plot(grid, col = grid_sf$rooks)



#### Spatial Joining
## using st_joining ()
set.seed(2018) # set seed for reproducibility
(bb = st_bbox(world)) # the world's bounds

random_df = data.frame(
  x = runif(n = 10, min = bb[1], max = bb[3]),
  y = runif(n = 10, min = bb[2], max = bb[4])
)
random_points = random_df |> 
  st_as_sf(coords = c("x", "y"), crs = "EPSG:4326") # set coordinates and CRS

world_random = world[random_points, ]
nrow(world_random)

random_joined = st_join(random_points, world["name_long"])


### Distance-based joins
plot(st_geometry(cycle_hire), col = "blue")
plot(st_geometry(cycle_hire_osm), add = TRUE, pch = 3, col = "red")

## checking if any points are the same using st_intersects()
any(st_intersects(cycle_hire, cycle_hire_osm, sparse = FALSE))

sel = st_is_within_distance(cycle_hire, cycle_hire_osm, 
                            dist = units::set_units(20, "m"))
summary(lengths(sel) > 0)

z = st_join(cycle_hire, cycle_hire_osm, st_is_within_distance, 
            dist = units::set_units(20, "m"))
nrow(cycle_hire)
nrow(z)

#####
z = z |> 
  group_by(id) |> 
  summarize(capacity = mean(capacity))
nrow(z) == nrow(cycle_hire)

plot(cycle_hire_osm["capacity"])
plot(z["capacity"])


###### Spatial aggregation (aggregate ())
nz_agg = aggregate(x = nz_height, by = nz, FUN = mean)

nz_agg2 = st_join(x = nz, y = nz_height) |>
  group_by(Name) |>
  summarize(elevation = mean(elevation, na.rm = TRUE))

####### Joining incongruent layers
## congruent
## Incongruent 
## st_interpolate_aw()

iv = incongruent["value"] # keep only the values to be transferred
agg_aw = st_interpolate_aw(iv, aggregating_zones, extensive = TRUE)

agg_aw$value


##### Spatial operations on raster data
elev = rast(system.file("raster/elev.tif", package = "spData"))
grain = rast(system.file("raster/grain.tif", package = "spData"))

### Spatial subsetting
## cellfromXY()
## terra::extract()

id = cellFromXY(elev, xy = matrix(c(0.1, 0.1), ncol = 2))
elev[id]
# the same as
terra::extract(elev, matrix(c(0.1, 0.1), ncol = 2))

clip = rast(xmin = 0.9, xmax = 1.8, ymin = -0.45, ymax = 0.45,
            resolution = 0.3, vals = rep(1, 9))
elev[clip]

elev[1:2, drop = FALSE]    # spatial subsetting with cell IDs

# create raster mask
rmask = elev
values(rmask) = sample(c(NA, TRUE), 36, replace = TRUE)

# spatial subsetting
elev[rmask, drop = FALSE]           # with [ operator

elev[elev < 20] = NA


################ Map algebra############
elev + elev
elev^2
log(elev)
elev > 5

rcl = matrix(c(0, 12, 1, 12, 24, 2, 24, 36, 3), ncol = 3, byrow = TRUE)
rcl

recl = classify(elev, rcl = rcl)

multi_raster_file = system.file("raster/landsat.tif", package = "spDataLarge")
multi_rast = rast(multi_raster_file)

multi_rast = (multi_rast * 0.0000275) - 0.2

multi_rast[multi_rast < 0] = 0

ndvi_fun = function(nir, red){
  (nir - red) / (nir + red)
}

ndvi_rast = lapp(multi_rast[[c(4, 3)]], fun = ndvi_fun)

#### Focal Operation 
r_focal = focal(elev, w = matrix(1, nrow = 3, ncol = 3), fun = min)

### Zonal operations
z = zonal(elev, grain, fun = "mean")
z

### Global operations and distances
##Map algebra counterparts in vector processing
### Merging rasters (merge ())
aut = geodata::elevation_30s(country = "AUT", path = tempdir())
ch = geodata::elevation_30s(country = "CHE", path = tempdir())
aut_ch = merge(aut, ch)



########### Summary ###############################################
## A) Spatial relations (who is where?)
#st_intersects() – overlaps in any way
#Find which rivers intersect with protected areas.
# prob_rivers <- rivers[st_intersects(rivers, protected_areas), ]
# count # nrow(schools_near_hwy)

#st_within() – A is inside B
#eg which schools falls inside  Accra Metropolitan
# schools_in_accra <- schools[st_within(schools, accra_boundary), ]

#st_contains() – B contains A

#st_touches() – boundaries touch or meet

#st_crosses() – lines cross

#st_distance() – how far apart 
#eg.Distance between Cape Coast and Takoradi, st_distance(cape_coast, takoradi)
#Select all hospitals within 5 km of major roads
#


## B) Geometry operations (change the shapes)
#st_buffer() – create zones (e.g. 1 km around a road)
#eg 2 km buffer around highways in Kumasi 
# kma_highway_zone <- st_buffer(highways_kma, dist = 2000)
# steps: buffer roads and use within
#roads_buf <- st_buffer(roads, 5000)
#hospitals_near_road <- hospitals[st_within(hospitals, roads_buf), ]


#st_union() – merge multiple shapes
#eg.Merge all regions into one Ghana polygon ## ghana <- st_union(regions)


#st_intersection() – keep overlapping areas (share space)
#st_intersects(roads, buildings)

#st_difference() – subtract one area from another

#st_crop()/st_clip() – clip to a boundary 
#Clip Ghana population raster to Ashanti region
#pop_ashanti <- terra::crop(pop_gh, ashanti)



## C) Spatial joins (transfer attributes by location)
#Assigning polygon ID  to points, or vice versa
# st_join(points, polygons)

## D) CRS transformations (very important)
#Distance and areas require projected CRS
#st_transform(data, 32630) #this is for Ghana
#eg Calculate the area of each district in square kilometres.
# districts_m <- st_transform(districts, 32630)
#districts_m$area_km2 <- st_area(districts_m) / 1e6
