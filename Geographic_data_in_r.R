#Geographic data in R
#learning objective: learning the fundamental data models: **vector** and **raster**
                    #exploring *sf* and *terra* for each

# Introduction
#A model is aw way to represent reality:
#Vector data model: it uses points, lines and polygons. dominates social science. Uses SF package. 
# Vector data is based on CRS (coordinate reference system).coordinates consist of two numbers representing distance from an origin.
# The sf package provides classes for geographic vector data and a consistent command line interface to important low-level libraries for geocomputation:
#GDAL, for reading, writing and manipulating a wide range of geographic data formats, 
#PROJ, a powerful library for coordinate system transformations, which underlies the content 
#GEOS, a planar geometry engine for operations such as calculating buffers and centroids on data with a projected CRS, Planar geometry engines such as GEOS assume ‘flat’ (projected) coordinates
#S2, a spherical geometry engine written in C++ developed by Google, via the s2 package. spherical geometry engines such as S2 assume unprojected (lon/lat) coordinates 


#Raster data model: it divides the surface into cells (pixel) of constant size (ie ariel photography, remote sensing) dominates environmental science. Uses terra packages

#loading of packages
sessionInfo()

install.packages("sf") # SF=Simple feature is an open standard developed and endorsed by the Open Geospatial Consortium (OGC), a not-for-profit organization. Simple features is a hierarchical data model that represents a wide range of geometry types. Of 18 geometry types supported by the specification, only seven (points, linestring, polygone, multipolygon, multilinesstring,multipoint and geometrycollection) are used in the vast majority of geographic research
install.packages("terra")
install.packages("spData")
install.packages("spDataLarge", repos = "https://geocompr.r-universe.dev")

library(sf)
library(terra)
library(spData)
library(spDataLarge)

##vector data
#Info: In R a vector is a class with type
#Geographic vector data model is base in points located with a CRS EG center of london, a bus stop, tree in a park etc

#introduction to simple features

vignette(package = "sf") # see which vignettes are available
vignette("sf1")          # an introduction to the package

View(world)

class(world)

names(world)

plot(world)

unique(world$name_long)
summary(world["lifeExp"]) #The word MULTIPOLYGON in the summary output above refers to the geometry type of features (countries) in the world object.
#The result provides a quick summary of both the non-spatial and spatial data contained in world: the mean average life expectancy is 71 years (ranging from less than 51 to more than 83 years with a median of 73 years) across all countries.

world_mini = world [1:2, 1:3]
world_mini 

#why SF: There are many reasons (linked to the advantages of the simple features model):
#Fast reading and writing of data
#Enhanced plotting performance
#sf objects can be treated as data frames in most operations
#sf function names are relatively consistent and intuitive (all begin with st_)
#sf functions can be combined with the |> operator and works well with the tidyverse collection of R packages.

#read_sf(), #a function for importing geographic vector data covered in detail in Section 8.3.1. 
#st_read (), #returns attributes stored in a base R data.frame (and which emits verbose messages, not shown in the code chunk below), read_sf() silently returns data as a tidyverse tibble

world_dfr = st_read(system.file("shapes/world.gpkg", package = "spData"))
world_tbl = read_sf(system.file("shapes/world.gpkg", package = "spData"))
class(world_dfr)
class(world_tbl)



#Basic maps mapping
#Basic geographic visualizations (maps) are created in sf with base R’s plot() function.
plot(world[3:6])
plot(world["pop"])

world_africa = world[world$continent == "Africa", ]
africa = st_union(world_africa)

plot(world["pop"], reset = FALSE)
plot(africa, add = TRUE, col = "red") ########### ask linus to explain this key, true and false concept clearly for me from the book

plot(world["continent"], reset = FALSE)
cex = sqrt(world$pop) / 10000
world_cents = st_centroid(world, of_largest = TRUE)
plot(st_geometry(world_cents), add = TRUE, cex = cex)

india = world[world$name_long == "India", ]
plot(st_geometry(india), expandBB = c(0, 0.2, 0.1, 1), col = "orange", lwd = 3)
plot(st_geometry(world_asia), add = TRUE)

China = world[world$name_long == "China", ]
plot(st_geometry(China), col = "orange", lwd = 3)
plot(st_geometry(world_asia), add = TRUE)


# Geometry types
"Geometries are the basic building blocks of simple features. Simple features in R can take on one of the 18 geometry types supported by the sf package."

#SF class: Simple features consist of two main parts: geometries and non-geographic attributes.
#Non-geographic attributes represent the name of the feature or other attributes such as measured values, groups, and other things.
"To illustrate attributes, we will represent a temperature of 25°C in London on June 21, 2023. This example contains a geometry (coordinates), and three attributes with three different classes (place name, temperature and date).13 Objects of class sf represent such data by combining the attributes (data.frame) with the simple feature geometry column (sfc). They are created with st_sf() as illustrated below, which creates the London example described above:"
lnd_point = st_point(c(0.1, 51.5))
lnd_geom = st_sfc(lnd_point, crs = "EPSG:4326")
lnd_attrib = data.frame(
  name = "London",
  temperature = 25,
  data = as.Date("2023-06-21")
)
lnd_sf = st_sf(lnd_attrib, geometry = lnd_geom)
lnd_sf
class(lnd_sf)

####Simple feature geometries (sfg): The sfg class represents the different simple feature geometry types in R: point, linestring, polygon (and their ‘multi’ equivalents, such as multipoints) or geometry collection.

"Usually you are spared the tedious task of creating geometries on your own since you can simply import an already existing spatial file. However, there are a set of functions to create simple feature geometry objects (sfg) from scratch, if needed. The names of these functions are simple and consistent, as they all start with the st_ prefix and end with the name of the geometry type in lowercase letters:

#A point: st_point()
A linestring: st_linestring()
A polygon: st_polygon()
A multipoint: st_multipoint()
A multilinestring: st_multilinestring()
A multipolygon: st_multipolygon()
A geometry collection: st_geometrycollection()
sfg objects can be created from three base R data types:
  
  A numeric vector: a single point
A matrix: a set of points, where each row represents a point, a multipoint or linestring
A list: a collection of objects such as matrices, multilinestrings or geometry collections
The function st_point() creates single points from numeric vectors:"

st_point(c(5, 2))                 # XY point
st_point(c(5, 2, 3))              # XYZ point
st_point(c(5, 2, 1), dim = "XYM") # XYM point
st_point(c(5, 2, 3, 1))           # XYZM point

# the rbind function simplifies the creation of matrices
## MULTIPOINT
multipoint_matrix = rbind(c(5, 2), c(1, 3), c(3, 4), c(3, 2))
st_multipoint(multipoint_matrix)
## LINESTRING
linestring_matrix = rbind(c(1, 5), c(4, 4), c(4, 1), c(2, 2), c(3, 2))
st_linestring(linestring_matrix)


## POLYGON: using lists for the creation of multilinestrings, (multi-)polygons and geometry collections:
polygon_list = list(rbind(c(1, 5), c(2, 2), c(4, 1), c(4, 4), c(1, 5)))
st_polygon(polygon_list)

## POLYGON with a hole
polygon_border = rbind(c(1, 5), c(2, 2), c(4, 1), c(4, 4), c(1, 5))
polygon_hole = rbind(c(2, 4), c(3, 4), c(3, 3), c(2, 3), c(2, 4))
polygon_with_hole_list = list(polygon_border, polygon_hole)
st_polygon(polygon_with_hole_list)

## MULTILINESTRING
multilinestring_list = list(rbind(c(1, 5), c(4, 4), c(4, 1), c(2, 2), c(3, 2)), 
                            rbind(c(1, 2), c(2, 4)))
st_multilinestring(multilinestring_list)

## MULTIPOLYGON
multipolygon_list = list(list(rbind(c(1, 5), c(2, 2), c(4, 1), c(4, 4), c(1, 5))),
                         list(rbind(c(0, 2), c(1, 2), c(1, 3), c(0, 3), c(0, 2))))
st_multipolygon(multipolygon_list)

## GEOMETRYCOLLECTION
geometrycollection_list = list(st_multipoint(multipoint_matrix),
                               st_linestring(linestring_matrix))
st_geometrycollection(geometrycollection_list)


####Simple feature columns (sfc): One sfg object contains only a single simple feature geometry. A simple feature geometry column (sfc) is a list of sfg objects, which is additionally able to contain information about the CRS in use. 
#For instance, to combine two simple features into one object with two features, we can use the st_sfc() function. This is important since sfc represents the geometry column in sf data frames:
# sfc POINT
point1 = st_point(c(5, 2))
point2 = st_point(c(1, 3))
points_sfc = st_sfc(point1, point2)
points_sfc

# sfc POLYGON
polygon_list1 = list(rbind(c(1, 5), c(2, 2), c(4, 1), c(4, 4), c(1, 5)))
polygon1 = st_polygon(polygon_list1)
polygon_list2 = list(rbind(c(0, 2), c(1, 2), c(1, 3), c(0, 3), c(0, 2)))
polygon2 = st_polygon(polygon_list2)
polygon_sfc = st_sfc(polygon1, polygon2)
st_geometry_type(polygon_sfc)



# sfc MULTILINESTRING
multilinestring_list1 = list(rbind(c(1, 5), c(4, 4), c(4, 1), c(2, 2), c(3, 2)), 
                             rbind(c(1, 2), c(2, 4)))
multilinestring1 = st_multilinestring((multilinestring_list1))
multilinestring_list2 = list(rbind(c(2, 9), c(7, 9), c(5, 6), c(4, 7), c(2, 7)), 
                             rbind(c(1, 7), c(3, 8)))
multilinestring2 = st_multilinestring((multilinestring_list2))
multilinestring_sfc = st_sfc(multilinestring1, multilinestring2)
st_geometry_type(multilinestring_sfc)

# sfc GEOMETRY (creating sfc objects fro sfg objects with different geometry types)
point_multilinestring_sfc = st_sfc(point1, multilinestring1)
st_geometry_type(point_multilinestring_sfc)

## note: sfc objects can additionally store information on the CRS. The default value is NA (Not Available), as can be verified with st_crs()
st_crs (points_sfc)

# Set the CRS with an identifier referring to an 'EPSG' CRS code:
points_sfc_wgs = st_sfc(point1, point2, crs = "EPSG:4326")
st_crs(points_sfc_wgs)

#The sfheaders package : sfheaders is an R package that speeds-up the construction, conversion and manipulation of sf objects
# The simplest use case for sfheaders is demonstrated in the code chunks below with examples of building sfg, sfc, and sf objects showing:
"A vector converted to sfg_POINT
A matrix converted to sfg_LINESTRING
A data frame converted to sfg_POLYGON"


v = c(1, 1)
v_sfg_sfh = sfheaders::sfg_point(obj = v)
v_sfg_sfh # printing without sf loaded

v_sfg_sf = st_point(v)
print(v_sfg_sf) == print(v_sfg_sfh)

# matrices :how sfheaders create sfg objects from matrices and dataframes
m = matrix(1:8, ncol = 2)
sfheaders::sfg_linestring(obj = m)

# data frames
df = data.frame(x = 1:4, y = 4:1)
sfheaders::sfg_polygon(obj = df)

sfheaders::sfc_point(obj = v)
sfheaders::sfc_linestring(obj = m)
sfheaders::sfc_polygon(obj = df)

sfheaders::sf_point(obj = v)
sfheaders::sf_linestring(obj = m)
sfheaders::sf_polygon(obj = df)

df_sf = sfheaders::sf_polygon(obj = df)
st_crs(df_sf) = "EPSG:4326"

# Spherical geometry operations with S2
"Spherical geometry engines are based on the fact that the world is round, while simple mathematical procedures for geocomputation, such as calculating a straight line between two points or the area enclosed by a polygon, assume planar (projected) geometries. Since sf version 1.0.0, R supports spherical geometry operations ‘out of the box’ (and by default), thanks to its interface to Google’s S2 spherical geometry engine via the s2 interface package . S2 is perhaps best known as an example of a Discrete Global Grid System (DGGS). Another example is the H3 global hexagonal hierarchical spatial index"
sf_use_s2()
india_buffer_with_s2 = st_buffer(india, 1) # 1 meter
sf_use_s2(FALSE)
#> Spherical geometry (s2) switched off
india_buffer_without_s2 = st_buffer(india, 1)

sf_use_s2(TRUE)






#Raster data
"The spatial raster data model represents the world with the continuous grid of cells (often also called pixels; Figure 2.13:A). This data model often refers to so-called regular grids, in which each cell has the same, constant size – and we will focus on the regular grids in this book only. However, several other types of grids exist, including rotated, sheared, rectilinear, and curvilinear grids"
"The raster data model usually consists of a raster header and a matrix (with rows and columns) representing equally spaced cells"
"The raster header defines the CRS, the extent and the origin. The origin (or starting point) is frequently the coordinate of the lower left corner of the matrix (the terra package, however, uses the upper left corner"
"The header defines the extent via the number of columns, the number of rows and the cell size resolution.

The resolution can be calculated as follows:

resolution= (xmax − xmin) /ncol, (ymax − ymin) /nrow"


#Introduction to terra
raster_filepath = system.file("raster/srtm.tif", package = "spDataLarge")
my_rast = rast(raster_filepath)
class(my_rast)

my_rast
plot(my_rast)

single_raster_file = system.file("raster/srtm.tif", package = "spDataLarge")
single_rast = rast(raster_filepath)

new_raster = rast(nrows = 6, ncols = 6, 
                  xmin = -1.5, xmax = 1.5, ymin = -1.5, ymax = 1.5,
                  vals = 1:36)

#Basic map-making
plot(my_rast)

single_raster_file = system.file("raster/srtm.tif", package = "spDataLarge")
single_rast = rast(raster_filepath)

new_raster = rast(nrows = 6, ncols = 6, 
                  xmin = -1.5, xmax = 1.5, ymin = -1.5, ymax = 1.5,
                  vals = 1:36)
multi_raster_file = system.file("raster/landsat.tif", package = "spDataLarge")
multi_rast = rast(multi_raster_file)
multi_rast
nlyr(multi_rast)

multi_rast3 = subset(multi_rast, 3)
multi_rast4 = subset(multi_rast, "landsat_4")

multi_rast34 = c(multi_rast3, multi_rast4)

###Coordinate Reference Systems


## Units
luxembourg = world[world$name_long == "Luxembourg", ]
st_area(luxembourg)
st_area(luxembourg) / 1000000
units::set_units(st_area(luxembourg), km^2)
res(my_rast)

repr = project(my_rast, "EPSG:26912")
res(repr)



############################## Exercise Geographic data in R########################################





