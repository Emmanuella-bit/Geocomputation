###########Exercise############

library(sf)
library(spData)
library(terra)


" E1. Use summary() on the geometry column of the world data object that is included in the spData package. What does the output tell us about:
  
  Its geometry type?
  The number of countries?
  Its coordinate reference system (CRS)?"


summary(world)
plot(world["continent"], reset = FALSE)
cex = sqrt(world$pop) / 10000
world_cents = st_centroid(world, of_largest = TRUE)
plot(st_geometry(world_cents), add = TRUE, cex = cex)


"E2. Run the code that ‘generated’ the map of the world in Section 2.2.3 (Basic map-making). Find two similarities and two differences between the image on your computer and that in the book.

What does the cex argument do (see ?plot)?
Why was cex set to the sqrt(world$pop) / 10000?
Bonus: experiment with different ways to visualize the global population."


plot(st_geometry(world_cents), cex = world$pop / 1e9)
plot(st_geometry(world_cents), cex = world$pop / 1e8)
plot(world["pop"])
plot(world["pop"], logz = TRUE)

"E3. Use plot() to create maps of Nigeria in context (see Section 2.2.3).

Adjust the lwd, col and expandBB arguments of plot().
Challenge: read the documentation of text() and annotate the map".

nigeria = world[world$name_long == "Nigeria", ]
plot(st_geometry(nigeria), expandBB = c(0, 0.2, 0.1, 1), col = "green", lwd = 3)
plot(world[0], add = TRUE)
world_coords = st_coordinates(world_cents)
text(world_coords, world$iso_a2)


#E4  Create an empty SpatRaster object called my_raster with 12 columns and 12 rows. Assign random values between 0 and 12 to the new raster and plot it.
my_raster = rast(ncol = 12, nrow = 12,
                 vals = sample(0:11, size = 11 * 11, replace = TRUE))
plot(my_raster)

#E5 Read-in the raster/nlcd.tif file from the spDataLarge package. What kind of information can you get about the properties of this file?
nlcd = rast(system.file("raster/nlcd.tif", package = "spDataLarge"))
dim(nlcd) # dimensions
res(nlcd) # resolution
ext(nlcd) # extent
nlyr(nlcd) # number of layers
cat(crs(nlcd)) # CRS

#E6 E6. Check the CRS of the raster/nlcd.tif file from the spDataLarge package. What kind of information you can learn from it?
cat(crs(nlcd))

