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

qgis_show_help("grass:v.clean")
