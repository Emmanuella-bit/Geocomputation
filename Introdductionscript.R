#Geocomputing with R
#Introduction test script

install.packages("leaflet")
library(leaflet)
popup = c("Robin", "Jakub", "Jannes")
leaflet() |>
  addProviderTiles("NASAGIBS.ViirsEarthAtNight2012") |>
  addMarkers(lng = c(-3, 23, 11),
             lat = c(52, 53, 49), 
             popup = popup)


#what is Geocomputation
# Geocomputation is about using the various different types of geodata and about developing relevant geo-tools within the overall context of a ‘scientific’ approach”
# Geocomputation with R goes beyond data analysis and modeling to include the development of new tools and methods for work that is not just interesting academically but beneficial
# Needs R packages such as spData, 

# what tools do you use for geospatial analysis or  work in  your daily life 
#what feature can be used to represent a country, or population. Ans:there is no absolute or specific feature allocated for it depends on resolution or scale and the interest of the end user or the information you are trying to communicate. 

install.packages("remotes")
install.packages("geocompkg", 
                 repos = c("https://geocompr.r-universe.dev",
                           "https://cloud.r-project.org"), 
                 dependencies = TRUE, force = TRUE)

bookdown::serve_book(".")
