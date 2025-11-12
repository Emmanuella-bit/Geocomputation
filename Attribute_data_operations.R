###########Attribute data operations###########

library(sf)      # vector data package introduced in Chapter 2
library(terra)   # raster data package introduced in Chapter 2
library(dplyr)   # tidyverse package for data frame manipulation
library(spData)

# Attribute data is nonspatial information associated with geographic (geometry) data. 






##vector attribute manipulation 
methods(class = "sf")
class(world)
dim(world)

world_df = st_drop_geometry(world)
class(world_df)

ncol(world_df)
world[1:6, ]    # subset rows by position
world[, 1:3]    # subset columns by position
world[1:6, 1:3] # subset rows and columns by position
world[, c("name_long", "pop")] # columns by name
world[, c(T, T, F, F, F, F, F, T, T, F, F)] # by logical indices
world[, 888] # an index representing a non-existent column

###using logical vectors for subsetting
i_small = world$area_km2 < 10000
summary(i_small) 
small_countries = world [i_small, ]
####The intermediary i_small (short for index representing small countries) is a logical vector that can be used to subset the seven smallest countries in the world by surface area. 

small_countries = world[world$area_km2 < 10000, ]
small_countries = subset(world, area_km2 < 10000)

### select() selects columns by name or position. For example, you could select only two columns, name_long and pop, with the following command
world1 = select(world, name_long, pop)
names(world1)

####select() also allows selecting a range of columns with the help of the : operator:
world2 = select(world, name_long:pop) #reveals all columns between name_long and pop (both inclusive)
names(world2)

#### remove specific columns with the - operator:
world3 = select(world, -subregion, -area_km2, -continent, -pop)
names(world3)

####Subset and rename columns at the same time with the new_name = old_name syntax:
world4 = select(world, name_long, population = pop) ## noting that the command above is more concise than base R equivalent, which requires two lines of code ie name_long, population=pop
names(world4)

#### 'pull' can also be used to extract a single column  as '$'  and '[[' ie ....
pull(world, pop)
world$pop
world[["pop"]] ###all are used to extract the column 'pop'

#####slice() is the row-equivalent of select()
slice(world, 1:6) #this selects rows from 1 to 6

##  filter () is also the same as subset () function ie....
world7 = filter(world, area_km2 < 1000)
world7 = filter(world, lifeExp > 82)


############# chaining commands with pipes 'pipe'##############
world7 = world |>
  filter(continent == "Asia") |> #### 'pipe' function enables chaining
  select(name_long, continent) |>
  slice(1:5)

world8 = slice (
  select (
    filter (world, continent == "Asia"),
    name_long, continent),
  1:5)
 
#################### vector attribute aggregation#################
## summarizing data with one or more grouping variables
 world_agg1 = aggregate(pop ~ continent, FUN = sum, data = world,
                        na.rm = TRUE)
class(world_agg1)

world_agg2 = aggregate(world["pop"], by = list(world$continent), FUN = sum, 
                       na.rm = TRUE)
class(world_agg2)
nrow(world_agg2) ## note world_agg2 object is a spatial object containing eight features representing the continents of world

## group_by () |> summarize () is the same as aggregate () ie..........
world_agg3 = world |> 
  group_by(continent) |>
  summarize(pop = sum(pop, na.rm = TRUE))

##### also using this to calculate the population, area and number of countries in each continent
world_agg4 = world |>
  group_by(continent) |>
  summarize(Pop = sum(pop, na.rm = TRUE), Area = sum(area_km2), N = n())


#### combining all dplyr functions
world_agg5 = world |>
  st_drop_geometry () |>
  select(pop, continent, area_km2) |>
  group_by(Continent = continent) |>
  summarize(Pop = sum(pop, na.rm = TRUE), Area = sum(area_km2), N = n ()) |>
            mutate(Density = round(Pop/Area))|>
              slice_max(Pop, n = 3) |>
              arrange(desc(N))
names(world_agg5)

####################Vector attribute joining#############################
#combine data from different sources##
world_coffee = left_join(world, coffee_data)
class(world_coffee)
names(world_coffee)
plot(world_coffee["coffee_production_2017"])

##### rename the key variable for/in one of the objects
coffee_renamed = rename(coffee_data, nm = name_long)
world_coffee2 = left_join(world, coffee_renamed, by = join_by(name_long == nm))

world_coffee_inner = inner_join(world, coffee_data)
nrow(world_coffee_inner)

#### identify rows that do not match using setdiff() function
setdiff(coffee_data$name_long, world$name_long)

drc = stringr :: str_subset(world$name_long, "Dem*.+Congo")
drc

#### to solve the issue, we create a new version of coffee_data and then update the name
coffee_data$name_long[grepl("Congo,", coffee_data$name_long)] = drc
world_coffee_match = inner_join(world, coffee_data)
nrow(world_coffee_match)


coffee_world = left_join(coffee_data, world)
class(coffee_world)


unique(world$name_long)

AFC = world %>%
  filter(continent == "Africa") %>%
  select(name_long, continent) %>%
  rename(country = name_long)
AFC

help(rename)

################################creating attributes and removing spatial information ####################
###  calculating population density for each country
### to do: divide a population column, here pop, by an area column, here area_km2 with unit area in square kilometers
world_new = world
world_new$pop_dens = world_new$pop / world_new$area_km2
world_new

#### Alternatively, using one of dplyr functions: mutate() or transmute(). mutate() adds new columns at the penultimate position in the sf object (the last one is reserved for the geometry)
world_new2 = world |>
  mutate(pop_dense = pop/area_km2)
world_new2

#### using the unite()
world_unite = world |>
  tidyr::unite("con_reg", continent:region_un, sep = ":", remove = TRUE) ## note: The resulting sf object has a new column called con_reg representing the continent and region of each country, e.g., South America:Americas for Argentina and other South America countries.

### using separate() function to split column into multiple columns
world_separate = world_unite |>
  tidyr::separate(con_reg, c("continent", "region_un"), sep = ":")

#### using rename() function and setNames() for renaming columns. first replace oldname with new name
world |>
  rename(name = name_long)

#oder
new_names = c("i", "n",  "c", "r", "s", "t", "a", "p", "l", "gP", "geom")
world_new_names = world |>
  setNames(new_names)

####using st_drop_geometry() to remove the geometry and speedup aggregation
world_data = world |> st_drop_geometry()
class(world_data)


#########################################Manipulating raster objects################################
elev = rast(nrows = 6, ncols = 6,
            xmin = -1.5, xmax = 1.5, ymin = -1.5, ymax = 1.5,
            vals = 1:36)

grain_order = c("clay", "silt", "sand")
grain_char = sample(grain_order, 36, replace = TRUE)
grain_fact = factor(grain_char, levels = grain_order)
grain = rast(nrows = 6, ncols = 6, 
             xmin = -1.5, xmax = 1.5, ymin = -1.5, ymax = 1.5,
             vals = grain_fact)

grain2 = grain # not overwrite the original data
levels(grain2) = data.frame(value = c(0, 1, 2), wetness = c("wet", "moist", "dry"))
levels(grain2)
print(grain2)

###### rasta subsetting ######
elev[1, 1]
elev[1]

elev[1, 1] = 0
elev[]

elev[1, c(1, 2)] = 0

two_layers = c(grain, elev) 
two_layers[1] = cbind(c(1), c(4))
two_layers[]

#####summarizing raster objects
global(elev, sd)
freq(grain)

### hist(elev)
