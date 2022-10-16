
# Load Packages -----------------------------------------------------------

library(tidyverse)
library(terra)

# Read in data ------------------------------------------------

# temperature raster data
avg_temp <- terra::rast("lab-1/data/Global Avg Temp.tif")
avg_temp
names(avg_temp) <- "Global_Avg_Temp" # rename column to have underscores instead of spaces
summary(avg_temp)

# states shapefile
states <- terra::vect("lab-1/data/USA_Shapefiles/states.shp")
states
summary(states)
nrow(states) # 51

# counties shapefile
counties <- terra::vect("lab-1/data/USA_Shapefiles/counties.shp")
counties
summary(counties)
nrow(counties) # 3141

# Join States and Counties by FIPS Code -----------------------------------

st_cnty <- merge(x=as.data.frame(states),y=as.data.frame(counties),by="STATE_FIPS",all=TRUE)
# nrow = 3141

st_cnty %>% group_by(SUB_REGION) %>% count() # 150 counties in Middle Atlantic

cnty_per_state <- st_cnty %>% group_by(STATE_NAME.x) %>% summarize(area=mean(area_km),num_counties=n())

cnty_per_state %>% ggplot(aes(x=area,y=num_counties)) + geom_point()
  

# Crop Temperature Data ---------------------------------------------------

# project temperature data to the same CRS
# original temp: +proj=merc +a=6378137 +b=6378137 +lat_ts=0 +lon_0=0 +x_0=0 +y_0=0 +k=1 +units=m +nadgrids=@null +wktext +no_defs
# original states/counties: +proj=eqdc +lat_0=40 +lon_0=-96 +lat_1=20 +lat_2=60 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs

proj <- crs(states,proj=TRUE) # returns proj4 string
temp_eqdc <- terra::project(avg_temp,proj) # project temp raster to NAD
crs(temp_eqdc,proj=TRUE)

# clip to extent of the states shapefile
temp_crop <- crop(temp_eqdc, states)

# ggplot needs rasters to be dataframes, but the base plot() does not and it actually messes things up
#temp_crop <- as.data.frame(temp_crop,xy=TRUE)

# plot states
plot(states)

# plot temps
plot(temp_crop)

# plot states and temps together
plot(temp_crop)
lines(states) # adds outline of states to the raster map

# Enhancing the Plot ------------------------------------------------------


# Focal Statistics --------------------------------------------------------
# Use the “Focal Statistics” tool to find the average temperature using a neighborhood of eight pixels.


# Zonal Statistics --------------------------------------------------------
# calculate the average temperature in each state

# extract point values for average temperature per state
ex <- terra::extract(x=temp_crop, y=states, fun='mean', na.rm=TRUE, xy=TRUE)
str(ex)

# add back state names
state_avg_temp <- cbind(ex,states$STATE_NAME)

# the above is just point values, so it can't be mapped as a grid of temp values

# Point Statistics --------------------------------------------------------
# extract the temperature value from the raster grid to the corresponding U.S. point features for major US cities

cities <- terra::vect("lab-1/data/USA_Major_Cities/USA_Major_Cities.shp")
cities
crs(cities,proj=TRUE) # longlat, WGS84

# project cities to match the other datasets
cities_eqdc <- terra::project(cities,proj)

ex <- terra::extract(x=temp_crop, y=cities_eqdc, xy=TRUE)
str(ex)
city_temp <- cbind(ex,cities$NAME)
