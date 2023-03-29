## ----setup, include=FALSE---------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
getwd()

library(terra)
library(here)
library(sf)
library(raster)
library(stars)
library(tidyverse)
library(tmap)
library(ggplot2)
library(weathermetrics)

rootdir <- ("/Users/alexreed/Documents/MEDS/Courses/EDS_223")
datadir <- file.path(rootdir,"Assignments", "data","assignment4")
#setwd(file.path(rootdir,"Assignments"))


## ----include=TRUE, warning=FALSE, messages=FALSE----------------------
#read in the shapefile 
wc_regions <- st_read("data/assignment4/wc_regions_clean.shp")
wc_regions <- vect(wc_regions)

#read in SST rasters
sst_2008 <- rast("data/assignment4/average_annual_sst_2008.tif") 
sst_2009 <- rast("data/assignment4/average_annual_sst_2009.tif") 
sst_2010 <- rast("data/assignment4/average_annual_sst_2010.tif") 
sst_2011 <- rast("data/assignment4/average_annual_sst_2011.tif") 
sst_2012 <- rast("data/assignment4/average_annual_sst_2012.tif") 

#combine SST rasters into a raster stack
all_sst <- c(sst_2008,
              sst_2009,
              sst_2010,
              sst_2011,
              sst_2012) 

#removing these object from my environment for clarity 
rm(sst_2008, sst_2009, sst_2010, sst_2011, sst_2012)

#read in bathymetry raster
depth <- rast("data/assignment4/depth.tif") 

all_sst <- terra::project(all_sst, crs(wc_regions))


## ----include=TRUE-----------------------------------------------------
#find the mean SST
all_sst_mean <- mean(all_sst) # doesn't give one value

#convert SST data from Kelvin to Celsius
all_sst_C <- all_sst_mean - 273.15

#crop depth raster to match the extent of the SST raster
depth_crop <- crop(depth, all_sst_C)

#resample the NPP data to match the resolution of the SST data using the nearest neighbor approach
depth_resample <- resample(depth_crop, all_sst_C, method = "near")

#stacked
sst_depth <- c(all_sst_C,
              depth_resample)


## ----include=TRUE-----------------------------------------------------
#sea surface temperature: 11-30°C
#depth: 0-70 meters below sea level

#reclassifying suitable SST
suitable_sst <- matrix(c(0, 11, NA,
                         11, 30, 1,
                         30, Inf, NA),
                       ncol = 3, byrow = TRUE)

suitable_sst <- classify(all_sst_C, rcl = suitable_sst)

#my first approach 
# suitable_sst <- replace(all_sst_C, all_sst_C > 11 | all_sst_C < 30, 1)
# #suitable_sst[is.na(suitable_sst)] <- NA 
# suitable_sst <- replace(all_sst_C, all_sst_C < 11 | all_sst_C > 30, NA)

#reclassifying suitable depth
suitable_depth <- matrix(c(-Inf, -70, NA,
                         -70, 0, 1,
                         0, Inf, NA),
                       ncol = 3, byrow = TRUE)

suitable_depth <- classify(depth_resample, rcl = suitable_depth)

suitable_depth <- project(suitable_depth, crs(suitable_sst))

#converting to dataframe to make sure the reclassification worked
# all_sst_df <- as.data.frame(all_sst_C)
# depth_resample_df <- as.data.frame(depth_resample)
# suitable_sst_df <- as.data.frame(suitable_sst)
# suitable_depth_df <- as.data.frame(suitable_depth)

#stack
suitable_sst_depth <- c(suitable_sst,
                        suitable_depth)

fun_multiply = function(x,y){return(x*y)}
suitable_sst_depth <- lapp(suitable_sst_depth, 
                           fun = fun_multiply)


## ----include=TRUE-----------------------------------------------------
#terra::extract() finds which raster cell each of the points is located within and assigns the value of the cell to the point.
#suitable_wc <- terra::extract(suitable_sst_depth, wc_regions)
#plot(wc_regions)

#rasterize 
nams <- names(wc_regions)

wc_regions_rast <- lapply(nams, function(x) {
  rasterize(wc_regions, suitable_sst_depth,
    field = x,
    touches = TRUE)})

wc_regions_rast <- do.call("c", wc_regions_rast)
#plot(wc_regions_rast)

#projecting
wc_regions_rast <- project(wc_regions_rast, 
                       suitable_sst_depth)

#area covered by each raster cell
cell_area <- cellSize(wc_regions_rast, unit = "km")
cell_area

#masking the suitable areas with the raster
wc_suitable <- mask(wc_regions_rast$rgn,
                    suitable_sst_depth)
plot(wc_suitable)

#extracting the areas
area_suitable <- expanse(wc_suitable, unit = "km")
area_suitable

area_region <- expanse(wc_suitable, unit = "km", byValue = TRUE)
as_tibble(area_region)
area_region_df <- as.data.frame(area_region)

region <- tribble(
  ~region, ~rgn_key,
  "Central California", "CA-C",
  "Northern California", "CA-N",
  "Oregon", "OR",
  "Southern California", "CA-S",
  "Washington", "WA")

combined <- cbind(region, area_region)

wc_regions_df <- as_tibble(wc_regions)

percent_suitable <- left_join(wc_regions_df, combined) |>
  mutate(percent_suitable = (area/area_km2) *100)

knitr::kable(percent_suitable)


## ----include=TRUE-----------------------------------------------------
map <- merge(wc_regions, percent_suitable) |>
  st_as_sf()

#total suitable area by region
map_1<- tm_shape(map) +
  tm_polygons(col = "area", 
              palette = "Blues",
              title = "Area of Suitable Habitat (km^2) by EEZ") +
  tm_format("World", main.title = "Total Area of Suitable Oyster Habitat by EEZ (km^2)") +
  tm_layout(legend.outside = TRUE) +
  tm_basemap(c("OpenStreetMap"))

#percent suitable area by region
map_2 <- tm_shape(map) +
  tm_polygons(col = "percent_suitable", 
              palette = "Greens",
              title = "Percent of Suitable Habitat (km^2) by EEZ") +
  tm_format("World", main.title = "Percent Suitable Oyster Habitat by EEZ (km^2)") +
  tm_layout(legend.outside = TRUE) +
  tm_basemap(c("OpenStreetMap"))

tmap_arrange(map_1, map_2)


## ---------------------------------------------------------------------
marine_species_function1 = function(name, min_temp, max_temp, min_depth, max_depth) {
  
#read in the shapefile 
wc_regions <- st_read("data/assignment4/wc_regions_clean.shp")
wc_regions <- vect(wc_regions)

#read in SST rasters
sst_2008 <- rast("data/assignment4/average_annual_sst_2008.tif") 
sst_2009 <- rast("data/assignment4/average_annual_sst_2009.tif") 
sst_2010 <- rast("data/assignment4/average_annual_sst_2010.tif") 
sst_2011 <- rast("data/assignment4/average_annual_sst_2011.tif") 
sst_2012 <- rast("data/assignment4/average_annual_sst_2012.tif") 

#combine SST rasters into a raster stack
all_sst <- c(sst_2008,
              sst_2009,
              sst_2010,
              sst_2011,
              sst_2012) 

#removing these object from my environment for clarity 
rm(sst_2008, sst_2009, sst_2010, sst_2011, sst_2012)

#read in bathymetry raster
depth <- rast("data/assignment4/depth.tif") 

all_sst <- terra::project(all_sst, crs(wc_regions))

###
#find the mean SST
all_sst_mean <- mean(all_sst) # doesn't give one value

#convert SST data from Kelvin to Celsius
all_sst_C <- all_sst_mean - 273.15

#crop depth raster to match the extent of the SST raster
depth_crop <- crop(depth, all_sst_C)

#resample the NPP data to match the resolution of the SST data using the nearest neighbor approach
depth_resample <- resample(depth_crop, all_sst_C, method = "near")

#stacked
sst_depth <- c(all_sst_C,
              depth_resample)

###
#reclassifying suitable SST
suitable_sst <- matrix(c(0, min_temp, NA,
                         min_temp, max_temp, 1,
                         max_temp, Inf, NA),
                       ncol = 3, byrow = TRUE)

suitable_sst <- classify(all_sst_C, rcl = suitable_sst)

#my first approach 
# suitable_sst <- replace(all_sst_C, all_sst_C > 11 | all_sst_C < 30, 1)
# #suitable_sst[is.na(suitable_sst)] <- NA 
# suitable_sst <- replace(all_sst_C, all_sst_C < 11 | all_sst_C > 30, NA)

#reclassifying suitable depth
suitable_depth <- matrix(c(-Inf, min_depth, NA,
                         min_depth, max_depth, 1,
                         max_depth, Inf, NA),
                       ncol = 3, byrow = TRUE)

suitable_depth <- classify(depth_resample, rcl = suitable_depth)

suitable_depth <- project(suitable_depth, crs(suitable_sst))

#converting to dataframe to make sure the reclassification worked
# all_sst_df <- as.data.frame(all_sst_C)
# depth_resample_df <- as.data.frame(depth_resample)
# suitable_sst_df <- as.data.frame(suitable_sst)
# suitable_depth_df <- as.data.frame(suitable_depth)

#stack
suitable_sst_depth <- c(suitable_sst,
                        suitable_depth)

fun_multiply = function(x,y){return(x*y)}
suitable_sst_depth <- lapp(suitable_sst_depth, 
                           fun = fun_multiply)

###

#rasterize 
nams <- names(wc_regions)

wc_regions_rast <- lapply(nams, function(x) {
  rasterize(wc_regions, suitable_sst_depth,
    field = x,
    touches = TRUE)})

wc_regions_rast <- do.call("c", wc_regions_rast)
#plot(wc_regions_rast)

#projecting
wc_regions_rast <- project(wc_regions_rast, 
                       suitable_sst_depth)

#area covered by each raster cell
cell_area <- cellSize(wc_regions_rast, unit = "km")
cell_area

#masking the suitable areas with the raster
wc_suitable <- mask(wc_regions_rast$rgn,
                    suitable_sst_depth)

#extracting the areas
area_suitable <- expanse(wc_suitable, unit = "km")
area_suitable

area_region <- expanse(wc_suitable, unit = "km", byValue = TRUE)
as_tibble(area_region)
area_region_df <- as.data.frame(area_region)

region <- tribble(
  ~region, ~rgn_key,
  "Central California", "CA-C",
  "Northern California", "CA-N",
  "Oregon", "OR",
  "Southern California", "CA-S",
  "Washington", "WA")

combined <- cbind(region, area_region)

wc_regions_df <- as_tibble(wc_regions)

percent_suitable <- left_join(wc_regions_df, combined) |>
  mutate(percent_suitable = (area/area_km2) *100)

###
map <- merge(wc_regions, percent_suitable) |>
  st_as_sf()

#total suitable area by region
map_A<- tm_shape(map) +
  tm_polygons(col = "area", 
              palette = "Purples",
              title = "Area of Suitable Habitat (km^2) by EEZ") +
  tm_format("World", main.title = paste("Total Area of Suitable", name, "Habitat by EEZ (km^2)")) +
  tm_layout(legend.outside = TRUE) +
  tm_basemap(c("OpenStreetMap"))

#percent suitable area by region
map_B <- tm_shape(map) +
  tm_polygons(col = "percent_suitable", 
              palette = "viridis",
              title = "Percent of Suitable Habitat (km^2) by EEZ") +
  tm_format("World", main.title = paste("Percent Suitable", name, "Habitat by EEZ (km^2)")) +
  tm_layout(legend.outside = TRUE) +
  tm_basemap(c("OpenStreetMap"))

tmap_arrange(map_A, map_B)

}


## ---------------------------------------------------------------------
#Leatherback turtle
#shouldn't max depth be -1250?
  
turtle_info <- marine_species_function1("Leatherback Turtle", 3, 29, 0, 1250) 
turtle_info


