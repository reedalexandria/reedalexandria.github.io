## ----setup, include=FALSE---------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

library(terra)
library(raster)
library(tidyverse)
library(tmap)
library(stars)

rootdir <- ("/Users/alexreed/Documents/MEDS/Courses/EDS_223")
datadir <- file.path(rootdir,"Assignments", "data","assignment3")
#setwd(file.path(rootdir,"Assignments","assignment3-reedalexandria"))


## ----include=TRUE-----------------------------------------------------
#read in night lights tiles
VNP_28 <- rast("data/assignment3/VNP46A1/VNP46A1.A2021038.h08v05.001.2021039064328.tif") |> st_as_stars()
VNP_29 <- rast("data/assignment3/VNP46A1/VNP46A1.A2021038.h08v06.001.2021039064329.tif") |> st_as_stars()
VNP_06 <- rast("data/assignment3/VNP46A1/VNP46A1.A2021047.h08v05.001.2021048091106.tif") |> st_as_stars()
VNP_05 <- rast("data/assignment3/VNP46A1/VNP46A1.A2021047.h08v06.001.2021048091105.tif") |> st_as_stars()

#combine tiles for each date
feb_7 <- st_mosaic(VNP_28, VNP_29)
#plot(feb_7)
feb_16 <- st_mosaic(VNP_06, VNP_05)
#plot(feb_16)


## ----include=TRUE-----------------------------------------------------
#finding the change in night lights intensity and locations that experienced a drop of more than 200
blackout_mask <- (feb_7 - feb_16) > 200

# Assign NA to values with a difference of less than 200 
blackout_mask[blackout_mask == FALSE] <- NA

#visualizing it
# tmap_mode("view")
# tm_shape(blackout_mask) +
#   tm_raster()


## ---------------------------------------------------------------------
#Another method I tried that worked. But I did not use it because I don't fully understand the cut function and how to create a vector that will drop more than 200

# #finding the change in night lights intensity
# change_in_light <- (feb_16 - feb_7) 
# plot(change_in_light)
# 
# #reclassify
# change_in_light_reclassify <- cut(change_in_light, c(-6000, -200), labels = c("blackout"))
# plot(change_in_light_reclassify, col = c("black"))
# 
# #visualizing it
# tmap_mode("view")
# tm_shape(change_in_light_reclassify) +
#   tm_raster()


## ----include=TRUE-----------------------------------------------------
#vectorize blackout mask
bm_vector <- st_as_sf(blackout_mask)

#fix invalid geometries
bm_vector <- st_make_valid(bm_vector)


## ----include=TRUE-----------------------------------------------------
#define roi and turn into a polygon
roi <- st_polygon(list(rbind(c(-96.5, 29),
                             c(-96.5, 30.5),
                             c(-94.5, 30.5),
                             c(-94.5, 29),
                             c(-96.5, 29)))) 

#convert to sf and assign crs
roi <- st_sfc(roi) |>
  st_set_crs(4326) #WGS84 has a unique reference code (EPSG 4326)

#crop blackout mask to roi
intersects <- st_intersects(bm_vector, roi, sparse = FALSE)
bm_cropped <- bm_vector[intersects,]

#reproject to EPSG 3083
bm_cropped <- bm_cropped |>
  st_transform(3083)


## ----include=TRUE-----------------------------------------------------
#load highways dataset and reproject
query <- "SELECT * FROM gis_osm_roads_free_1 WHERE fclass='motorway'"
highways <- st_read("data/assignment3/gis_osm_roads_free_1.gpkg", 
                    query = query, 
                    quiet = TRUE)
#reproject
highways <- highways |>
  st_transform(3083)

#identify areas within 200m of all highways
highway_buffer  <- st_buffer(highways, dist =  200)
highway_buffer <- st_union(highway_buffer)
#plot(highway_buffer)

#identify areas beyond 200m of highways.st_disjoint does not include houses touching the buffer
#beyond_buffer1 <- bm_cropped[highway_buffer, , op = st_disjoint]
#plot(beyond_buffer1)

#identify areas beyond 200m of highways. st_difference includes "partial houses" and houses touching the boundary. I decided to use this one because I think the houses touching the buffer should be included
beyond_buffer2 <- st_difference(bm_cropped, highway_buffer)
#plot(beyond_buffer2)

#visualizing it
# tmap_mode("view")
# tm_shape(beyond_buffer1) +
#   tm_polygons() +
#   tm_shape(highway_buffer) +
#   tm_borders()
# 
# tmap_mode("view")
# tm_shape(beypnd_buffer2) +
#   tm_polygons() +
#   tm_shape(highway_buffer) +
#   tm_borders()


## ----include=TRUE-----------------------------------------------------
#load buildings dataset and reproject
query <- "SELECT *
FROM gis_osm_buildings_a_free_1
WHERE (type IS NULL AND name IS NULL)
OR type in ('residential', 'apartments', 'house', 'static_caravan', 'detached')"
buildings <- st_read("data/assignment3/gis_osm_buildings_a_free_1.gpkg", 
                     query = query, 
                     quiet = TRUE)
#reproject
buildings <- buildings |>
  st_transform(3083)


## ----include=TRUE-----------------------------------------------------
#filter to homes within blackout area
buildings_blackout <- buildings[beyond_buffer2, op = st_intersects]
impacted_homes <- nrow(buildings_blackout)

#if you use beyond_buffer1 (st_disjoint) you'd get 138,652 impacted homes

print(paste("There were", impacted_homes, "homes within blackout areas."))


## ----include=TRUE-----------------------------------------------------
#load geodatabase layers
geoms <- st_read("data/assignment3/ACS_2019_5YR_TRACT_48_TEXAS.gdb",
                     layer = "ACS_2019_5YR_TRACT_48_TEXAS") 

#load income data and update column names
income <- st_read("data/assignment3/ACS_2019_5YR_TRACT_48_TEXAS.gdb",
                      layer = "X19_INCOME") |>
  select(GEOID, B19013e1) |>
  rename(GEOID_Data = GEOID,
         median_income = B19013e1) 


## ---------------------------------------------------------------------
#join income data to the census tract geometries and crop to roi
census_income <- left_join(geoms, income) |>
  st_transform(3083) 

roi <- roi |>
  st_transform(3083)

census_income_cropped <- census_income[roi, op = st_intersects]

# census tracts with blackouts
census_blackout <- census_income_cropped[buildings_blackout,] |>
  mutate(blackout = "yes")

#census_income_cropped combined with census_blackout
combined <- full_join(as.data.frame(census_income_cropped), as.data.frame(census_blackout))
combined$blackout[is.na(combined$blackout)] <- "no" 

census_noblackout_only <- st_as_sf(combined[!grepl("yes",combined$blackout),])


## ---------------------------------------------------------------------
tmap_mode("view")

#Create a map of median income by census tract, designating which tracts had blackouts
#I chose to make it an interactive map because then you can turn off the layer that contains the census tracts which had blackouts
  tm_shape(census_income_cropped) + 
  tm_polygons("median_income", 
          title = "Median Income ($)",
          palette = "viridis") +
  tm_shape(census_blackout) +
  tm_dots() +
  tm_layout(title = "Median income by census tract")
  
#tmap_mode("plot") to get out of interactive version
tmap_mode("plot")

#census tracts with blackouts
census_blackout_map <-  tm_shape(census_blackout) + 
  tm_fill("median_income", 
          title = "Median Income ($)",
          palette = "viridis") +
  tm_borders() +
  tm_layout(title = "Median income for census tracts that had blackouts")

#census tracts with no blackouts   
census_noblackout_only_map <-  tm_shape(census_noblackout_only) + 
  tm_fill("median_income", 
          title = "Median Income ($)",
          palette = "viridis") +
  tm_borders() +
  tm_layout(title = "Median income for census tracts that did not have blackouts")
  
tmap_arrange(census_blackout_map, census_noblackout_only_map)


## ---------------------------------------------------------------------
#side by side histogram. I'm not sure how to 
par(mfrow=c(1,2))
#histogram of the distribution of income in impacted tracts
median_income_blackout <- (census_blackout$median_income)
hist(median_income_blackout,
main="Median Income for Tracts Impacted by Blackout",
cex.main = .75,
xlab="Median Income ($)",
ylab="Number of Homes",
col="darkblue",
n = 30)
#histogram of the distribution of income in unimpacted tracts
median_income_no_blackout <- (census_noblackout_only$median_income)
hist(median_income_no_blackout,
main="Median Income for Tracts Not Impacted by Blackout",
cex.main = .75,
xlab="Median Income ($)",
ylab="Number of Homes",
col="darkblue",
n = 30)

summary(median_income_blackout)
summary(median_income_no_blackout)

