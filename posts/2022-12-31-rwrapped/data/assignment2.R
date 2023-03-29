## ----setup, include=FALSE---------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ----load, include=TRUE, message=FALSE, warning=FALSE-----------------
# add required packages here
library(sf)
library(terra)
library(dplyr)
library(spData)
library(spDataLarge)
library(tmap)


## ----include=TRUE-----------------------------------------------------
west_region <- us_states |>
  filter(REGION == "West",
         as.numeric(AREA) < 250000,
         total_pop_15 > 5000000)

print(paste0("The state that belongs in the West region, has an area below 250,000 square kilometers, and greater than 5,000,000 residents in 2015 is ", west_region$NAME, "."))


## ----include=TRUE-----------------------------------------------------
total_pop <- us_states |>
  summarize(total_pop = sum(total_pop_15))

#print(paste0("The total population of the US in 2015 was 314,375,347 people."))

print(paste0("The total population of the US in 2015 was ", total_pop$total_pop, " people."))


## ----include=TRUE-----------------------------------------------------
us_states_stats <- us_states_df |>
  left_join(us_states, by = c("state" = "NAME"))
class(us_states_stats)

us_states_df |>
anti_join(us_states, by = c("state" = "NAME"))

print(paste0("I used left_join on us_states_df because it includes all of the rows in us_states_df, which had two more rows than us_states. An inner-join would not have included the two additional rows in us_states_df. The variable NAME in us_states and the variable state in us_states_df are key. The class of the new object is a tbl_df, tbl, data.frame. The us_states_df has 2 more rows than us_states because it contains the states Alaska and Hawaii."))


## ----include=TRUE-----------------------------------------------------
density <- us_states |>
  mutate(density10 = total_pop_10/AREA,
         density15 = total_pop_15/AREA,
         density_dif = density15 - density10,
         density_perc = ((density_dif)/density10)*100)

current.mode <- tmap_mode("plot")
tm_shape(density) +
  tm_polygons("density_perc",
              title = "Percent Change in Population Density",
              palette = "Blues",
              legend.format = list(fun=function(x) paste0(formatC(x, digits=0, format="f"), "%"))) +
  tm_layout(legend.position = c("left", "bottom"),
            main.title = "Percent Change in Population Density between 2010 and 2015")


## ----include=TRUE-----------------------------------------------------
density_decrease <- density |>
  filter(as.numeric(density_dif) < 0)

print(paste0("The population density decreased in two states: Michigan and Rhode Island"))


## ----include=TRUE-----------------------------------------------------
canterbury <- nz |>
  filter(Name == "Canterbury")

c_height <- nz_height[canterbury,] 
nrow(c_height)

print(paste0("There are ", nrow(c_height), " high points in the Canterbury region."))


## ----include=TRUE-----------------------------------------------------
region_height <- st_join(nz_height, nz)
region_height2 <- region_height |>
  count(Name, sort = TRUE)

# Another way to solve, but more involved:
# nz_height_agg <- aggregate(nz_height, nz, length)
# nz_height_combined <- cbind(nz, count = nz_height_agg$elevation) |>
#   st_drop_geometry() |> 
#   dplyr::select(Name, count) |> 
#   arrange(desc(count)) |> 
#   slice(2)

print(paste0("The West Coast region has the second highest number of nz_height points, with a total of 22."))


## ----include=TRUE-----------------------------------------------------
colorado <- us_states |>
  filter(NAME == "Colorado")

intersects <- us_states[colorado, , op = st_intersects]
plot(us_states$geometry, main = "States that geographically intersect with Colorado")
plot(intersects$geometry, col = "lightblue", add = TRUE)


## ----include=TRUE-----------------------------------------------------
touches = us_states[colorado, , op = st_touches]
plot(us_states$geometry, main = "States that touch Colorado")
plot(touches$geometry, col = "blue", add = TRUE)


## ----include=TRUE-----------------------------------------------------
plot1 <- rmapshaper::ms_simplify(st_geometry(nz), keep = 0.5)
plot1 <- tm_shape(plot1) + 
  tm_polygons() + 
  tm_layout(main.title = "keep = 0.5")

plot2 <- rmapshaper::ms_simplify(st_geometry(nz), keep = 0.05)
plot2 <- tm_shape(plot2) + 
  tm_polygons() + 
  tm_layout(main.title = "keep = 0.05")

plot3 <- rmapshaper::ms_simplify(st_geometry(nz), keep = 0.005)
plot3 <- tm_shape(plot3) + 
  tm_polygons() + 
  tm_layout(main.title = "keep = 0.005")

plot4 <- rmapshaper::ms_simplify(st_geometry(nz), keep = 0.0005)
plot4 <- tm_shape(plot4) + 
  tm_polygons() + 
  tm_layout(main.title = "keep = 0.0005")

plot5 <- rmapshaper::ms_simplify(st_geometry(nz), keep = 0.00005)
plot5 <- tm_shape(plot5) + 
  tm_polygons() + 
  tm_layout(main.title = "keep = 0.00005")

current.mode <- tmap_mode("plot")
tmap_arrange(plot1, plot2, plot3, plot4, plot5)

# ----------------dTolerance
plotA <- st_simplify(st_geometry(nz), dTolerance = 100)
plotA <- tm_shape(plotA) + 
  tm_polygons() + 
  tm_layout(main.title = "dTolerance = 100")

plotB <- st_simplify(st_geometry(nz), dTolerance = 1000)
plotB <- tm_shape(plotB) + 
  tm_polygons() + 
  tm_layout(main.title = "dTolerance = 1000")

plotC <- st_simplify(st_geometry(nz), dTolerance = 10000)
plotC <- tm_shape(plotC) + 
  tm_polygons() + 
  tm_layout(main.title = "dTolerance = 10000")

plotD <- st_simplify(st_geometry(nz), dTolerance = 39000) 
plotD <- tm_shape(plotD) + 
  tm_polygons() + 
  tm_layout(main.title = "dTolerance =39000")

#I used 39,000 because this was the largest value it would allow before not having an output (not enough points to simplify by)

current.mode <- tmap_mode("plot")
tmap_arrange(plotA, plotB, plotC, plotD)


## ----include=TRUE-----------------------------------------------------
canterbury_buffer <- st_buffer(canterbury, dist = 100000) 
nz_height_near <- nz_height[canterbury_buffer, ]
nrow(nz_height_near)

print(paste0("There are ", nrow(nz_height_near), " points within 100km of the Canterbury region"))


## ----include=TRUE-----------------------------------------------------
nz_cent <- st_centroid(st_union(nz))
cant_cent <- st_centroid(canterbury)
st_distance(nz_cent, cant_cent)

#answer provided in meters (234,192.6 meters)

print(paste0("The geographic centroid of New Zealand is ", st_distance(nz_cent, cant_cent), " meters (234.193km) from the centroid of Canterbury."))

