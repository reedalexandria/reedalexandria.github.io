## ----setup, include=FALSE, echo = TRUE, eval = FALSE------------------
## list.of.packages <- c("sf", "sp","raster", "gstat", "automap", "patchwork", "viridis")
## new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
## if(length(new.packages)) install.packages(new.packages)


## ---- echo = TRUE, eval = TRUE----------------------------------------
library(tidyverse)
library(sf)
library(sp)
library(raster)
library(gstat)
library(automap)
library(patchwork)
library(viridis)

options(scipen = 999) # disable scientific notation

# For labs, we want to see all our code
knitr::opts_chunk$set(echo = TRUE)


## ---------------------------------------------------------------------
rootdir <- ("/Users/alexreed/Documents/MEDS/Courses/EDS_222") # you need to change this to fit your local file structure!


## ---------------------------------------------------------------------
plot_my_gstat_output <- function(raster_object, object_name){
  
  df <- rasterToPoints(raster_object) %>% as_tibble()
  colnames(df) <- c("X", "Y", "Z")
  
  ggplot(df, aes(x = X, y = Y, fill = Z)) +
    geom_raster() +
    ggtitle(label = object_name) +
    scale_fill_viridis(option = "B", limits = c(48, 101)) +
    theme_void() +
    theme(
      plot.title = element_text(hjust = 0.5)
    )
}


## ---------------------------------------------------------------------
wellobs <- readRDS(file.path(rootdir, "labs", "week-10-lab", "week-ten-data.RDS"))

# X and Y  = lat and long
# Z = well depth


## ---------------------------------------------------------------------
wellobs |>
  ggplot(mapping = aes(x = X, y = Y, color = Z)) + #mapping = isn't necessary 
  geom_point(size = 3) +
  scale_color_viridis(option = "B") + theme_classic()


## ---------------------------------------------------------------------
wellobs_sf <- st_as_sf(wellobs, coords = c("X", "Y"), crs = 25832) %>%
  cbind(st_coordinates(.))
head(wellobs_sf)


## ---------------------------------------------------------------------
v_mod_full <- automap::autofitVariogram(Z~1, as(wellobs_sf, "Spatial"))
v_mod <- v_mod_full$var_model
head(v_mod)


## ---------------------------------------------------------------------
plot(v_mod_full)
#as we get further away the variance gets further
#within 1000 units the points are very correlated 
#not getting a leveling off. should we be worried? not yet getting to distances where spatial correlation goes away. within our sample we are seeing changes. if we sampled larger area we would eventually get the taper off. not a problem because suggests spatial structure within box and we can feel good about spatial interpolating. 
#the values on dots are the number of observations 


## ---------------------------------------------------------------------
#arbitrarily making it 100x100
grid_100_sf <- wellobs_sf |>
  st_bbox() |>
  st_as_sfc() |> #making bounding box an sf object
  st_make_grid(
    cellsize = c(100, 100),
    what = "centers"
  ) |>
  st_as_sf() %>%
  cbind(., st_coordinates(.))

plot(grid_100_sf)


## ---------------------------------------------------------------------
grid_100_sp <- as(grid_100_sf, "Spatial")
gridded(grid_100_sp) = TRUE
grid_100_sp = as(grid_100_sp, "SpatialPixels")


## ---------------------------------------------------------------------
#ordinary kriging
OK <- krige(Z~1, as(wellobs_sf, "Spatial"),
            grid_100_sp,
            model = v_mod)

#we like kriging because we get a prediction and measure of uncertainty (variance)

plot(OK)


## ---------------------------------------------------------------------
p_raw = ggplot(data = wellobs, aes(x = X, y = Y, color = Z)) +
  geom_point(size = 3) + scale_color_viridis(option = "B", limits = c(48, 101)) +
  ggtitle(label = "Observation wells sampled") + 
  theme_void() + 
  theme(plot.title = element_text(hjust = .5))

p_kriging = plot_my_gstat_output(raster(OK), "Ordinary Kriging")

p_raw + p_kriging
# kriging is not an exact fit. no guarantee it will go through point

