## ----setup, include=FALSE---------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
# Load all the packages needed here
library(tidyverse)
library(readr)
library(gt)
library(tufte)

# Set your filepath here! Or, set this up as an .Rproj if you'd like.
rootdir <- ("/Users/alexreed/Documents/MEDS/Courses/EDS_222")
datadir <- file.path(rootdir,"data","HW3")
setwd(file.path(rootdir,"homework","HW3"))


## ---------------------------------------------------------------------
forest_fires <- readRDS(file.path(datadir, "forestfires.rds"))

ggplot(data = forest_fires, 
       aes(x = temp, y = area)) +
  geom_point() +
  labs(y = "Area",
       x = "Temperature")


## ---------------------------------------------------------------------
summary(lm(area ~ temp, data = forest_fires))


## ---------------------------------------------------------------------
forest_fires_rescale <- forest_fires |>
  mutate(acres = area*2.47)

summary(lm(acres ~ temp, data = forest_fires_rescale))


## ---------------------------------------------------------------------
ggplot(data = forest_fires, 
       aes(x = temp, y = area)) +
  geom_point() +
  geom_smooth(se = F) +
  labs(y = "Area",
       x = "Temperature") 


## ---------------------------------------------------------------------
mod <- lm(area ~ temp, data = forest_fires)
summary(mod)$r.squared

#R2 of the area burned on temperature is 0.28.


## ---------------------------------------------------------------------
summary(lm(area ~ temp + RH, data = forest_fires))

