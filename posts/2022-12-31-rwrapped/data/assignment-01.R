## ----setup, include=FALSE---------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
# Load all the packages needed here
library(tidyverse)
library(readr)
library(gt)
library(tufte)

# Set your file path here! Or, set this up as an .Rproj if you'd like.
rootdir <- ("/Users/alexreed/Documents/MEDS/Courses/EDS_222")
datadir <- file.path(rootdir,"data","01-week-one") #check this matches the folder structure on your local computer
setwd(file.path(rootdir,"homework","HW1")) #check this one too


## ---- out.width = "100%", echo=FALSE, fig.margin=TRUE-----------------
knitr::include_graphics("pm_south_asia.jpeg")


## ---------------------------------------------------------------------
crowdsourced <- readRDS(file.path(datadir,"airpol-PK-crowdsourced.RDS"))
govt <- readRDS(file.path(datadir, "airpol-PK-govt.RDS"))


## ---------------------------------------------------------------------
print(nrow(crowdsourced))
print(nrow(govt))


## ---------------------------------------------------------------------
crowdsourced_monitors <- crowdsourced |>
  group_by(longitude,latitude) |>
  mutate(monitors = cur_group_id()) 

govt_monitors <- govt |>
  group_by(longitude,latitude) |>
  mutate(monitors = cur_group_id()) 


## ---- include = TRUE, fig.margin = TRUE-------------------------------
crowdsourced_mean <- mean(crowdsourced$PM)
crowdsourced_min <- min(crowdsourced$PM)
crowdsourced_max <- max(crowdsourced$PM)

#another way to find the answer
summary(crowdsourced)


## ---- include = TRUE, fig.margin = TRUE-------------------------------
govt_mean <- mean(govt$PM)
govt_min <- min(govt$PM)
govt_max <- max(govt$PM)

#another way to find the answer
summary(govt)


## ---- include = TRUE, out.width = "100%", echo = FALSE, fig.margin = TRUE----
#changing the text in the "id" column to "crowd-sourced" or "government" for each dataset so that I can differentiate the two once the datasets are combined
crowdsourced_update <- crowdsourced_monitors |>
  mutate(monitors = "crowd-sourced") 
govt_update <- govt_monitors |>
  mutate(monitors = "government")

#combine datasets
all_monitors <- rbind(crowdsourced_update, govt_update)

#plot
ggplot(data = all_monitors, 
       aes(x = longitude, 
           y = latitude, 
           color = monitors)) +
  geom_point() +
  labs(y = "Latitude",
       x = "Longitude",
       title = "Air Pollution Monitoring Locations in Lahore") +
  theme_minimal() 


## ---- include = TRUE, eval = TRUE-------------------------------------
# (i) pooled above

# (ii)
random_sample <- all_monitors[sample(nrow(all_monitors), 1000), ]


## ---- include = TRUE, eval = TRUE-------------------------------------
stratified_sample <- all_monitors |>
  mutate(strata = cut(latitude,
                      breaks = c(-Inf, 31.555, 31.565, 31.575, 31.585, 31.595, Inf))) |> 
  group_by(strata) |>
  sample_n(size=200)

# I went with five strata bins instead of six so that the random_sampling and stratified_sampling would have the same number of observations.


## ---- include = TRUE, eval = TRUE-------------------------------------
print(mean(random_sample$PM))
print(mean(stratified_sample$PM))

