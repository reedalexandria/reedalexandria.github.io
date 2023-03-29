## ----setup, include=FALSE---------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

library(tidyverse)
library(here)


## ---------------------------------------------------------------------
meteorological <- read_csv(here("data", "ntl20_v6.csv"))
ice_duration <- read_csv(here("data", "ntl33_v7.csv"), 
                         na = "-999")


## ---------------------------------------------------------------------
ice_duration |>
  ggplot(aes(x = lakeid, y = ice_duration)) +
  geom_jitter(width = 0.2, color = "gray") +
  geom_boxplot(width = 0.3, fill = NA, 
               size = 1, 
               outlier.color = NA)


## ---------------------------------------------------------------------
ice_duration_summary <- ice_duration |>
  group_by(year4) |>
  summarize(mean_ice_duration = mean(ice_duration, na.rm = TRUE))

# Create an exploratory visualization of mean ice cover duration by year for the Madison Lake Area. Add a sentence or two below this exploratory graph describing the overall trend(s) you observe.

ggplot(data = ice_duration_summary, aes(x = year4, y = mean_ice_duration)) +
  geom_line(color = "gray") +
  labs(caption = "The mean ice cover has generally decreased throughout the past 150 years")


## ---------------------------------------------------------------------
mean_temp_plot <- meteorological |>
  filter(month %in% c("December", "January", "February"))
  group_by(year4) |>
  summarize(mean_temp = mean(ave_air_temp_adjusted, na.rm = TRUE))

ggplot(data = mean_temp_plot, aes(x = year4, y = mean_temp)) +
  geom_line()


