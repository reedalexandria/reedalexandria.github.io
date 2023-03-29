## ----setup, include=FALSE---------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(tidyverse)


## ---------------------------------------------------------------------
# Make the Leslie Matrix:
fish_leslie <- matrix(c(0, 0, 0, 600, 0.05, 0, 0, 0, 0, 0.20, 0, 0, 0, 0, 0.16, 0.9), nrow = 4, ncol = 4, byrow = TRUE)

# Check it out: 
fish_leslie


## ---------------------------------------------------------------------
# Initial population structure (Roe, Fry, Juvenile, Adult): 
fish_year0 <- c(2000, 8000, 400, 500)


## ---------------------------------------------------------------------
# Model projections

# ------ WHAT HAPPENS HERE? ------ #
# ------ comment line-by-line ---- #

# Create a sequence from 0 to 8
time_yr <- seq(from = 0, to = 8, by = 1)

# Create a matrix with columns as the initial population and the rows as the sequence of times
proj_year <- matrix(ncol = length(fish_year0), nrow = length(time_yr))

# Populate initial population in the first row of the matrix
proj_year[1, ] <- fish_year0

# Create a loop: starts at 2 for time_yr, looping over the dot product of the leslie matrix with the initial values, populate row 2
for (i in 2:length(time_yr)) {
  proj_year[i,] <- fish_leslie %*% proj_year[i-1,]
}


# The rest is wrangling & visualization (run to check it out):
colnames(proj_year) <- c("eggs", "fry", "juvenile", "adult")

proj_df <- data.frame(time_yr, proj_year) %>% 
  pivot_longer(cols = -time_yr, names_to = "lifestage", values_to = "stage_population")

ggplot(data = proj_df, aes(x = time_yr, y = stage_population)) +
  geom_line(aes(color = lifestage)) +
  scale_y_log10()


