## ----setup, include = FALSE-------------------------------------------
knitr::opts_chunk$set(echo = TRUE, warning = FALSE, message = FALSE)

library(tidyverse)
library(palmerpenguins)


## ---------------------------------------------------------------------
burrito <- 2.4

if (burrito > 2) {
  print("I love burritos!")
}


## ---------------------------------------------------------------------
my_ships <- c("Millenium Falcon", "X-wing", "Tie-Fighter", "Death Star")

str_detect(my_ships, pattern = "r")

# does the string vector contain an r. This is case sensitive (wouldn't capture R)

phrase <- "I love burritos"

if (str_detect(phrase, pattern = "love")) {
  print("I am a burrito fan.")
}


## ---------------------------------------------------------------------
pika <- 89.1

if (pika > 60) {
  print("mega pika")
} else {
  print("normal pika")
}


## ---------------------------------------------------------------------
# |> = %>% = and then  
# filter function is a part of dplyr

tatooine_characters <- starwars |> 
  filter(homeworld == "Tatooine") 

# Use %in% operator to check for multiple positive matches

tatooine_endor_naboo <- starwars |> 
  filter(homeworld %in% c("Tatooine", "Naboo", "Endor"))

#In words: Does the value in the homeworld variable for that row exist IN that vector c("Tatooine", "Naboo", "Endor")



## ---------------------------------------------------------------------
penguins |> 
  group_by(species, island) |>
  summarize(mean_bill_length = mean(bill_length_mm, na.rm = TRUE),
            sd_bill_length = sd(bill_length_mm, na.rm = TRUE),
            n = n())

# 1. Find the mean and sd of character height in 'starwars' grouped by homeworld

starwars |> 
  group_by(homeworld) |> 
  summarize(mean_height = mean(height, na.rm = TRUE),
            sd_height = sd(height, na.rm = TRUE),
            n = n())

# 2. Find the maximum, minimum flipper length, and sample size, for each species of penguins 
penguins |> 
  group_by(species, sex) |> 
  summarize(max_flipper_length = max(flipper_length_mm, na.rm = TRUE),
            min_flipper_length = min(flipper_length_mm, na.rm = TRUE),
            sample_size = n())

# Across example. Nested for loop. Able to iterate in multiple directions
penguins |> 
  group_by(species) |> 
  summarize(across(where(is.numeric), mean, na.rm = TRUE))


## ---------------------------------------------------------------------
tree_height <- c(1, 2, 6, 10, 14, 20)

for (i in seq_along(tree_height)) {
  val = tree_height[i] + tree_height[i + 1]
  print(val)
}


## ---------------------------------------------------------------------
#Create empty vector animal_ages

animal_ages <- vector(mode = "numeric", length = length(species))

species <- c("dog", "elephant", "goat", "dog", "dog", "elephant")

age_human <- c(3, 8, 4, 6, 12, 18)

for (i in seq_along(species)) {
  if (species[i] == "dog") {
    animal_age <- age_human[i] * 7 
  } else if (species[i] == "elephant") {
    animal_age <- age_human[i] * 0.88 
  } else if (species[i] == "goat") {
    animal_age <- age_human[i] * 4.7
  }
  animal_ages[i] <- animal_age
} 



## ---------------------------------------------------------------------
# Storage vector
mean_mtcars <- vector(mode = "numeric", length = ncol(mtcars))

#Writing the loop

for (i in seq_along(mtcars)) {
  mean_val <- mean(mtcars[[i]], na.rm = TRUE)
  mean_mtcars[[i]] <- mean_val
}

mean_mtcars


## ---------------------------------------------------------------------
apply(X= mtcars, MARGIN =2, FUN = mean)


## ---------------------------------------------------------------------
#{purrr}
map(.x = mtcars, .f = mean)

#to return output in a data frame (instead of a list)
map_df(.x = mtcars, .f = mean)

