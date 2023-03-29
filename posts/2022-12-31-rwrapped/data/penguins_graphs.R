## ----setup, include=FALSE---------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(tidyverse)
library(palmerpenguins)


## ---------------------------------------------------------------------
ggplot(data = penguins, aes(x = body_mass_g, y = flipper_length_mm)) +
  geom_point(aes(color = species), size = 2) +
  facet_wrap(~island) +
  theme_minimal() + 
  labs(x = "Body mass (g)", 
       y= "Flipper length (mm)",
       title = "Penguin variables")


