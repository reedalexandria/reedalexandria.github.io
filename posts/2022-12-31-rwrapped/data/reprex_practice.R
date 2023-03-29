## ----setup, include=FALSE---------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(tidyverse)
library(here)
library(janitor)


## ---------------------------------------------------------------------
mack_creek_vertebrates <- read_csv(here("AS00601.csv"))


## ---------------------------------------------------------------------
# Example 1: 
mack_creek_lengths <- mack_creek_vertebrates %>% 
  clean_names() %>% 
  select(year:sampledate) %>% 
  filter(section == "CC") %>% 
  mutate(across(where(is.character), tolower)) %>% 
  drop_na(species) %>% 
  group_by(species) %>% 
  summarize(mean_length_cm = mean(length1, na.rm = TRUE),
            sd_length_cm = sd(length1, na.rm = TRUE)) %>% 
  ungroup()


## ---------------------------------------------------------------------
# Example 2:
mack_creek_vertebrates %>% 
  clean_names() %>% 
  filter(species == "ONCL") %>% 
  ggplot(aes(x = length1, y = weight)) +
         geom_point(aes(shape = 12), color = "purple") +
         theme_minimal() +
         labs(x = "Cutthroat trout length (cm)",
              y = "Weight (g)")

