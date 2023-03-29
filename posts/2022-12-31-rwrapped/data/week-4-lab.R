## ---- echo = FALSE, eval = TRUE---------------------------------------
# You probably already have these packages installed, so let's just load them
library(tidyverse)
library(readr)
library(gt)
library(openintro)
library(ggplot2)
library(modelr)
library(knitr)
library(xtable)

options(scipen = 999) # disable scientific notation

# Set your file path here! Or, set this up as an .Rproj if you'd like.
rootdir <- ("/Users/alexreed/Documents/MEDS/Courses/EDS_222")

# This runs the script _common.R, which loads all the packages we'll need for today and does some extra stuff we won't really use, at least for now.
source(file.path(rootdir,"labs","_common.R"))

# For labs, we want to see all our code
knitr::opts_chunk$set(echo = TRUE)


## ---- echo = F--------------------------------------------------------
ggplot(data = possum, aes(y = total_l, x = tail_l)) +
  geom_point() +
  labs(x = "Length of Possum Tail (cm)",
       y = "Total length of Possum (cm)")

ggplot(data = possum, aes(y = head_l, x = tail_l)) +
  geom_point() +
  labs(x = "Length of Possum Tail (cm)",
       y = "Length of Possum Head (cm)")


## ---------------------------------------------------------------------
# (i) the length of possum tail affects the total length of a possum:
mod_total = lm(total_l ~ tail_l, data = possum)
summary(mod_total) #provides summary stats

# (ii) the length of possum tail affects the length of a possum's head:
mod_head = lm(head_l ~ tail_l, data = possum)

# Recovering R2 from the regressions
R2_tot = summary(mod_total)$r.squared
R2_head = summary(mod_head)$r.squared

print(paste0("R2 of total length regressed on tail length is:", round(R2_tot, 2))) #first two decimal places
#tail length explains 32% of the variance in total length
#R squared is 0.32

print(paste0("R2 of head length regressed on tail length is:", round(R2_head, 2)))
#tail length explains 8% of the variance in head length
#correlation squared is equal to R squared


## ---------------------------------------------------------------------
head(mpg)
class(mpg$year) #shows us that the class of year is integer, want to change it to a factor

mpg <- mpg |>
  mutate(year = as.factor(year)) #now class will show that year is a factor


## ---------------------------------------------------------------------
ggplot(mpg, aes(x = year, y = hwy)) +
  geom_boxplot() +
  labs(x = "Year",
       y = "Fuel efficiency")
#Graphs our quantiles. Thick line is the median. Upper quantile is above line, lower quantile is below line.


## ---- echo=TRUE-------------------------------------------------------
lm(hwy ~ year, mpg) |>
  summary() |>
  xtable() |>
  kable()

#table doesn't show because of global options settings


## ---------------------------------------------------------------------
#trying to determine if there is omitted variable bias
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point() +
  labs(x = "engine displacement, in litres",
       y = "fuel efficiency, miles per gallon")
#larger engines = lower fuel efficiency 


## ---------------------------------------------------------------------
#run a regression where we add second variable
lm(hwy ~ displ + year, data = mpg)


## ---------------------------------------------------------------------
ggplot(data = mpg, aes(x = displ,
                       y = hwy, 
                       color = year)) +
  geom_point() +
  labs(x = "Engine displacement, litres", y = "Highway miles per gallon,")


## ---- echo=TRUE, cache=T----------------------------------------------
mod <- lm(hwy ~ displ + year, data = mpg)
augment(mod)


## ---------------------------------------------------------------------
mpg |>
  ggplot(aes(x = displ, y = hwy, color = year)) +
  geom_point() +
  geom_line(data = augment(mod), aes(y = .fitted, color = year)) + 
  labs(x = "Engine Displacement, in litres",
       y = "Highway Miles per gallon") +
  scale_colour_discrete("Year")

