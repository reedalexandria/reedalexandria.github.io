## ---- echo = FALSE, eval = TRUE---------------------------------------
# You probably already have these packages installed, so let's just load them
library(tidyverse)
library(readr)
library(ggplot2)
library(modelr)
library(knitr)
library(broom)

options(scipen = 999) # disable scientific notation

# For labs, we want to see all our code
knitr::opts_chunk$set(echo = TRUE)


## ---- echo=TRUE-------------------------------------------------------
mpg <- mpg %>% mutate(year = as.factor(year)) # Recall, we do this to ensure our year variable is treated as a categorical variable

mod <- lm(hwy ~ displ + year, data = mpg) # year 2008 is vintage, reference is 1999 

#avg mpg for a 1999 car when 2008 is 0
#levels(mpg$year) = 1999 is the lowest and that will be reference 
#can use ?relevel to relevel
#B1 is the slope of both lines because parallel 
#B2 is the the gap between the two lines. The difference between the two slopes 

mpg %>% 
  ggplot(aes(x = displ, y = hwy, color = year)) +
  geom_point() +
  geom_line(data = augment(mod), aes(y = .fitted, color = year)) + #augment adds predicted values from regression
  labs(x = "Engine Displacement, in litres",
       y = "Highway Miles per gallon") +
  scale_colour_discrete("Year")


## ---------------------------------------------------------------------
mod_int <- lm(hwy ~ displ + year + displ:year, data = mpg) #want year to be a factor (categorical)
summary(mod_int)


## ---------------------------------------------------------------------
mpg |>
  ggplot(aes(x = displ, hwy, color = year)) +
  geom_point() +
  geom_line(data = augment(mod_int), aes(y = .fitted, color = year))

#fitted will contain estimated coefficients. they vary in dataframe because dif. year and displ.
#should be able to identify each coefficients (B0, B1) and slope on graph 
#google how to interpret an interaction model


## ---------------------------------------------------------------------
# no interaction
mod = lm(hwy ~ displ + year, data = mpg)
AdjR2 = summary(mod)$adj.r.squared
print(AdjR2) #0.5969

# with the interaction
AdjR2_int = summary(mod_int)$adj.r.squared
print(AdjR2_int) #0.5962

#adding interaction basically did nothing. Not a lot of evidence that we need to account for engine displacement. Added complexity and got a "worse" R2. 


## ---------------------------------------------------------------------
# no interaction
R2 = summary(mod)$r.squared
print(R2) #0.6004

# with the interaction
R2_int = summary(mod_int)$r.squared
print(R2_int) #0.6014 
#slightly higher when adding complexity 

