## ---- echo = FALSE, eval = TRUE---------------------------------------
# You probably already have these packages installed, so let's just load them
library(tidyverse)
library(readr)
library(gt)
library(openintro)
library(ggplot2)
library(modelr)

options(scipen = 999) # disable scientific notation

# Set your file path here! Or, set this up as an .Rproj if you'd like.
rootdir <- ("/Users/alexreed/Documents/MEDS/Courses/EDS_222")

# This runs the script _common.R, which loads all the packages we'll need for today and does some extra stuff we won't really use, at least for now.
source(file.path(rootdir,"labs","_common.R"))

# For labs, we want to see all our code
knitr::opts_chunk$set(echo = TRUE)


## ---------------------------------------------------------------------
head(possum)

#correlation is telling us how likely is knowing x will it allow us to predict y 
ggplot(possum, aes(x = tail_l, y = total_l)) +
  geom_point() +
  labs(x = "length of tail (cm)",
       y = "total length (cm)")

#appears to have more correlation, possibly 0.7 or 0.55

ggplot(possum, aes(x = tail_l, y = head_l)) +
  geom_point() +
  labs(x = "length of tail (cm)",
       y = "head length (cm)")

#appears to have less correlation because points are more spread out, possibly 0.2 or 0.3


## ---------------------------------------------------------------------
# (i) total & tail:
cor(possum$total_l, possum$tail_l) # = 0.566

#tidyverse way to find it 
possum |>
  summarize(total_tail = cor(total_l, tail_l))


## ---- fig.margin=TRUE-------------------------------------------------
# (ii) head & tail:
cor(possum$head_l, possum$tail_l) # = 0.287


## ---------------------------------------------------------------------
lm(total_l ~ tail_l, data = possum) |> 
  tidy() |> 
  gt()

#another way
summary(lm(total_l ~ tail_l, data = possum))

# tail_l: for every cm increase in tail length, their is a 1.24 increase in total length. Possums with larger tail tend to have a longer body because greater than 1. 
# intercept: add a tail length of 0 cm the total length would be 41.04 cm. 


## ---------------------------------------------------------------------
lm(head_l ~ tail_l, data = possum) |> 
  tidy() |> 
  gt()

# tail_l: for every cm increase in tail length, their is a 1.24 increase in total length. Possums with larger tail tend to have a longer body because greater than 1. 
# intercept: add a tail length of 0 cm the total length would be 41.04 cm. 


## ---------------------------------------------------------------------
ggplot(possum, aes(x = tail_l, 
                   y = total_l)) +
  geom_point() +
  geom_smooth(method = "lm", 
              formula = y~x, 
              se=FALSE, 
              size = 2,
              col = "darkorchid") +
  labs(x = "length of tail (cm)",
       y = "total length (cm)")


## ---------------------------------------------------------------------



## ---------------------------------------------------------------------
mod <- lm(total_l ~ tail_l, possum)

predictions <- possum |>
  add_predictions(mod) |> #added a column "pred" which is our y hat
  mutate(residuals = total_l-pred) #added a column "residuals" which are the total_1 minus pred


## ---------------------------------------------------------------------
#erros should be distributed normally
ggplot(predictions) +
  geom_histogram(aes(residuals), bins = 25)

#in general the histogram appears normally distributed


## ---------------------------------------------------------------------
#errors mean should be zero
mean(predictions$residuals) # = 0.000000


## ---------------------------------------------------------------------
#errors should have constant variance 
ggplot(predictions) +
  geom_point(aes(x = tail_l, y = residuals))

#looks pretty good, no structure, no increasing or decreasing variance 

