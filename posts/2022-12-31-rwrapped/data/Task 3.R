## ----setup, include=FALSE---------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ---------------------------------------------------------------------
library(tidyverse)
library(skimr)
library(GGally)


## ---------------------------------------------------------------------
names(diamonds)
dim(diamonds)
summary(diamonds)
head(diamonds)
tail(diamonds)
skimr::skim(diamonds)



## ---------------------------------------------------------------------
ggplot(data = diamonds, aes(x = price, y = carat))+
  geom_point()




## ---------------------------------------------------------------------
ggplot(data = diamonds, aes(x = carat))+
  geom_histogram(fill = "yellow", color = "orange", alpha = 0.5)


## ---------------------------------------------------------------------
ggplot(data = diamonds, aes(x = clarity, y = price))+
  geom_jitter()


