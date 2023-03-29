## ----setup, include=FALSE---------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
# Load all the packages needed here
library(tidyverse)
library(readr)
library(gt)
library(tufte)
library(cowplot)
library(here)

# Set your filepaths here! Or, set this up as an .Rproj if you'd like.
rootdir <- ("/Users/alexreed/Documents/MEDS/Courses/EDS_222")
datadir <- file.path(rootdir,"data","HW2") # The data you'll need are on Taylor, as usual
#setwd(file.path(rootdir,"homework","HW2"))


## ---- echo = T--------------------------------------------------------
x = seq(-4, 4, 0.01)


## ---- echo = TRUE, message=FALSE, warning=FALSE-----------------------
density <- dnorm(x, mean = 0, sd = 1) 
  
data.frame(x,  f = dnorm(x)) |>
     ggplot(aes(x, f)) +
     geom_line() + 
     stat_function(fun=dnorm, 
                   geom="line")


## ---- echo = TRUE, message=FALSE, warning=FALSE-----------------------
pnorm(density, mean = 0, sd = 1)




## ---- echo = TRUE, message=FALSE, warning=FALSE-----------------------



## ---- fig.fullwidth=TRUE, fig.height=4, message=FALSE, warning=FALSE----

#colombia_climate <- read.csv("data/HW2/colombia_climate.csv")

#cc <- read_csv(here::here("data", "HW2", "colombia_climate.csv"))

#cc <- readRDS(file.path(datadir, "colombia_climate.csv"))

cc <- read_csv(here("data", "HW2", "colombia_climate.csv"))



## ---- fig.fullwidth=TRUE, fig.height=4, message=FALSE, warning=FALSE----



## ---- message=FALSE, warning=FALSE------------------------------------



## ---- message=FALSE, warning=FALSE------------------------------------


