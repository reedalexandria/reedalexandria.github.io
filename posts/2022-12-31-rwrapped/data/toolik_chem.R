## ----setup, include=FALSE---------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

library(tidyverse)
library(here)
library(janitor)


## ---------------------------------------------------------------------
toolik_biochem <- read_csv(here::here("data", "2011_Kling_Akchem.csv"), na = ".") %>%
  clean_names()


## ---------------------------------------------------------------------
inlet_biochem <- toolik_biochem |> 
  filter(site == "Toolik Inlet") |> 
  select("p_h", "doc_u_m", "tdn_u_m")



## ---------------------------------------------------------------------
#1

mean_inlet_biochem <- vector(mode = "numeric", length = ncol(inlet_biochem))

for (i in seq_along(inlet_biochem)) {
  mean_val <- mean(inlet_biochem[[i]], na.rm = TRUE)
  mean_inlet_biochem[i] <- mean_val
}

mean_inlet_biochem

#2

apply(X = inlet_biochem, MARGIN = 2, FUN = mean, na.rm = TRUE)

#3


map_df(.x = inlet_biochem, .f = mean, na.rm = TRUE)



