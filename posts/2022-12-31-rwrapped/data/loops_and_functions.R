## ----setup, include=FALSE---------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(tidyverse)


## ---------------------------------------------------------------------
weekdays <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
transects <- c("Transect A", "Transect B", "Transect C")

matrix <- matrix(nrow = length(weekdays), ncol = length(transects))

for (i in seq_along(transects)) {
  for (j in seq_along(weekdays)) {
    combined_1 <- paste(weekdays[j], "-", transects[i])
    matrix[j, i] <- combined_1
  }
}

matrix


## ---------------------------------------------------------------------
force <- function(mass, acceleration) {
  print(paste("The resulting force is", mass * acceleration, "Newtons"))
}

force(mass = 2, acceleration = 4)
force(5, 10)


## ---------------------------------------------------------------------

fish_parms <- tribble(
  ~sci_name, ~common_name, ~a_est, ~b_est,
  "Chanos chanos", "Milkfish", 0.0905, 2.52,
  "Sphyraena barracuda", "Great barracuda", 0.0181, 3.27,
  "Caranx ignobilis", "Giant trevally", 0.0353, 3.05
)

fish_weight <- function(fish_name, tot_length) {
   filter_by_name <- filter(fish_parms, common_name == fish_name)
   filter_by_name$a_est * (tot_length) ^ (filter_by_name$b_est)
  
}

fish_weight(fish_name = "Milkfish", tot_length = 10)

vec_length <- seq(from = 0, to = 100, by = 1)
milkfish_weight <- fish_weight(fish_name = "Milkfish", tot_length = vec_length)

milkfish_weight


## ---------------------------------------------------------------------
wave_power <- function(H, T) {
   print(0.5 * (H) ^ 2 * T)
}

wave_power(H = 2, T = 4)

wave_heights <- seq(from = 0, to = 3, by = 0.2)
wave_power(T = 8, H = wave_heights)


## ---------------------------------------------------------------------
wave_power_shallow <- function(depth, H, T) {
  
  if (depth > 12) {
    message("Deep")
}

if (depth <= 12) {
  message("Shallow")
 }
  print(0.81 * H ^ 2 * T)
}

wave_power_shallow(depth = 5, H = 1, T = 5)


