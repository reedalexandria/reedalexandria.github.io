## ----setup, include=FALSE---------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

library(tidyverse)
library(palmerpenguins)



## ---------------------------------------------------------------------

exp_1 <- expression(x ^ 2)
deriv_1 <- deriv(exp_1, "x")
deriv_1
x <- 3
eval(deriv_1)



## ---------------------------------------------------------------------

exp_2 <- expression(2 * (3 * a + 2) ^ 4 - 5)
deriv_2 <- deriv(exp_2, "a")
deriv_2
a <- 1.2
eval(deriv_2)




## ---------------------------------------------------------------------

exp_3 <- expression ((-4 * t) / (t ^ 2 + 1) ^ 3)
deriv_3 <- deriv(exp_3, "t")
deriv_3
t = 0
eval(deriv_3)




## ---------------------------------------------------------------------
ggplot(data = penguins, aes(x = bill_length_mm, y = bill_depth_mm)) + 
  geom_point(aes(color = species)) +
  labs(x = "Bill Length (mm)", 
       y = "Bill Depth (mm)",
       title = "Bill Length vs Depth")





## ---------------------------------------------------------------------

ggplot(data = penguins, aes(x = species, y = flipper_length_mm)) + 
  geom_jitter(aes(color = species)) +
  labs(x = "Species", 
       y = "Flipper Length (mm)",
       title = "Species vs Flipper Length")


