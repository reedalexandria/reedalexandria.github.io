## ----setup, include=FALSE---------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ---------------------------------------------------------------------

ex_1 <- expression(5 * x ^ 2)

my_derivative <- deriv(ex_1, "x")
my_derivative

#I want to find slope at x = 2.8

x <- 2.8
eval(my_derivative)



## ---------------------------------------------------------------------
gz <- expression(2 * z ^ 3 - 10.5 * z ^ 2 + 4.1)

dg_dz <- D(gz, "z")
dg_dz

# Let's evaluate the slope over a range of values

z <- seq(from = -3, to = 4, by = 0.5)

# Evaluate it!

eval(dg_dz)
  


