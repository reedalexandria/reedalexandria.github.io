## ----setup, include=FALSE---------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(tidyverse)
library(deSolve)


## ---------------------------------------------------------------------
### Specify parameters

## Proportion in each compartment: Susceptible 0.999999, Infected 0.000001, Recovered 0 (these are directly from the example linked above)
init <- c(S = 1-1e-6, I = 1e-6, R = 0.0)

## beta: infection parameter; gamma: recovery parameter
parameters <- c(beta = 2.316, gamma = 0.261)

## Time frame
times      <- seq(0, 100, by = 5)


## ---------------------------------------------------------------------
## Build the function with all 
sir <- function(time, init, parameters) {

  with(as.list(c(init, parameters)), {

    dS <- -beta * S * I
    dI <-  beta * S * I - gamma * I
    dR <-  gamma * I
    
    return(list(c(dS, dI, dR)))
  })
}


## ---------------------------------------------------------------------
## Solve using `deSolve::ode()`
approximation <- ode(y = init, times = times, func = sir, parms = parameters)


## ---------------------------------------------------------------------
## Get output into a data frame
approx_df <- as.data.frame(approximation)

## Pivot longer so R will do the work for us: 
approx_long <- approx_df %>% 
  pivot_longer(cols = S:R, names_to = "population", values_to = "proportion")



## ---------------------------------------------------------------------
ggplot(data = approx_long, aes(x = time, y = proportion)) +
  geom_line(aes(color = population))

