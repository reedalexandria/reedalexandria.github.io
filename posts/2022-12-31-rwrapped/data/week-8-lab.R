## ---- echo = FALSE, eval = TRUE---------------------------------------
# You probably already have these packages installed, so let's just load them
library(tidyverse)
library(readr)
library(ggplot2)
library(modelr)
library(knitr)
library(broom)
library(openintro)

options(scipen = 999) # disable scientific notation

# For labs, we want to see all our code
knitr::opts_chunk$set(echo = TRUE)


## ---------------------------------------------------------------------
head(ncbirths)
ncbirths_complete <- ncbirths |>
  filter(is.na(habit)==FALSE)


## ---------------------------------------------------------------------
# point estimate is mean of smoker - mean of nonsmoker
mu_nonsmoker <- ncbirths_complete |>
  filter(habit == "nonsmoker") |>
  summarize(mean(weight))

mu_smoker <- ncbirths_complete |>
  filter(habit == "smoker") |>
  summarize(mean(weight))

point_est <- as.numeric(mu_nonsmoker - mu_smoker)
print(point_est) # = 0.31 lbs is the average weight of a baby born to a nonsmoker 


## ---------------------------------------------------------------------
n1 = ncbirths_complete |>
  filter(habit=="nonsmoker") |>
  count()

n2 = ncbirths_complete |>
  filter(habit=="smoker") |>
  count()

s1 = ncbirths_complete |>
  filter(habit=="nonsmoker") |>
  summarize(sd(weight))

s2 = ncbirths_complete |>
  filter(habit=="smoker") |>
  summarize(sd(weight))

SE = as.numeric(sqrt(s1^2/n1 +s2^2/n2))
print(SE) #our estimate of the SD of the hypothetical situation. Every time we ran the study, the plotted distribution of that difference, the SE would be 0.133


## ---------------------------------------------------------------------
zscore = (point_est - 0)/SE
print(zscore)
#z score or t statistic 


## ---------------------------------------------------------------------
pval <- 2 * pnorm(point_est, mean = 0, sd = SE, lower.tail = FALSE)
print(pval)

#significant evidence that we can reject the null because value is low
#0.05 and above = failed to reject null hypothesis 


## ---------------------------------------------------------------------
?t.test


## ---------------------------------------------------------------------
#break weight into means across habit
t.test(weight~habit, data = ncbirths_complete)
#p-value is slightly different that by hand 
# 95% chance that the interval includes the true pop - baby weighs between 0.05 and 0.57 lbs


## ---------------------------------------------------------------------
t.test(ncbirths_complete$weight[ncbirths_complete$habit=="nonsmoker"], ncbirths_complete$weight[ncbirths_complete$habit=="smoker"])


## ---------------------------------------------------------------------
t.test(ncbirths_complete$weight[ncbirths_complete$habit=="nonsmoker"] - ncbirths_complete$weight[ncbirths_complete$habit=="smoker"])


## ---------------------------------------------------------------------
#qnorm gives us quantiles 
crit_val = qnorm(0.025, lower.tail = FALSE)
crit_val

#lower.tail = FALSE returns the right wing


## ---------------------------------------------------------------------
ci_lower = round(point_est - crit_val*SE, 2) 
ci_upper = round(point_est + crit_val*SE, 2)

print(paste0("95% probabaility that [", ci_lower, ", ", ci_upper, "] contains the difference in birth weights across smoking and non smoking mothers"))


## ---------------------------------------------------------------------
t.test(weight ~ habit, data = ncbirths_complete, conf.level = .90)

