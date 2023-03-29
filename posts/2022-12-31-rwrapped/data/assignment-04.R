## ----setup, include=FALSE---------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
# Load all the packages needed here
library(tidyverse)
library(readr)
library(gt)
library(tufte)
library(feasts)
library(janitor)

# Set your filepath here! Or, set this up as an .Rproj if you'd like.
rootdir <- ("/Users/alexreed/Documents/MEDS/Courses/EDS_222")
setwd(file.path(rootdir,"homework","HW4"))


## ---------------------------------------------------------------------
data("state")
state_df <- as.data.frame(state.x77) |>
  add_column(region = state.region, state = state.name) |>
  clean_names()


## ---------------------------------------------------------------------
#create a data table with the mean and standard deviation of column "frost" for each region
state_dt <- state_df |>
  group_by(region) |>
  summarise(across(.cols = frost, list(mean = mean, sd = sd, var = var)))

knitr::kable(state_dt)


## ---------------------------------------------------------------------
#compute point estimate
mu_north_central = state_df |>
  filter(region == "North Central") |>
  summarize(mean(frost))

mu_south = state_df |>
  filter(region == "South") |>
  summarize(mean(frost))

point_est = as.numeric(mu_north_central - mu_south)
print(point_est) #74.2

#compute standard error
n1 = state_df |> 
  filter(region == "North Central") |>
  count()
n2 = state_df |>
  filter(region == "South") |>
  count()
s1 = state_df |> 
  filter(region == "North Central") |>
  summarize(sd(frost))
s2 = state_df |> 
  filter(region == "South") |> 
  summarize(sd(frost))

SE = as.numeric(sqrt(s1^2/n1 + s2^2/n2))
print(SE) # 10.4

#compute the test statistic 
zscore = (point_est - 0)/SE
print(zscore) #7.11

#quantify the probability that your sample statistic differs from the null
p_value <- (1 - pt(zscore, df = 26)) *2 
print(p_value)

#p_value1 <- pt(zscore, df = 26, lower.tail = FALSE) *2 # want area from both tails
# print(p_value1)


## ---------------------------------------------------------------------
crit_val = qnorm(0.025, lower.tail=FALSE)

ci_lower = round(point_est - crit_val*SE, 2)
ci_upper = round(point_est + crit_val*SE, 2)

print(paste0("95% probability that [", ci_lower, " ,", ci_upper, "] contains the difference in mean frost days across North Central and South regions."))


## ---------------------------------------------------------------------
#t.test(frost ~ region, data = state_df) #levels error - maybe try filtering 

t.test(state_df$frost[state_df$region=="North Central"], state_df$frost[state_df$region=="South"])


## ---------------------------------------------------------------------
one_tail <- t.test(state_df$frost[state_df$region=="North Central"], state_df$frost[state_df$region=="South"], alternative = "greater")

one_tail_p_value <- one_tail$p.value


## ---------------------------------------------------------------------
model <- lm(murder ~ frost, data = state_df) |>
  summary()
model

beta0_intercept <- round(model$coefficients[1], 2)
beta1_frost <- round(model$coefficients[2], 3)


## ---------------------------------------------------------------------
model_p_value <- model$coefficients[[8]]
model_p_value


## ---------------------------------------------------------------------
model_point_est <- model$coefficients[2, "Estimate"]
model_SE <- model$coefficients[2, "Std. Error"]

model_point_est
model_SE

crit_val_95 <- qnorm(0.025, lower.tail=FALSE)

ci_lower_95 <- round(model_point_est - crit_val_95*model_SE, 3)
ci_upper_95 <- round(model_point_est + crit_val_95*model_SE, 3)


## ---------------------------------------------------------------------
# critical quantile value for 90% confidence interval
crit_val_90 = qnorm(0.05, lower.tail=FALSE)
crit_val_90

ci_lower_90 <- round(beta1_frost - crit_val_90*model_SE, 3)
ci_upper_90 <- round(beta1_frost + crit_val_90*model_SE, 3)


## ---------------------------------------------------------------------
mdeaths <- as_tsibble(mdeaths)

mdeaths_plot <- ggplot(data = mdeaths, 
                       aes(x = as.Date(index),
                           y = value)) +
  geom_line(color = "red") + 
  labs(x = "Date",
       y = "Deaths per Month",
       title = "Lung Disease Deaths in Males in the UK (between 1974 and 1979)") +
  theme_minimal()

mdeaths_plot


## ---------------------------------------------------------------------
decomp <- as_tsibble(mdeaths) |>
  model(classical_decomposition(value, type = "additive")) |>
  components()

glimpse(decomp)

decomp_plot <- decomp |>
  autoplot() +
  labs(title = "Classical Decomposition Model of Lung Disease Deaths in Males in the UK",
       x = "Year - Month")
decomp_plot 


## ---------------------------------------------------------------------
ukts = as_tsibble(mdeaths)
acf(ukts, lag.max = 12)

