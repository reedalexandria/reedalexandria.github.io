## ----setup, include=FALSE---------------------------------------------
knitr::opts_chunk$set(echo = TRUE, message = FALSE, warning = FALSE)
library(tidyverse)
library(here)
library(janitor)
library(skimr)


## ---------------------------------------------------------------------
toolik_weather <- read_csv(here::here("data", "data-raw", "toolik_weather.csv"))


## ---------------------------------------------------------------------
# An overview:
skimr::skim(toolik_weather)

# A few other things to explore
dim(toolik_weather)
names(toolik_weather)


## ---------------------------------------------------------------------
toolik_weather <- toolik_weather %>% clean_names() # What does this do? Say it in words. Or |>

# What is this %>% thing? Meet the pipe operator! Also, as of a couple months ago, |> is a native pipe operator (versus %>%, which comes along with the tidyverse) 

# Now what are the column names?


## ---------------------------------------------------------------------
names(toolik_weather)


## ---------------------------------------------------------------------
toolik_temp_plot <- ggplot(data = toolik_weather, aes(x = as.factor(month), y = daily_air_temp_mean_c)) +
  geom_jitter(aes(color = daily_air_temp_mean_c), show.legend = FALSE) +
  scale_color_gradient(low = "blue", high = "yellow") +
  theme_minimal() +
  labs(title = "Toolik Station daily air temperature",
       x = "Month",
       y = "Daily mean air temperatures (Celsius)")

toolik_temp_plot


## ---------------------------------------------------------------------
ggsave(here::here("figs", "toolik_temp_plot.png"), height = 6, width = 7)

