## ----setup, include=FALSE---------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(tidyverse)
library(here)
library(janitor)


## ---------------------------------------------------------------------
us_tilapia_imports <- read_csv(here::here ("data", "us_tilapia_imports.csv"))

us_tilapia_imports_long <- us_tilapia_imports |> 
  pivot_longer(cols = -country, values_to = "imports", names_to = "year")

us_tilapia_imports_num <- us_tilapia_imports_long |>
  mutate(year = as.numeric(year))

yearly_tilapia_tot <- us_tilapia_imports_num |>
  group_by(year) |>
  summarize(total = sum(imports, na.rm = TRUE))

ggplot(data = yearly_tilapia_tot, aes(x = year, y = total)) +
  geom_line() +
  labs(Title = "US tilapia imports by years", x = "Years", y = "Imports")

ggsave(here("figs", "tilapia_imports.png"),
       width = 6,
       height = 5)


## ---------------------------------------------------------------------

yearly_tilapia_country <- us_tilapia_imports_num |>
  filter(country %in% c("Ecuador", "Honduras", "Costa Rica", "Mexico"))

ggplot(data = yearly_tilapia_country, aes(x = year, y = imports)) +
  geom_line(aes(color = country)) 

ggsave(here("figs", "tilapia_imports_country.jpg"),
       width = 6,
       height = 5)


