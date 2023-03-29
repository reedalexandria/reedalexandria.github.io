## ----setup, include=FALSE---------------------------------------------
knitr::opts_chunk$set(echo = TRUE,
                      message = FALSE)

library(tidyverse)
library(janitor)
library(here)


## ---------------------------------------------------------------------
wb_indicators <- read_csv(here::here("data", "wb_indicators.csv"), na = c("..", ""))
wb_metadata <- read.csv(here::here("data", "wb_indicators_metadata.csv"))

# convert to na's (missing values)



## ---------------------------------------------------------------------
wb_indicators_long <- wb_indicators |>
  pivot_longer(cols = '2001 [YR2001]': '2020 [YR2020]',
              names_to = "years", 
              values_to = "indicator_value"
               )

# don't need to include cols = 


## ---------------------------------------------------------------------

wb_clean <- wb_indicators_long |>
  separate(col = years, into = c("year", "year_chr"),
           sep = " ") |> 
  select(-year_chr, -`Country Code`, -`Series Code` ) |>
  mutate(year = as.numeric(year)) |>
  drop_na("Series Name") |>
  pivot_wider(names_from = "Series Name",
              values_from = "indicator_value")

#rename columns

names(wb_clean) <- c("country", "year", "acces_clean_fuels_pp", "access_electricity_pp", "co2_emissions_kt", "fossil_fuel_cons_pct", "water_stress")

wb_subset <- wb_clean |>
  filter(country %in% c("Algeria", "Barbados", "Bulgaria", "Chile"))

ggplot(data = wb_subset, aes(x = year, y = co2_emissions_kt)) +
  geom_line(aes(color = country)) +
  facet_wrap(~country)

# sep = " ": what is the delimiter I want to separate these on? Separate on a single
# select: remove these columns space " "
# mutate: converts year column character to numeric 
# unique(wb_clean$"Series Name")
# pivot_wider: spread out the variables in the Series Name column and populate with values from indicator_valyue column
# names: rename the columns 
# filter: for countries you want
# create a line plot. x and y needs to match spelling in data. Could also include group = country after y = 
  


## ---------------------------------------------------------------------
ex_1 <- starwars |>
  filter(height > 180)

ex_2 <- starwars |>
  filter(eye_color == "blue")

ex_3 <- starwars |>
  filter(homeworld == "Naboo")


## ---------------------------------------------------------------------
ex_4 <- starwars |>
  filter(height > 180 & homeworld == "Tatooine")

ex_5 <- starwars |>
  filter(hair_color == "brown" & species == "Human")


## ---------------------------------------------------------------------
ex_6 <- starwars |>
  filter(height > 180 | eye_color == "yellow")

ex_7 <- starwars |>
  filter(homeworld == "Endor" | species == "Droid")

ex_8 <- starwars |>
  filter(eye_color  == "blue" | eye_color == "brown" | eye_color == "red")

#same as #8 but more efficient
ex_9 <- starwars |>
  filter(eye_color %in% c("blue", "brown", "red"))

# NEVER DO THIS!!
ex_10 <- starwars |>
  filter(eye_color == c("blue", "brown", "red"))

ex_11 <- starwars |>
  filter(homeworld %in% c("Endor", "Tatooine", "Naboo"),
         height < 150)

# can do , or &


## ---------------------------------------------------------------------
# not equal to human
ex_12 <- starwars |>
  filter(species != "Human")

ex_13 <- starwars |>
  filter(!species %in% c("Human", "Ewok", "Wookiee"))


## ---------------------------------------------------------------------
# Select by name
ex_14 <- wb_clean |>
  select(country, year, co2_emissions_kt)

# Select by column name range
ex_15 <- wb_clean |>
  select(year:fossil_fuel_cons_pct)

# this range and exclude this one column
ex_16 <- wb_clean |>
  select(year:fossil_fuel_cons_pct, -access_electricity_pp)


## ---------------------------------------------------------------------
ex_17 <- wb_clean |>
  filter(country %in% c("Bolivia", "Chile", "Mexico")) |>
  select(country:fossil_fuel_cons_pct)



## ---------------------------------------------------------------------
# renaming column 
ex_18 <- wb_clean |>
  select(year_new = year, 
         emissions = co2_emissions_kt,
         country_name = country)

ex_19 <- wb_clean |>
  select(year, country, water_stress) |>
  filter(country %in% c("Honduras", "Ukraine", "Poland"))

# Select only columns for country, year, and water stress, and reorder them as year, country, water stress, then filter to only include observations for any three countries of your choice


## ---------------------------------------------------------------------
# new name = old name 

ex_20 <- wb_clean |>
  rename(ffcp = fossil_fuel_cons_pct,
         ws = water_stress)

ex_21 <- wb_clean |>
  rename(co2 = co2_emissions_kt,
         acf = access_electricity_pp)


## ---------------------------------------------------------------------
# add a new column. 
# Convert from kilo tons to tons. new = old * 1000

ex_22 <- wb_clean |>
  mutate(co2_emissions_t = co2_emissions_kt * 1000) |>
  relocate(co2_emissions_t, .after = co2_emissions_kt)

ex_23 <- wb_clean |>
  mutate(yay = "YAY")

ex_24 <- wb_clean |>
  mutate(year = as.character(year))

ex_25 <- wb_clean |>
  mutate(year = as.numeric(year))

## dplyr::relocate()
ex_26 <- wb_clean |>
  relocate(water_stress, .after = year)


## ---------------------------------------------------------------------
# Calculate avg height by species

ex_27 <- starwars |>
  filter(homeworld %in% c("Naboo", "Tatooine")) |>
  group_by(species, homeworld) |>
  summarize(mean_height = mean(height, na.rm = TRUE),
            mean_mass = mean(mass, na.rm = TRUE))


## ---------------------------------------------------------------------
# frequency table:
df <- tribble(
  ~species, ~length, ~number,
  "lobsters", 12, 4,
  "lobsters", 14, 6,
  "lobsters", 15, 2
)

# we want to create a case table:
df_case <- df |>
  uncount(number)


## ---------------------------------------------------------------------
# count counts observations
# combination of group_by summarize n()

starwars |>
  count(homeworld, species)

# same as:

starwars |>
  group_by(homeworld, species) |>
  summarize(size = n(),
            mean_height = mean(height, na.rm = TRUE))


## ---------------------------------------------------------------------
ex_28 <- wb_clean |>
  filter(country %in% c("Nicaragua", "Ecuador", "Peru", "Chile") & year > 2005) |>
  select(country, year, co2_emissions_kt) |>
  mutate(co2_emissions_mt = co2_emissions_kt / 1000) |>
  group_by(country) |>
  summarize(total_co2_mt = sum(co2_emissions_mt, na.rm = TRUE))
  

