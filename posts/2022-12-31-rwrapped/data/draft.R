## ----setup, include=FALSE---------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

#load packages 
#library(ggiraph)
#library(ggiraphExtra)
library(tidyverse)
library(readr)
library(here)
library(janitor)
library(gt)
library(openintro)
library(ggplot2)
library(modelr)
library(sf)
library(tmap)
library(sf)
library(leaflet)
library(censusxy)
library(tigris)
library(readxl)
#library(fuzzyjoin)


## ---------------------------------------------------------------------
#read in CalEnviro data - shape file
# Cal_Enviro_all <- st_read("data/calenviroscreen40shpf2021shp/CES4 Final Shapefile.shp") |>
#   clean_names() 

#read in CalEnviro data - xlsx file
cal_enviro <- read_xlsx(here("data/calenviroscreen40resultsdatadictionary_F_2021.xlsx")) |>
  clean_names()

cal_enviro$zip <- as.character(cal_enviro$zip)

#not sure I used the below code
#|>
  #cxy_geography(Cal_Enviro_all$latitude, Cal_Enviro_all$longitude)

#convert lat and long to census tract/block
# Cal_Enviro_all$tract <- apply(Cal_Enviro_all, 1, function(row) call_geolocator_latlon(row['latitude'], row['longitude']))

#poverty subset
cal_enviro_poverty <- cal_enviro |>
  select("california_county", "zip", "poverty") |>
  group_by(zip) |>
  summarise(avg_poverty = mean(poverty)) 
cal_enviro_poverty$zip <- as.character(cal_enviro_poverty$zip)

#poverty subset
cal_enviro_poverty <- cal_enviro |>
  select("california_county", "zip", "poverty") |>
  group_by(zip) |>
  summarise(avg_poverty = mean(poverty)) 
cal_enviro_poverty$zip <- as.character(cal_enviro_poverty$zip)

#gw threat subset
cal_enviro_gw_threat <- cal_enviro |>
  select("california_county", "zip", "groundwater_threats") |>
  group_by(zip) |>
  summarise(avg_gw = mean(groundwater_threats)) 
cal_enviro_gw_threat$zip <- as.character(cal_enviro_gw_threat$zip)

#PM 2.5 subset
cal_enviro_pm <- cal_enviro |>
  select("california_county", "zip", "pm2_5") |>
  group_by(zip) |>
  summarise(avg_pm = mean(pm2_5)) 
cal_enviro_pm$zip <- as.character(cal_enviro_pm$zip)


## ---------------------------------------------------------------------
#read in cafo data, clean, and select columns of interest 
cafo_df<- read_xlsx(here("data/Regulated_Facility_Report_Detail.xlsx")) |>
  clean_names() |>
  separate(col = facility_address, c("address", "city", "CA_zipcode"), ", ") |>
  separate(col = CA_zipcode, c("CA", "zip"), " ") |>
  drop_na(zip) |>
  select("facility_name", "zip") |>
  mutate(count = 1) |>
  group_by(zip) |>
  summarise(count = sum(count))



#read in cafo data and select columns of interest 
# cafo_df <- read_xlsx(here("data/Regulated_Facility_Report_Detail.xlsx")) |>
#   clean_names() |>
#   select("county", "facility_name", "facility_address", "latitude", "longitude")

#convert lat and long to census tract/block
#cafo_df$tract <- apply(cafo_df, 1, function(row) call_geolocator_latlon(row['latitude'], row['longitude']))
# 
# cafo_df <- cafo_df |>
#   select("county", "facility_name", "facility_address", "tract")



## ---------------------------------------------------------------------
joined_poverty <- left_join(cal_enviro_poverty, cafo_df, by = "zip") 
joined_poverty["count"][is.na(joined_poverty["count"])] <- 0
joined_poverty <- mutate(joined_poverty, cafo_presence = ifelse(count==0, 0, 1))

#convert NA values in facility_name to 0 
joined_gw_threat <- left_join(cal_enviro_gw_threat, cafo_df, by = "zip") 
joined_gw_threat["count"][is.na(joined_gw_threat["count"])] <- 0
joined_gw_threat <- mutate(joined_gw_threat, cafo_presence = ifelse(count==0, 0, 1))

joined_pm <- left_join(cal_enviro_pm, cafo_df, by = "zip") 
joined_pm["count"][is.na(joined_pm["count"])] <- 0
joined_pm <- mutate(joined_pm, cafo_presence = ifelse(count==0, 0, 1))


joined_pm_gw <- left_join(joined_pm, joined_gw_threat, by = "zip") 
#joined_pm_gw["count"][is.na(joined_pm_gw["count"])] <- 0
#joined_pm_gw <- mutate(joined_pm_gw, cafo_presence = ifelse(count==0, 0, 1))

rm(joined_pm_gw)

# joined <- full_join(Cal_Enviro_all, cafo_df) 
# #convert NA values in facility_name to 0 - making a binary variable
# joined["facility_name"][is.na(joined["facility_name"])] <- 0
# 
# joined <- mutate(joined, cafo_presence = ifelse(facility_name==0, 0, 1))

# x = NA
# joined$facility_name = ifelse(joined$facility_name == 0,0,1)
# 
# joined |>
# mutate(cafo_presence = ifelse(facility_name == 0, 0, 1)) 
# 
# joined |>
# mutate(cafo_presence = ifelse(facility_name %in% is.na(x), 0, 1)) |>
#   select("cafo_presence")

# joined |>
#   rename("ifelse(facility_name %in% is.na(x), 0, 1)" = cafo_presence)


# colnames(joined)[colnames(joined) == 'ifelse(facility_name %in% is.na(x), 0, 1)'] <- 'cafo_presence' 
# joined["cafo_presence"][is.na(joined["cafo_presence"])] <- 0
# 
#   


# joined_fuzzy <- stringdist_join(Cal_Enviro_all, cafo_df,
#                 by='tract',
#                 mode = 'left',
#                 method = "jw", #use jw distance metric
#                 max_dist=99)
               


# joined_fuzzy["facility_name"][is.na(joined_fuzzy["facility_name"])] <- 0
# 
# joined_fuzzy <- mutate(joined_fuzzy, cafo_presence = ifelse(facility_name==0, 0, 1))






## ---------------------------------------------------------------------
#visualizing a binary response

#joined_new$poverty[joined$poverty == -999.0] <- NA

# poverty_plot <- ggplot(data = joined_new, aes(x = avg_poverty, y = cafo_presence)) + 
#   geom_jitter(width = 0, height = 0.05, alpha = 0.8) +
#    labs(x = "Average poverty percent", y = "Cafo Presence") 

pm_gw_plot <- ggplot(data = joined_pm_gw, aes(x = avg_pm, y = avg_gw)) + 
  geom_jitter(width = 0, height = 0.05, alpha = 0.8) +
   labs(x = "Average PM 2.5", y = "Average Groundwater Threat") 
pm_gw_plot


## ---------------------------------------------------------------------
cafo_plot +
  geom_smooth(method = "lm", se = FALSE)


## ---------------------------------------------------------------------
# fit model
mod_poverty <- glm(cafo_presence~avg_poverty, data = joined_poverty, family = 'binomial')
summary(mod_poverty)

mod_gw_threat <- glm(cafo_presence~avg_gw, data = joined_gw_threat, family = 'binomial')
summary(mod_gw_threat)

mod_pm <- glm(cafo_presence~avg_pm, data = joined_pm, family = 'binomial')
summary(mod_pm)

mod_new <- glm(cafo_presence.x ~ 
                 avg_pm +
                 avg_gw, 
               data = joined_pm_gw, family = 'binomial')
summary(mod_new)


## ---------------------------------------------------------------------
cafo_plot + 
  geom_smooth(method = "lm", se = FALSE) + 
  geom_smooth(method = "glm", se = FALSE, color = "red", 
              method.args = list(family = "binomial"))




## ---------------------------------------------------------------------
#not sure what this is doing
cafo_odds <-  mod_gg |>
  augment(type.predict = "response") |>
  mutate(y_hat = .fitted) %>% 
  mutate(odds_hat = y_hat / (1 - y_hat))

cafo_odds


## ---------------------------------------------------------------------
# cafo_odds <- augment(mod, type.predict = "response") |>
#   mutate(presence_hat = round(.fitted))
#   
# mod_plus %>%
#   select(cafo_presence, age, transplant, .fitted, alive_hat)

