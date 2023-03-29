## ----setup, include=FALSE---------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

#load packages
library(tidyverse)
library(readr)
library(here)
library(janitor)
#library(gt)
library(ggplot2)
library(modelr)
# library(sf)
# library(tmap)
# library(leaflet)
# library(censusxy)
# library(tigris)
library(readxl)


## ---------------------------------------------------------------------
#read in CalEnviro data 
cal_enviro <- read_xlsx(here("data/calenviroscreen40resultsdatadictionary_F_2021.xlsx")) |>
  clean_names()

cal_enviro$zip <- as.character(cal_enviro$zip)

#poverty subset
cal_enviro_poverty <- cal_enviro |>
  select("california_county", "zip", "poverty") |>
  group_by(zip) |>
  summarise(avg_poverty = mean(poverty, na.rm = T)) |>
  drop_na(avg_poverty)

cal_enviro_poverty$zip <- as.character(cal_enviro_poverty$zip)


## ---------------------------------------------------------------------
#poverty histogram
poverty_hist2 <- ggplot(cal_enviro, aes(x = poverty)) +
  geom_histogram(fill = "light gray") +
  labs(x = "Poverty rate per census tract (%)",
       y = "Count",
       title = "Percent of population living below two times the federal poverty level") +
  theme_light() +
  theme(panel.grid = element_blank(),
        plot.title = element_text(size = 12))
poverty_hist2


poverty_hist <- ggplot(cal_enviro_poverty, aes(x = avg_poverty)) +
  geom_histogram(fill = "light gray") +
  labs(x = "Average poverty rate per zipcode",
       y = "Count",
       title = "Average percent of population living below two times the federal poverty level") +
  theme_light() +
  theme(panel.grid = element_blank(),
        plot.title = element_text(size = 12))
poverty_hist


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


## ---------------------------------------------------------------------
joined_poverty <- left_join(cal_enviro_poverty, cafo_df, by = "zip") 
joined_poverty["count"][is.na(joined_poverty["count"])] <- 0
joined_poverty <- mutate(joined_poverty, cafo_presence = ifelse(count==0, "not present", "present")) |>
  mutate(joined_poverty, cafo_presence_numeric = ifelse(cafo_presence=="not present", 0, 1))


## ---------------------------------------------------------------------
poverty_plot <- 
  ggplot(data = joined_poverty, aes(x = avg_poverty, 
                                    y = cafo_presence, 
                                    color = cafo_presence)) + 
  geom_point(position=position_jitter(height=0.05, width=0), 
             alpha = 0.8) +
  labs(x = "Average poverty percent", 
       y = "Cafo Presence")
  
  
  # scale_fill_manual(color = c("red", "blue")) +
  # geom_jitter(width = 0, height = 0.05, alpha = 0.8) +
  # labs(x = "Average poverty percent", y = "Cafo presence") 
poverty_plot + scale_color_manual(values = c("#aec3b0", "#124559"))



joined_plot <- ggplot(data = joined_poverty, aes(x = avg_poverty, y = cafo_presence)) +
  geom_jitter(width = 0, height = 0.05, alpha = 0.8, color = "#0b2e3b") +
   labs(x = "Average poverty percent", 
        y = "Cafo Presence")
  theme(panel.grid = element_blank())

joined_plot


## ---------------------------------------------------------------------
new <- ggplot(joined_poverty, aes(avg_poverty, cafo_presence_numeric, color = cafo_presence)) +
  stat_smooth(method="glm", 
              formula=y~x,
              alpha=0.2, size=2, color = "#598392") +
  geom_point(position=position_jitter(height=0.03, width=0), alpha = 0.8, color = "#0b2e3b") + xlab("Average poverty percent") + 
  ylab("CAFO presence")
new


## ---------------------------------------------------------------------
poverty_plot_no_color <-
ggplot(data = joined_poverty, aes(x = avg_poverty, 
                                  y =as.numeric(cafo_presence)-1, 
                                  color = cafo_presence)) + 
  geom_point(position=position_jitter(height=0.05, width=0), 
             alpha = 0.8) +
  labs(x = "Average poverty percent", 
       y = "Cafo Presence")

poverty_plot_no_color + geom_smooth(method = "lm", se = FALSE)


## ---------------------------------------------------------------------
# fit model
mod_poverty <- glm(cafo_presence_numeric~avg_poverty, data = joined_poverty, family = 'binomial')
summary(mod_poverty)

new + 
  geom_smooth(method = "lm", se = FALSE) + 
  geom_smooth(method = "glm", se = FALSE, color = "purple", 
              method.args = list(family = "binomial"))


## ---------------------------------------------------------------------
poverty_plot2 <- ggplot(data = joined_poverty, aes(x = cafo_presence, y = avg_poverty)) +
  geom_jitter(width = 0, height = 0.05, alpha = 0.8) +
   labs(x = "Cafo Presence", y = "Average poverty percent")

poverty_plot2 +
  geom_smooth(method = "lm", se = FALSE)


## ---------------------------------------------------------------------
# glm_plot <- ggplot(joined_poverty, aes(avg_poverty, cafo_presence_numeric, color = cafo_presence)) +
#   stat_smooth(method="glm", 
#               formula=y~x,
#               alpha=0.2, size=2, color = "#598392") +
#   geom_point(position=position_jitter(height=0.03, width=0), alpha = 0.8, color = "#0b2e3b") +
#   xlab("Average poverty percent") + 
#   ylab("CAFO presence")
# 
# glm_plot + scale_color_manual(values = c("#aec3b0", "#124559"))


## ---------------------------------------------------------------------
summary(mod_poverty)
confint(mod_poverty)
exp(mod_poverty$coefficients)
# 95% CI for exponentiated coefficients
exp(confint(mod_poverty))


## ---------------------------------------------------------------------
#Analysis of variance for individual terms
library(car)
Anova(mod_poverty, type="II", test="Wald")


## ---------------------------------------------------------------------
#Pseudo-R-squared
library(rcompanion)
nagelkerke(mod_poverty)


## ---------------------------------------------------------------------
#Overall p-value for model

anova(mod_poverty,
      update(mod_poverty, ~1),    # update here produces null model for comparison
      test="Chisq")

 


## ---------------------------------------------------------------------
library(sf)
library(leaflet)
library(tmap)
library(janitor)
library(readxl)
library(here)

cafo_df2<- read_xlsx(here("data/Regulated_Facility_Report_Detail.xlsx")) |>
  clean_names()

cafos_plot <- cafo_df2 |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)


cafos_plot |>
leaflet() |>
addProviderTiles(providers$Esri.WorldImagery, group = "World Imagery") |>
addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") |>
addLayersControl(baseGroups = c("World Imagery", "Toner Lite")) |>
  addCircles()


## ---------------------------------------------------------------------
zip <- st_read("data/zip/ZCTA2010.shp") |>
  clean_names() 

plot(zip)

tmap

zip_plot <- zip 




## ---------------------------------------------------------------------
mod_plus <- mod_poverty |>
  augment(type.predict = "response") |>
  mutate(y_hat = .fitted)

ggplot(mod_plus, aes(x = avg_poverty, y = y_hat)) + 
  geom_point() + 
  geom_line() + 
  scale_y_continuous("Probability of CAFO Presence", limits = c(0, 1))


## ---------------------------------------------------------------------
mod_plus <- mod_plus |>
  mutate(odds_hat = y_hat / (1 - y_hat))

ggplot(mod_plus, aes(x = avg_poverty, y = odds_hat)) + 
  geom_point() + 
  geom_line() + 
  scale_y_continuous("Odds of CAFO Presence")


## ---------------------------------------------------------------------
mod_plus <- mod_plus |> 
  mutate(log_odds_hat = log(odds_hat))

ggplot(mod_plus, aes(x = avg_poverty, y = log_odds_hat)) + 
  geom_point() + 
  geom_line() + 
  scale_y_continuous("Log(odds) of CAFO Presence")


## ---------------------------------------------------------------------
exp(cbind(OR = coef(mod_poverty), confint(mod_poverty)))


## ---- warning=FALSE, message=FALSE, results='hide'--------------------
library(pscl)
pscl::pR2(mod_poverty)["McFadden"]

