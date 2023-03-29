## ---- warning=FALSE, message=FALSE, results='hide'--------------------
#load packages
library(tidyverse)
library(readr)
library(here)
library(janitor)
library(ggplot2)
library(modelr)
library(broom)
library(readxl)
library(sjPlot)

#read in CalEnviro data 
cal_enviro <- read_xlsx(here("/Users/alexreed/Documents/MEDS/Courses/EDS_222/final_project/EDS222_Final/data/calenviroscreen40resultsdatadictionary_F_2021.xlsx")) |>
  clean_names()

cal_enviro$zip <- as.character(cal_enviro$zip)

#poverty subset
cal_enviro_poverty <- cal_enviro |>
  select("california_county", "zip", "poverty") |>
  group_by(zip) |>
  summarise(avg_poverty = mean(poverty, na.rm = T)) |>
  drop_na(avg_poverty)
cal_enviro_poverty$zip <- as.character(cal_enviro_poverty$zip)


## ---- warning=FALSE, message=FALSE, results='hide', fig.cap = 'Fig 1. Average poverty rates in California were not normally distributed. The average mean percent of the population living below two times the federal property level per zip code is 30.86%.'----
#poverty histogram
poverty_hist <- ggplot(cal_enviro_poverty, aes(x = avg_poverty)) +
  geom_histogram(fill = "light gray") +
  labs(x = "Average poverty percent",
       y = "Count",
       title = "Distribution of average poverty percent per zip code") +
  theme_light() +
  theme(panel.grid = element_blank(),
        plot.title = element_text(size = 12))
poverty_hist

#ggsave("fig1.png", width = 8, height = 5, units = "in", dpi = 300)
summary(cal_enviro_poverty)


## ---- warning=FALSE, message=FALSE, results='hide'--------------------
#read in cafo data, clean, and select columns of interest 
# create new column that sums the number of CAFOs per zip code
cafo_df<- read_xlsx(here("/Users/alexreed/Documents/MEDS/Courses/EDS_222/final_project/EDS222_Final/data/Regulated_Facility_Report_Detail.xlsx")) |>
  clean_names() |>
  separate(col = facility_address, c("address", "city", "CA_zipcode"), ", ") |>
  separate(col = CA_zipcode, c("CA", "zip"), " ") |>
  drop_na(zip) |>
  select("facility_name", "zip") |>
  mutate(count = 1) |>
  group_by(zip) |>
  summarise(count = sum(count))


## ---- warning=FALSE, message=FALSE, results='hide', fig.cap = 'Fig 2. The number of CAFOs in California were not normally distributed. The mean number of CAFOs per zip code was 9.7.'----
#CAFO histogram
cafo_hist <- ggplot(cafo_df, aes(x = count)) +
  geom_histogram(fill = "light gray") +
  labs(x = "Number of CAFOs",
       y = "Count",
       title = "Distribution of CAFOs per zip code") +
  theme_light() +
  theme(panel.grid = element_blank(),
        plot.title = element_text(size = 12))
cafo_hist

#ggsave("fig2.png", width = 8, height = 5, units = "in", dpi = 300)
#summary(cafo_df)


## ---- warning=FALSE, message=FALSE, results='hide'--------------------
#join the poverty and CAFO data sets by zip code. Make a binary variable.
joined_poverty <- left_join(cal_enviro_poverty, cafo_df, by = "zip") 
joined_poverty["count"][is.na(joined_poverty["count"])] <- 0 #NA = zip codes without CAFOs, converting to 0 instead of NA
joined_poverty <- mutate(joined_poverty, cafo_presence = ifelse(count==0, "not present", "present")) |>
  mutate(joined_poverty, cafo_presence_numeric = ifelse(cafo_presence=="not present", 0, 1))
#creating two new rows: cafo_presence = 'present' or 'not present', and cafo_presence_numeric = 1 or 0


## ---- fig.cap = 'Fig 3. Visualizing a binary response'----------------
joined_plot <- 
  ggplot(data = joined_poverty, aes(x = avg_poverty, 
                                        y = cafo_presence, 
                                        color = cafo_presence)) +
  geom_point(position=position_jitter(height=0.05, width=0), 
                 alpha = 0.8) +
      labs(x = "Average poverty percent", 
           y = "CAFO Presence")
joined_plot + scale_color_manual(values = c("#aec3b0", "#124559"))

#ggsave("fig3.png", width = 8, height = 5, units = "in", dpi = 300)


## ----warning=FALSE, message=FALSE, results='hide'---------------------
#fit model
mod_poverty <- glm(cafo_presence_numeric~avg_poverty, data = joined_poverty, family = 'binomial')
summary(mod_poverty)


## ---------------------------------------------------------------------
#model output table format
tab_model(mod_poverty,
          transform = NULL,
          pred.labels = c("Intercept", "Average Poverty (%)"),
          dv.labels = c("log CAFO Presence Pobability"),
          show.p = TRUE,
          p.style = c("numeric_stars"),
          p.threshold = c(0.10, 0.05, 0.01),
          string.p = "P-value",
          show.r2 = FALSE,
          title = "Tbl 1. Logisitc Regression Model Results for Average Poverty Percent",
          digits = 3)


## ---- warning=FALSE, message=FALSE, results='hide', fig.cap = 'Fig 4. CAFO presence by average poverty percent per zip code. As the average poverty percent increases, the probability of CAFO presence increases.'----
#visualizing logistic regression
joined_plot2 <- 
  ggplot(data = joined_poverty, aes(x = avg_poverty, 
                                    y = cafo_presence_numeric,
                                    color = cafo_presence)) + 
  geom_point(position=position_jitter(height=0.05, width=0), 
             alpha = 0.8) +
  labs(x = "Average poverty percent", 
       y = "CAFO Presence")
joined_plot2 + scale_color_manual(values = c("#aec3b0", "#124559")) +
  geom_smooth(method = "glm", se = FALSE, color = "#598392", 
              method.args = list(family = "binomial"))
#ggsave("fig4.png", width = 8, height = 5, units = "in", dpi = 300)


## ---- warning=FALSE, message=FALSE, results='hide', fig.cap = 'Fig 5: Probability of CAFO presence by poverty percent'----
mod_plus <- mod_poverty |>
  augment(type.predict = "response") |>
  mutate(y_hat = .fitted)

ggplot(mod_plus, aes(x = avg_poverty, y = y_hat)) + 
  geom_point(color = "#124559") + 
  geom_line(color = "#598392") + 
  scale_y_continuous("Probability of CAFO Presence", limits = c(0, 1)) +
  labs(x = "Average poverty percent")
ggsave("fig5.png", width = 8, height = 5, units = "in", dpi = 300)


## ----fig.cap = 'Fig 6. Odds of CAFO presence by poverty percent.'-----
mod_plus <- mod_plus |>
  mutate(odds_hat = y_hat / (1 - y_hat))

ggplot(mod_plus, aes(x = avg_poverty, y = odds_hat)) + 
  geom_point(color = "#124559") + 
  geom_line(color = "#598392") + 
  scale_y_continuous("Odds of CAFO Presence") +
  labs(x = "Average poverty percent")
ggsave("fig6.png", width = 8, height = 5, units = "in", dpi = 300)

