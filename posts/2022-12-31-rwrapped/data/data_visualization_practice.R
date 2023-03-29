## ----setup, include=FALSE---------------------------------------------
knitr::opts_chunk$set(echo = TRUE,
                      message = FALSE,
                      warning = FALSE)
library(tidyverse)
library(here)
library(janitor)

library(patchwork)
library(ggrepel)
library(gghighlight)
library(paletteer)
library(ggExtra)
library(ggbeeswarm)

library(gapminder)

library(sf)


## ---------------------------------------------------------------------
lizards <- read_csv(here("data_tidy", "lizards.csv"))


## ---------------------------------------------------------------------
lizard_counts <- lizards |>
  mutate(date = lubridate::mdy(date)) |>
  count(year = lubridate::year(date), common_name) |>
  drop_na()

# drop na's for missing counts


## ---------------------------------------------------------------------
ggplot(data = lizard_counts, aes(x = year, y = common_name)) +
  geom_tile(aes(fill = n), show.legend = FALSE) +
  geom_text(aes(label = n), color = "white", size = 3) +
  scale_fill_gradientn(colors = c("navy", "red", "orange")) +
  theme_minimal() +
  labs(title = "Lizard Count by Common Name Compared by Year", x = "Year", y = "Common Name")



## ---------------------------------------------------------------------
whiptails <- lizards |>
  filter(common_name == "western whiptail") |>
  drop.na(total_length, weight)



## ---------------------------------------------------------------------
whiptail_bee <- ggplot(data = whiptails, aes(x = sex, y = weight))+
  geom_beeswarm(size = 0.5) +
  geom_boxplot(fill = NA)


## ---------------------------------------------------------------------
whiptail_plot <- ggplot(data = whiptails, aes(x = total_length, y = weight)) +
  geom_point(aes(color = sex)) +
  theme(legend.position = "bottom") 

ggMarginal(whiptail_plot, type = "boxplot", groupColour = TRUE)



## ---------------------------------------------------------------------
# combine whiptail_bee and whiptail_plot
(whiptail_bee + whiptail_plot) / whiptail_bee / whiptail_plot & theme_dark() 
  
# + = side by side
# division(/) one on top of each other
# & is applied to all plots


## ---------------------------------------------------------------------
# or ggplot(data = lizards) +
  #geom_point(aes(x = total_length, y = weight))

ggplot(data = lizards, aes(x = total_length, y = weight)) +
  geom_point()


## ---------------------------------------------------------------------
plot_1 <- ggplot(data = lizards, aes(x = total_length)) +
  geom_histogram(color = "orange",
                 fill = "purple",
                 size = 0.5,
                 linetype = "dotted")

plot_1

# Color aesthetic - bounding, straights, points/ Fill 
# Don't put aes in hist () if changing something based on a constant 


## ---------------------------------------------------------------------
plot_2 <- ggplot(data = lizards, aes(x = total_length, y = weight)) +
  geom_point(
    shape = 22,
    color = "cyan4",
    fill = "yellow", 
    size = 4,
    alpha = 0.4
  )

plot_2

# Alpha = transperency. 0 can't see,  1 = filled


## ---------------------------------------------------------------------
plot_3 <- ggplot(data = lizards, aes(x = total_length, 
                                     y = weight)) +
  geom_point(aes(color = common_name, 
                 size = total_length),
                 alpha = 0.5) +
  theme_minimal()
              
plot_3


## ---------------------------------------------------------------------
plot_4 <- ggplot(data = lizards, aes(x = total_length, y = weight)) +
  geom_point() +
  facet_wrap(~common_name, scales = "free")

# ~ function of 
# default is assuming you want x and y axis scales to be the same, which can be good. scales = "free" changes the scale.

plot_4


## ---------------------------------------------------------------------
lizard_counts <- lizards |>
  group_by(common_name) |>
  summarize(count = n())

#or could do this (dplyr):
#lizard_counts <- lizards |>
  #count(common_name)

plot_5 <- ggplot(data = lizard_counts, aes(y = fct_reorder(common_name, count), 
                                           x = count)) +
  geom_col(aes(fill = common_name)) +
  theme_minimal()

plot_5

# convert things to a factor so that you can change the order (fct_reorder)


## ---------------------------------------------------------------------
plot_6 <- ggplot(data = lizards, aes(x = total_length, 
                           y = weight)) +
  geom_point(aes(color = common_name, 
                 shape = common_name),
             alpha = 0.6,
             size = 3) +
  facet_wrap(~common_name, scales = "free") +
  theme_light() +
  labs(x = "Total Length (mm)", 
         y = "Weight (grams",
         title = "Jornada Basain Lizard Sizes",
         subtitle = "All lizards follow the standard length-weight relationship, with Western Whiptails being largest",
         caption = "Data: Jornada Basin LTER, CCBY")
  
plot_6


## ---------------------------------------------------------------------

plot_7 <- ggplot(data = lizards, aes(x = total_length, y = weight)) +
  geom_point(aes(color = weight)) +
  scale_color_gradient(low = "purple", high = "orange")

plot_7

plot_8 <- ggplot(data = lizards, aes(x = total_length, y = weight)) +
  geom_point(aes(color = weight)) +
  scale_color_gradientn(colors = c("magenta", "cyan4", "darkorchid3", "dodgerblue", "green", "yellow"))

plot_8

plot_9 <- ggplot(data = lizards, aes(x = total_length, y = weight)) +
  geom_point(aes(color = weight)) +
  scale_color_steps(low = "red", high = "black")

plot_9

plot_10 <- ggplot(data = lizards, aes(x = total_length, y = weight)) +
  geom_point(aes(color = total_length)) +
  scale_color_steps2(low = "purple", 
                     mid = "white",
                     high = "orange", 
                     midpoint = 150, 
                     breaks = c(50, 75, 150, 180, 220, 280))
plot_10

plot_11 <- ggplot(data = lizards, aes(x = total_length, y = weight)) +
  geom_point(aes(color = total_length)) +
  scale_color_stepsn(colors = c("red", "orange", "purple", "blue"),
                     breaks = seq(from = 0, to = 300, by = 60))

plot_11


## ---------------------------------------------------------------------
# Creating a new column, overight existing column, recast common name as a factor, then put in order by median
lizards_median <- lizards |>
  mutate(common_name = fct_reorder(common_name, total_length, .fun = median))

plot_12 <- ggplot(data = lizards, aes(y = common_name, x = total_length)) +
  geom_boxplot(aes(fill = common_name))

plot_12

plot_13 <- ggplot(data = lizards_median, aes(y = common_name, x = total_length)) +
  geom_boxplot(aes(fill = common_name), show.legend = FALSE) +
  scale_fill_paletteer_d(palette = "khroma::sunset") +
  theme_minimal()

plot_13

# To look at various packets 
# View(palettes_d_names)
# View(palettes_c_names)


## ---------------------------------------------------------------------
plot_14 <- ggplot(data = lizards, aes(x = total_length, y = weight)) +
  geom_point(aes(color = weight)) +
  theme(panel.grid.major.x = element_line(color = "red"),
        panel.grid.minor.y = element_line(color = "green"),
        axis.title.x = element_text(color = "purple", size = 10),
        axis.title.y = element_text(color = "orange"),
        axis.text.y = element_text(color = "green"),
        text = element_text(size = 18),
        panel.background = element_rect(color = "purple", fill = "pink")) +
  annotate("text", x = 300, y = 50, label = "Woooo", color = "blue") +
  geom_vline(xintercept = 250, linetype = "dashed", color = "orange", size = 4)

plot_14

# could do element_blank to have the minor y not show up
# could change both axis titles by axis.title


## ---------------------------------------------------------------------
jornada_veg <- read_sf(here("data_raw", "spatial_vegetation", "doc.kml"))
                       
ggplot() +
  geom_sf(data = jornada_veg,
          aes(fill = Name),
          color = NA) +
  theme_minimal() +
  scale_fill_paletteer_d(palette = "ggthemes::manyeys")


## ---------------------------------------------------------------------
wws_lizards <- lizards |>
  filter(common_name == "western whiptail",
         site == "sand")

ggplot(data = wws_lizards, aes(x = total_length, y = weight)) +
  geom_point() +
  geom_text_repel(aes(label = toe_num))

# repels the numbers so they are not right on top of the points

gapminder |>
  filter(year == 2002, continent == "Europe") |>
  ggplot(aes(x = gdpPercap, y = lifeExp)) +
  geom_point() +
  geom_text_repel(aes(label = country), size = 3)

ggplot(data = wws_lizards, aes(x = total_length, y = weight)) +
  geom_point() +
  gghighlight(toe_num == 250, label_key = toe_num)

ggplot(data = wws_lizards, aes(x = total_length, y = weight)) +
  geom_point() +
  gghighlight(weight > 30, label_key = toe_num)

