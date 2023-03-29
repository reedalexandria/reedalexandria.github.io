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


## ---- echo=TRUE-------------------------------------------------------
head(heart_transplant)

ggplot(data = heart_transplant, aes(x = age, y = survived, col = survived)) +
  geom_jitter(width = 0, height = 0.05, alpha = 0.8) + #interested in vertically jittering (height = 0.05 but width = 0)
  labs(x = "Age at time of study", y = "Survived 5 years after study")


## ---------------------------------------------------------------------
heart_transplant = heart_transplant |>
  mutate(is_alive = ifelse(survived =="alive", 1, 0))
#alive values are 1 and not alive are 0

class(heart_transplant$is_alive)
summary(heart_transplant$is_alive)


## ---- echo = TRUE-----------------------------------------------------
gg <- ggplot(data = heart_transplant, aes(x = age, y = is_alive)) +
  geom_jitter(width = 0, height = 0.05, alpha = 0.8) + 
  labs(x = "Age at time of study", y = "Is alive")

gg


## ---------------------------------------------------------------------
gg + geom_smooth(method = "lm", se = FALSE) #adding a linear fit on top of plot. Had to delete color because didn't work.
#modeling the probability of being alive based on age


## ---------------------------------------------------------------------
# fit model
mod_heart <- glm(is_alive ~ age, data = heart_transplant, family = "binomial")
summary(mod_heart)

#increase in age, decreases your chance of being alive. 1.56 - 0.0587
#on average more likely to survive than not survive (because 1.56 is greater than 1)


## ---------------------------------------------------------------------
phat_64 = exp(1.56-0.05847*64)/(1 + exp(1.56-0.05847*64))
phat_64


## ---------------------------------------------------------------------
# using the uniroot function 



## ---------------------------------------------------------------------
gg + geom_smooth(method = "lm", se = FALSE) +
  geom_smooth(method = "glm", se = FALSE, color = "red", method.args = list(family = "binomial"))


## ---------------------------------------------------------------------




## ---------------------------------------------------------------------
mod = glm(is_alive ~ age + transplant, data = heart_transplant, family = binomial)
summary(mod)
#good that with value is positive when adding transplant 


## ---------------------------------------------------------------------
# probability scale
augment(mod, type.predict = "response")



## ---------------------------------------------------------------------




## ---------------------------------------------------------------------



