## ----setup, include=FALSE---------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(tidyverse)


## ---------------------------------------------------------------------
alarm_report <- data.frame(
  stringsAsFactors = FALSE,
                              date = c("2020-03-14","2020-03-15","2020-04-10",
                                       "2020-04-18"),
                          building = c("Engineering-North","Bren Hall",
                                       "Engineering-South","Engr-North"),
                     alarm_message = c("10:02am – HVAC system down, facilities management alerted",
                                       "8:24am – Elevator North out of service",
                                       "12:41am – Fire alarm, UCSB fire responded and cleared",
                                       "9:58pm – Campus point emergency siren, UCPD responded")
                )



## ---------------------------------------------------------------------
alarm_report2 <- alarm_report |>
  mutate(building = str_replace(building, pattern = "Engr", replacement = "Engineering")) |>
  separate(col = building, into = c("building", "wing"), sep = "-") |>
  filter(str_detect(alarm_message, pattern = "responded")) |>
  separate(col = alarm_message, into =  c("time", "alarm_message"), sep = "–") |>
  mutate(date = ymd(date))
  



