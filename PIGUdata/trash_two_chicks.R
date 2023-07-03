library(tidyverse)
library(dplyr)
library(lubridate)
library(here)


## PIGU eggs incubate for 26-32 days
## PIGU chicks fledge at 20-54 days after hatching

data <- read.csv("PI_Nest_Data_Full.csv")
data$DATE <- ymd(data$DATE) #convert from character to date format




## this code isn't great
success <- nestdat %>%
  dplyr::select(YEAR, BOX_ID, yday, Number_Live_Chicks) %>%
  filter(Number_Live_Chicks !=0) %>%
  mutate(Num_Chicks = ifelse(Number_Live_Chicks == 2, 2, 1)) %>%
  group_by(YEAR, BOX_ID) %>%
  summarize(min_day = min(yday),
            max_day = max(yday),
            chick_days = max_day - min_day) %>%
  mutate(chick_fledge = ifelse(chick_days >= 20, 1, 0))

two_chicks <- nestdat %>%
  filter(Number_Live_Chicks != 0) %>%
  unite(key, c(YEAR, BOX_ID), sep = "-", remove = FALSE) %>%
  group_by(YEAR) %>%
  mutate(two_chicks = ifelse(Number_Live_Chicks = 2, "yes", "no"))

two_chicks_boxes <- unique(two_chicks$key)

success_table <- success %>%
  group_by(YEAR) %>%
  summarize(Num_Succ_Box = sum(chick_fledge))
