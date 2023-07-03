library(tidyverse)
library(dplyr)
library(lubridate)
library(here)


## PIGU eggs incubate for 26-32 days
## PIGU chicks fledge at 20-54 days after hatching

data <- read.csv("PI_Nest_Data_Full.csv")
data$DATE <- ymd(data$DATE) #convert from character to date format


nestdata <- data %>% 
  dplyr::select(YEAR, DATE, BOX_ID, Number_Eggs, Number_Live_Chicks, Number_Dead_Chicks) %>% #select fields of interest
  mutate(DATE = as_date(DATE), #reformat dates
         yday = yday(DATE)) %>% #get day num
  filter(!is.na(Number_Eggs)) 

success <- nestdata %>%
  dplyr::select(YEAR, BOX_ID, yday, Number_Live_Chicks) %>% #select fields of interest
  filter(Number_Live_Chicks !=0) %>% #return rows where chicks observed
  group_by(YEAR, BOX_ID) %>% 
  summarize(min_day = min(yday),
            max_day = max(yday),
            chick_days = max_day - min_day) %>%
  mutate(chick_fledge = ifelse(chick_days >= 20, 1, 0)) #if chick 20 days or older, yield 1, otherwise 0 (fledge success)

success_table <- success %>%
  group_by(YEAR) %>%
  summarize(Num_Succ_Box = sum(chick_fledge))