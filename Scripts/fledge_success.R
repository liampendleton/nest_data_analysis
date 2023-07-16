library(tidyverse)
library(dplyr)
library(lubridate)
library(here)


## PIGU eggs incubate for 26-32 days
## PIGU chicks fledge at 20-54 days after hatching
################################################################################

data <- read.csv("PI_Nest_Data_Full.csv")
data$DATE <- ymd(data$DATE) #convert from character to date format

nestdata <- data %>% 
  select(YEAR, DATE, BOX_ID, Nest_Status, Number_Eggs, Number_Live_Chicks, Number_Dead_Chicks) %>% #select fields of interest
  mutate(DATE = as_date(DATE), #reformat dates
         yday = yday(DATE)) %>% #get day num of each observation
  # filter(!is.na(Number_Eggs)) #show only rows that have eggs

################################################################################
# 
# ## This works for returning when there was a successfully fledged bird. Does not state how many birds fledged.
# success <- nestdata %>%
#   select(YEAR, BOX_ID, yday, Number_Live_Chicks) %>% #select fields of interest
#   filter(Number_Live_Chicks !=0) %>% #return rows where chicks observed
#   group_by(YEAR, BOX_ID) %>%
#   summarize(min_day = min(yday),
#             max_day = max(yday),
#             chick_days = max_day - min_day) %>% #count minimum days chicks were in box
#   mutate(chick_fledge = ifelse(chick_days >= 20, 1, 0)) #if chick 20 days or older, yield 1, otherwise yield 0
# 
# success_table <- success %>%
#   group_by(YEAR) %>%
#   summarize(Num_Succ_Box = sum(chick_fledge))
# 
################################################################################

## Code for denoting a 1 if 1 chick reached 20 days.
test_1_chick <- nestdata %>%
  select(YEAR, BOX_ID, yday, Number_Live_Chicks) %>%
  filter(Number_Live_Chicks == 1) %>%
  unite(col = "KEY", c(YEAR, BOX_ID), sep = "-", remove = FALSE) %>%
  group_by(KEY) %>%
  summarize(min_day = min(yday),
            max_day = max(yday),
            chick_days = max_day - min_day) %>%
  mutate(chick2_fledge = ifelse(chick_days >= 20, 1, 0)) 

chick_1_key <- unique(test_1_chick$KEY)

## Code for denoting a 2 if 2 chicks reached 20 days.
test_2_chick <- nestdata %>%
          select(YEAR, BOX_ID, yday, Number_Live_Chicks) %>%
          filter(Number_Live_Chicks == 2) %>%
          unite(col = "KEY", c(YEAR, BOX_ID), sep = "-", remove = FALSE) %>%
          group_by(KEY) %>%
          summarize(min_day = min(yday),
                    max_day = max(yday),
                    chick_days = max_day - min_day) %>%
          mutate(chick2_fledge = ifelse(chick_days >= 20, 2, 0))

chick_2_key <- unique(test_2_chick$KEY)

########################################

test <- chick_2_key %in% chick_1_key #show all instances in which chick_2_key appear in chick_1_key
overlap <- chick_1_key[test == TRUE] #show all chick_1_key where there is overlap
test2 <- anti_join(test_1_chick, test_2_chick, by = "KEY") #show all 1-chick outcomes that are unique from 2-chick output
no_overlap <- bind_rows(test_2_chick, test2) #bind these results together

################################################################################


# ## This almost works. Note in the output rows 4 and 5. Having difficulty creating result when chicks drops from 2 to 1.
# test2 <- nestdata %>%
#   select(YEAR, BOX_ID, yday, Number_Live_Chicks) %>%
#   filter(Number_Live_Chicks != 0) %>%
#   group_by(YEAR, BOX_ID, Number_Live_Chicks) %>%
#   summarize(min_day = min(yday),
#             max_day = max(yday),
#             chick_days = max_day - min_day) %>%
#   mutate(chick2_fledge = ifelse(chick_days >= 20 &
#                                 Number_Live_Chicks == 2, 2, 0)) %>%
#   mutate(chick1_fledge = ifelse(chick_days >= 20 &
#                                   Number_Live_Chicks == 1, 1, 0))
#         
                      
          
  