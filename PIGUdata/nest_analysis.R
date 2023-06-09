library(tidyverse)
library(dplyr)
library(lubridate)

data <- read.csv("PI_Nest_Data_Full.csv")
data$DATE <- ymd(data$DATE) #convert from character to date format

nestdata <- data[, c(1:2, 4, 9:11)] #isolate data of interest
nestdata <- replace(nestdata, is.na(nestdata), 0) #change 'NA' to '0'

box_IDs <- unique(data$BOX_ID) #all unique box IDs



for (i in 1:nrow(nestdata)) {
  
}








data[data$YEAR == '2022' & data$BOX_ID == '4', c(2, 4, 9:11)] #return observations of NB4 from 2022




