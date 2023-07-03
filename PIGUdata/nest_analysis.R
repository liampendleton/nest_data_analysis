library(tidyverse)
library(dplyr)
library(lubridate)
library(here)


## PIGU eggs incubate for 26-32 days
## PIGU chicks fledge at 20-54 days after hatching

# data <- read.csv(here("PIGUdata", "PI_Nest_Data_Full.csv"))

data <- read.csv("PI_Nest_Data_Full.csv")
data$DATE <- ymd(data$DATE) #convert from character to date format

nestdata <- data[, c(1:2, 4, 9:11)] #isolate data of interest
nestdata <- replace(nestdata, is.na(nestdata), 0) #change 'NA' to '0'

box_IDs <- unique(data$BOX_ID) #all unique box IDs
years <- sort(unique(nestdata$YEAR)) #all unique years
n.years <- length(years)



## do we want to know overall success per year? or a more intimate look at each box? here is the former.
success <- rep(0, length(years)) #empty vector to store nest successes
failure <- rep(0, length(years)) #empty vector to store nest failures
nestsuccess <- years %>% cbind(success) %>% cbind(failure) #matrix to record yearly outcomes



for (i in 1:n.years) {                 #run through each year in dataset
  yeardata <- subset(nestdata, YEAR == i)     #store all data for each successive year in temp variable
  successstorage <- rep(0, length(unique(yeardata$BOXID)))
  failurestorage <- rep(0, length(unique(yeardata$BOXID)))
  for (j in box_IDs) {                        #run through each box in dataset
    boxyear <- subset(yeardata, BOX_ID == j)  #store data for each box in temp variable
    chickrange <- which(boxyear$Number_Live_Chicks == 1)  #positions of chick observations
    maxchick <- max(chickrange)     #position of last chick observation
    minchick <- min(chickrange)     #position of first chick observation
    maxdate <- boxyear[maxchick,]   #store data for final chick observation
    mindate <- boxyear[minchick,]   #store data for first chick observation
    dayschick <- yday(maxdate$DATE) - yday(mindate$DATE)    #get number of days that chick was known to be alive
    for (k in successstorage) {
      if (dayschick => 20) {successstorage[k] <- xxx} else
        if (dayschick == "NA") {failurestorage[k] <- xxx}
    }
  }
}





