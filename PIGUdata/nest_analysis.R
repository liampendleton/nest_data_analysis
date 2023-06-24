library(tidyverse)
library(dplyr)
library(lubridate)

## PIGU eggs incubate for 26-32 days
## PIGU chicks fledge at 20-54 days after hatching

data <- read.csv("PI_Nest_Data_Full.csv")
data$DATE <- ymd(data$DATE) #convert from character to date format

nestdata <- data[, c(1:2, 4, 9:11)] #isolate data of interest
nestdata <- replace(nestdata, is.na(nestdata), 0) #change 'NA' to '0'

box_IDs <- unique(data$BOX_ID) #all unique box IDs
years <- unique(nestdata$YEAR) #all unique years

## do we want to know overall success per year? or a more intimate look at each box? here is the former.
success <- rep(0, length(years)) #empty vector to store nest successes
failure <- rep(0, length(years)) #empty vector to store nest failures
nestsuccess <- years %>% cbind(success) %>% cbind(failure) #matrix to record yearly outcomes

###############################################

for (i in min(years):years) {                 #run through each year in dataset
  yeardata <- subset(nestdata, YEAR == i)     #store all data for each successive year in temp variable
  for (j in box_IDs) {                        #run through each box in dataset
    boxyear <- subset(yeardata, BOX_ID == j)  #store data for each box in temp variable
    chickrange <- which(boxyear$Number_Live_Chicks == 1)  #positions of chick observations
    maxchick <- max(chickrange)     #position of last chick observation
    minchick <- min(chickrange)     #position of first chick observation
    maxdate <- boxyear[maxchick,]   #store data for final chick observation
    mindate <- boxyear[minchick,]   #store data for first chick observation
    dayschick <- yday(maxdate$DATE) - yday(mindate$DATE)    #get number of days that chick was known to be alive
    if (dayschick > 20) {}
    

      
      
  }
}








###############################################

for (i in min(years):years) {                 #run through each year in dataset
  yeardata <- subset(nestdata, YEAR == i)     #store all data for each successive year in temp variable
  for (j in 1:nrow(yeardata)) {               #run through all rows in yeardata
    boxyear <- subset(yeardata, BOX_ID == j)
    
    nestsuccess[2,j] <- 
      
      which(yeardata$Number_Live_Chicks == 1)
    
    
  }
}
if (yeardata$Number_Live_Chicks == 1 &  )



if (nestdata$YEAR == 2022 & BOX_ID == 3 & if ) {
  
} 

nestdata[]


data[data$YEAR == '2022' & data$BOX_ID == '3', c(2, 4, 9:11)] #return observations of NB3 from 2022




