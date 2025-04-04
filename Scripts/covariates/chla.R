library(here)
library(tidyverse)
library(dplyr)
library(readr)

chla.fxn <- function(){
#####
#Read in chla data
chla1 <- read_csv(here("Data", "Data_Unprocessed", "erdSW2018chlamday_38ca_d77b_b178.csv"), na = c("NaN", "NA")) #this dataset begins Sep 1997 and ends Dec 2010
chla2 <- read_csv(here("Data", "Data_Unprocessed",  "erdMH1chlamday_0ced_fc2c_6f3b.csv"), na = c("NaN", "NA")) #this dataset begins Jan 2011 and ends May 2022
chla3 <- read_csv(here("Data", "Data_Unprocessed", "erdMBchlamday_LonPM180_265c_0d9f_f605.csv"), na = c("NaN", "NA")) #this dataset begins May 2022 and ends Mar 2025

chla1 <- chla1[-1,] #get rid of extra headers
chla2 <- chla2[-1,]
chla3 <- chla3[-1,]

chla1 <- transform(chla1, chlorophyll = as.numeric(chlorophyll)) #make chlorophyll column numeric
chla2 <- transform(chla2, chlorophyll = as.numeric(chlorophyll))
chla3 <- transform(chla3, chlorophyll = as.numeric(chlorophyll))

chla1.monthly <- rep(NA, length(unique(chla1$time))) #make empty vec to store monthly values
chla2.monthly <- rep(NA, length(unique(chla2$time)))
chla3.monthly <- rep(NA, length(unique(chla3$time)))


#compile monthly averages for chla1
for(i in 1:length(chla1.monthly)){
  vals1 <- chla1 %>% filter(time == unique(chla1$time)[i])
  chla1.monthly[i] <- mean(vals1$chlorophyll, na.rm = TRUE)
}

chla1.monthly[sapply(chla1.monthly, is.nan)] <- NA #change NaN to NA

#Months of Feb and March of 2008 and May of 2009 are missing from chla1
#we are adding NAs here to represent those missing months 
chla1.monthly.new <- c(chla1.monthly[1:125],NA,NA,chla1.monthly[126:138],NA,chla1.monthly[139:157])
chla1.monthly <- chla1.monthly.new

#compile monthly averages for chla2
for(j in 1:length(chla2.monthly)){
  vals2 <- chla2 %>% filter(time == unique(chla2$time)[j])
  chla2.monthly[j] <- mean(vals2$chlorophyll, na.rm = TRUE)
}
chla2.monthly[sapply(chla2.monthly, is.nan)] <- NA

#compile monthly averages for chla3
for(k in 1:length(chla3.monthly)){
  vals3 <- chla3 %>% filter(time == unique(chla3$time)[k])
  chla3.monthly[k] <- mean(vals3$chlorophyll, na.rm = TRUE)
}
chla3.monthly[sapply(chla3.monthly, is.nan)] <- NA


chla.data <- c(chla1.monthly, chla2.monthly, chla3.monthly)

return(list(chla.data = chla.data))
}