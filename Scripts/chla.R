library(here)
library(tidyverse)
library(dplyr)
library(readr)

#####
#Read in chla data
chla1 <- read_csv(here("Data", "erdSW2018chlamday_29f3_6396_25da.csv"), na = c("NaN", "NA")) #this dataset begins Sep 1997 and ends Dec 2010
chla2 <- read_csv(here("Data", "erdMH1chlamday_ff42_85c8_cd0c.csv"), na = c("NaN", "NA")) #this dataset begins Jan 2011 and ends May 2022

chla1 <- chla1[-1,] #get rid of extra headers
chla2 <- chla2[-1,]

rownames(chla1) = seq(length=nrow(chla1)) #reset row numbers
rownames(chla2) = seq(length=nrow(chla2))

chla1 <- transform(chla1, chlorophyll = as.numeric(chlorophyll)) #make chlorophyll column numeric
chla2 <- transform(chla2, chlorophyll = as.numeric(chlorophyll))

chla1.monthly <- rep(NA, length(unique(chla1$time))) #make empty vec to store monthly values
chla2.monthly <- rep(NA, length(unique(chla2$time)))

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

chla.data <- c(chla1.monthly, chla2.monthly)

rm(chla1)
rm(chla2)
rm(chla1.monthly)
rm(chla2.monthly)
rm(vals1)
rm(vals2)
rm(i)
rm(j)