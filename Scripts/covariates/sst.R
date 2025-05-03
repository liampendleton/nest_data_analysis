library(here)
library(tidyverse)

#Create a function that can be called on to process and show average annual SST values from 1995-2024
sst.fxn <- function(){
sst <- read.csv(here("Data", "Data_Unprocessed", "Race_Rocks_-_Average_Monthly_Sea_Surface_Temperatures_1921-2024.csv"), skip = 1) #as of importing this on 3/27/25, data only covers through April 2024.

####
#Format SST
sst[sst == 999.99] <- NA #reformat NA
pi_full_sst <- sst[which(sst$YEAR >= 1995),] #Isolate years of interest

#save vectorized version of sst for later
sst_vec <- pi_full_sst[,2:13] #Remove year column
sst_vec <- as.matrix(sst_vec)
sst_vec <- t(sst_vec)
sst_vec <- as.numeric(sst_vec)
sst_vec <- sst_vec[-c(1:4,358:360)] #Remove January through April of 1995, and October through December of 2024

return(list(sst_vec = sst_vec))
}

#Note these data get processed later within hierarchical models, so no SST file will appear in Data_Processed.