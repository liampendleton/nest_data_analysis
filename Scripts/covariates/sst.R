library(here)
library(tidyverse)

#create a function that can be called on to process and show average annual SST values from 1995-2023
sst.fxn <- function(){
sst <- read.csv(here("Data", "racerocks_mSST.csv"))

####
#Format SST
sst[sst == 999.99] <- NA #reformat NA
pi_full_sst <- sst[which(sst$YEAR >= 1995),] #isolate years of interest

#save vectorized version of sst for later
sst_vec <- pi_full_sst[,2:13] #get rid of year column
sst_vec <- as.matrix(sst_vec)
sst_vec <- t(sst_vec)
sst_vec <- as.numeric(sst_vec)
sst_vec <- sst_vec[-c(1:4,346:348)] #remove January through April of 1995 and October through December of 2023 

return(list(sst_vec = sst_vec))
}