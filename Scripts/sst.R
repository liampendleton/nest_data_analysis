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

## Address different timescales
sst.1 <- sst.2 <- sst.3 <- sst.4 <- sst.5 <- rep(NA,28)

# full-year; May(t-1):Apr(t)
for(p in 1:28){
  sst.1[p] <- mean(sst_vec[((p*12)-11) : (p*12)])
}

# full-year; Oct(t-1):Sep(t)
for(p in 1:28){
  sst.2[p] <- mean(sst_vec[((p*12)-6) : ((p*12)+5)])
}

# winter; Oct(t-1):Mar(t)
for(p in 1:28){
  sst.3[p] <- mean(sst_vec[((p*12)-6) : ((p*12)-1)])
}

# pre-breed; Jan(t):Apr(t)
for(p in 1:28){
  sst.4[p] <- mean(sst_vec[((p*12)-3) : (p*12)])
}

# breed; May(t):Sep(t)
for(p in 1:(28-1)){
  sst.5[p] <- mean(sst_vec[((p*12)+1) : ((p*12)+5)])
}

return(list(sst1 = sst.1,
            sst2 = sst.2,
            sst3 = sst.3,
            sst4 = sst.4,
            sst5 = sst.5,
            sst6 = sst_vec))
}