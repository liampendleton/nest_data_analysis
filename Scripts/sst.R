library(here)
library(tidyverse)

sst <- read.csv(here("Data", "racerocks_mSST.csv"))

####
#SST
sst[sst == 999.99] <- NA #wouldn't want a value like that guiding our analyses
pi_full_sst <- sst[which(sst$YEAR >= 1995),] #isolate years of interest
pi_pre_sst <- pi_full_sst[,1:6] #pre-breeding season defined as Jan. 1 - May 31 