library(here)
library(tidyverse)

pdo <- read.csv(here("Data", "PDO.csv"))

####
#PDO
pi_full_pdo <- pdo[which(pdo$Year >= 1995),]
pi_full_pdo[pi_full_pdo == 99.99] <- NA
pi_pre_pdo <- pi_full_pdo[,1:6]
