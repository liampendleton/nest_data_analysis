library(here)
library(tidyverse)


#create a function that can be called on to process and show average annual PDO values from 1996-2023
pdo <- function(pdo){
pdo <- read.csv(here("Data", "PDO.csv"))

####
#PDO setup
pi_full_pdo <- pdo[which(pdo$Year >= 1995),] #all data at and after '95
pi_full_pdo[pi_full_pdo == 99.99] <- NA #assign NA to 99.99

#save vectorized version of pdo for later
pdo_vec <- pi_full_pdo[,2:13] #get rid of year column
pdo_vec <- as.matrix(pdo_vec)
pdo_vec <- t(pdo_vec)
pdo_vec <- as.numeric(pdo_vec)
pdo_vec <- pdo_vec[-c(1:4,346:348)] #remove January through April of 1995 and October through December of 2023 

## Address different timescales
# full-year; May(t-1):Apr(t)
for(p in 1:28){
  pdo.1[p] <- mean(pdo_vec[((p*12)-11) : (p*12)])
}

# full-year; Oct(t-1):Sep(t)
for(p in 1:28){
  pdo.2[p] <- mean(pdo_vec[((p*12)-6) : ((p*12)+5)])
}

# winter; Oct(t-1):Mar(t)
for(p in 1:28){
  pdo.3[p] <- mean(pdo_vec[((p*12)-6) : ((p*12)-1)])
}

# pre-breed; Jan(t):Apr(t)
for(p in 1:28){
  pdo.4[p] <- mean(pdo_vec[((p*12)-3) : (p*12)])
}

# breed; May(t):Sep(t)
for(p in 1:(28-1)){
  pdo.5[p] <- mean(pdo_vec[((p*12)+1) : ((p*12)+5)])
}

return(list = c(pdo1 = pdo.1,
                pdo2 = pdo.2,
                pdo3 = pdo.3,
                pdo4 = pdo.4,
                pdo5 = pdo.5,
                pdo6 = pdo_vec))
}
