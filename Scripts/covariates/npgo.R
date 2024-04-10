library(here)


#create a function that can be called on to process and show average annual NPGO values from 1995-2023
npgo.fxn <- function(){
npgo <- read.csv(here("Data", "NPGO.csv"))     

#####
#NPGO setup
pi_full_npgo <- npgo[which(npgo$YEAR >= 1995),]
npgo_vec <- pi_full_npgo[-c(1:4),3] #remove January through April of 1995
npgo_vec <- c(npgo_vec, rep(NA, 3)) #add temp NA to get to Sept

## Address different timescales
npgo.1 <- npgo.2 <- npgo.3 <- npgo.4 <- npgo.5 <- rep(NA,28)

# full-year; May(t-1):Apr(t)
for(p in 1:28){
  npgo.1[p] <- mean(npgo_vec[((p*12)-11) : (p*12)])
}

# full-year; Oct(t-1):Sep(t)
for(p in 1:28){
  npgo.2[p] <- mean(npgo_vec[((p*12)-6) : ((p*12)+5)])
}

# winter; Oct(t-1):Mar(t)
for(p in 1:28){
  npgo.3[p] <- mean(npgo_vec[((p*12)-6) : ((p*12)-1)])
}

# pre-breed; Jan(t):Apr(t)
for(p in 1:28){
  npgo.4[p] <- mean(npgo_vec[((p*12)-3) : (p*12)])
}

# breed; May(t):Sep(t)
for(p in 1:(28)){
  npgo.5[p] <- mean(npgo_vec[((p*12)+1) : ((p*12)+5)])
}

return(list(npgo1 = npgo.1,
            npgo2 = npgo.2,
            npgo3 = npgo.3,
            npgo4 = npgo.4,
            npgo5 = npgo.5,
            npgo6 = npgo_vec))
}