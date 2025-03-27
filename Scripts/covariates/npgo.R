#Create a function that can be called on to process and show average annual NPGO values from 1995-2023
# npgo.fxn <- function(){
  npgo <- read.table("https://www.o3d.org/npgo/data/NPGO.txt", #Download directly from source
                     skip = 1,
                     col.names = c("Year", "Month", "NPGO"))
  #NPGO setup
  pi_full_npgo <- npgo[which(npgo$Year >= 1995 & npgo$Year != 2025),] #Isolate period of interest; 1995-2024
  npgo_vec <- pi_full_npgo[-c(1:4, 898:900),3] #Remove January through April of 1995 and any records after Sept of 2024
  
  #Address different timescales
  npgo.1 <- npgo.2 <- npgo.3 <- npgo.4 <- npgo.5 <- rep(NA,29) #29 annual averages 
  
  #Full-year; May(t-1):Apr(t)
  for(p in 1:29){
    npgo.1[p] <- mean(npgo_vec[((p*12)-11) : (p*12)])
  }
  
  #Full-year; Oct(t-1):Sep(t)
  for(p in 1:29){
    npgo.2[p] <- mean(npgo_vec[((p*12)-6) : ((p*12)+5)])
  }
  
  #Winter; Oct(t-1):Mar(t)
  for(p in 1:29){
    npgo.3[p] <- mean(npgo_vec[((p*12)-6) : ((p*12)-1)])
  }
  
  #Pre-breed; Jan(t):Apr(t)
  for(p in 1:29){
    npgo.4[p] <- mean(npgo_vec[((p*12)-3) : (p*12)])
  }
  
  #Breed; May(t):Sep(t)
  for(p in 1:(29)){
    npgo.5[p] <- mean(npgo_vec[((p*12)+1) : ((p*12)+5)])
  }
  
  return(list(npgo1 = npgo.1,
              npgo2 = npgo.2,
              npgo3 = npgo.3,
              npgo4 = npgo.4,
              npgo5 = npgo.5,
              npgo6 = npgo_vec)) #Call on these using >npgo.fxn()$npgo1, etc.
}
