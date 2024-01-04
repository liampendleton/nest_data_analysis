## Checking covariate correlation
#run all scripts for sst.R, pdo.R, npgo.R, and chla.R
#run nest_model_chla.R to fill in missing chla data

#run correlations on all covariates from May of 1995 through September of 2023 
#this is 28 years and 5 months of data 
#these are the data used in the model 
#note that data are missing from chla (we will fix this in the model with the random walk component)
#we use complete.obs only for the correlations 

source(here("scripts","npgo.r"))


source(here("scripts","pdo.r"))
pdo()$pdo.new #here you're running the function from pdo and saving only the vector of 341 

source(here("scripts","sst.r"))
source(here("scripts","chla.r"))

# Make sure to run "source" on chla.R file
chla.s1 <- chla[,2:13]
chla.s2 <- as.matrix(chla.s1)
chla.s3 <- t(chla.s2)
chla.s4 <- as.numeric(chla.s3)
chla.s5 <- c(rep(NA,20),chla.s4)
chla.s6 <- c(chla.s5,rep(NA,10))
est.chla <- chla.s6  #these data go from May of 1995 through October of 2023
est.chla <- est.chla[-c(342)] #remove October 

sst.new <- pi_full_sst[,2:13]
sst.new <- as.matrix(sst.new)
sst.new <- t(sst.new)
sst.new <- as.numeric(sst.new)
sst.new1 <- sst.new[-c(1:4,346:348)] #remove January through April of 1995 and October through December of 2023 
sst.new <- sst.new1 

npgo.new <- pi_full_npgo[,2:13]
npgo.new <- as.matrix(npgo.new)
npgo.new <- t(npgo.new)
npgo.new <- as.numeric(npgo.new)
npgo.new1 <- npgo.new[-c(1:4,346:348)] #remove January through April of 1995 and October through December of 2023 
npgo.new <- npgo.new1 

pdo.new <- pi_full_pdo[,2:13]
pdo.new <- as.matrix(pdo.new)
pdo.new <- t(pdo.new)
pdo.new <- as.numeric(pdo.new)
pdo.new1 <- pdo.new[-c(1:4,346:348)] #remove January through April of 1995 and October through December of 2023 
pdo.new <- pdo.new1 

#chla.new <- out$summary[1:342]
#chla.new <- c(rep(NA, 4), chla.new) #start at Jan
#chla.new <- c(chla.new, rep(NA, 2)) #end at Dec; now same length as other vectors

#now we have vectors of each covariate. assess correlation.
cor.1 <- cor(x = sst.new,
             y = npgo.new,
             use = "complete.obs")

cor.2 <- cor(x = sst.new,
             y = pdo.new,
             use = "complete.obs")

cor.3 <- cor(x = sst.new,
             y = est.chla,
             use = "complete.obs")

cor.4 <- cor(x = est.chla,
             y = npgo.new,
             use = "complete.obs")

cor.5 <- cor(x = est.chla,
             y = pdo.new,
             use = "complete.obs")

cor.6 <- cor(x = npgo.new,
             y = pdo.new,
             use = "complete.obs")