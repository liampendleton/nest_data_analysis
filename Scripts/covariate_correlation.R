## Checking covariate correlation
#run all scripts for sst.R, pdo.R, npgo.R, and chla.R
#run nest_model_chla.R to fill in missing chla data

#run correlations on all covariates from May of 1995 through September of 2023 
#this is 28 years and 5 months of data 
#these are the data used in the model 
#note that data are missing from chla (we will fix this in the model with the random walk component)
#we use complete.obs only for the correlations 

############
### Data ###

source(here("scripts","pdo.r"))
#Writing out each different PDO covariates from the function 
pdo.mayapr <- pdo.fxn()$pdo1 #full-year first version; May(t-1) - Apr(t)
pdo.octsep <- pdo.fxn()$pdo2 #full-year second version; Oct(t-1) - Sep(t) 
pdo.winter <- pdo.fxn()$pdo3 #winter; Oct(t-1) - Mar(t)
pdo.prebreed <- pdo.fxn()$pdo4 #pre-breeding season; Jan(t) - Apr(t)
pdo.breed <- pdo.fxn()$pdo5 #breeding season; May(t) - Sep(t)

source(here("Scripts", "npgo.r"))
#Writing out each different NPGO covariates from the function 
npgo.mayapr <- npgo.fxn()$npgo1
npgo.octsep <- npgo.fxn()$npgo2
npgo.winter <- npgo.fxn()$npgo3
npgo.prebreed <- npgo.fxn()$npgo4
npgo.breed <- npgo.fxn()$npgo5

source(here("Scripts", "sst.r"))
#Writing out each different SST covariates from the function 
sst.mayapr <- sst.fxn()$sst1
sst.octsep <- sst.fxn()$sst2
sst.winter <- sst.fxn()$sst3
sst.prebreed <- sst.fxn()$sst4
sst.breed <- sst.fxn()$sst5

source(here("Scripts", "chla.r"))
chla.s1 <- c(rep(NA,20),chla.data)
chla.s2 <- c(chla.s1,rep(NA,10))
est.chla <- chla.s2  #these data go from May of 1995 through October of 2023
l.est.chla <- log(est.chla)

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