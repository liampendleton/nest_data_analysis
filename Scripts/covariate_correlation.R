## Checking covariate correlation
#run all scripts for sst.R, pdo.R, npgo.R, and chla.R
#run nest_model_chla.R to fill in missing chla data

sst.new <- pi_full_sst[,2:13]
sst.new <- as.matrix(sst.new)
sst.new <- t(sst.new)
sst.new <- as.numeric(sst.new)

npgo.new <- pi_full_npgo[,2:13]
npgo.new <- as.matrix(npgo.new)
npgo.new <- t(npgo.new)
npgo.new <- as.numeric(npgo.new)

pdo.new <- pi_full_pdo[,2:13]
pdo.new <- as.matrix(pdo.new)
pdo.new <- t(pdo.new)
pdo.new <- as.numeric(pdo.new)

chla.new <- out$summary[1:342]
chla.new <- c(rep(NA, 4), chla.new) #start at Jan
chla.new <- c(chla.new, rep(NA, 2)) #end at Dec; now same length as other vectors

#now we have vectors of each covariate. assess correlation.
cor.1 <- cor(x = sst.new,
             y = npgo.new,
             use = "complete.obs")

cor.2 <- cor(x = sst.new,
             y = pdo.new,
             use = "complete.obs")

cor.3 <- cor(x = sst.new,
             y = chla.new,
             use = "complete.obs")

cor.4 <- cor(x = chla.new,
             y = npgo.new,
             use = "complete.obs")

cor.5 <- cor(x = chla.new,
             y = pdo.new,
             use = "complete.obs")

cor.6 <- cor(x = npgo.new,
             y = pdo.new,
             use = "complete.obs")