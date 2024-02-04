library(dplyr)

#bind all model weights together 
weights.out <- cbind(as.matrix(out$sims.list$w.S), as.matrix(out$sims.list$w.gam)) #left four values w.S, right four w.gam

#paste the ws together to get model names 
models.out <- rep(NA,nrow(weights.out)) #empty vec to store models
for(i in 1:nrow(weights.out)){
  models.out[i] <- paste(weights.out[i,1],weights.out[i,2],weights.out[i,3],weights.out[i,4],weights.out[i,5],weights.out[i,6],weights.out[i,7],weights.out[i,8],sep="")
}

#get frequency of each model 
table <- table(models.out)
model.names <- names(table)
freqs <- rep(NA,dim(table))
for(i in 1:dim(table)){
  freqs[i] <- table[i][[1]]
}

#create a new object 
msr <- data.frame(model.names,as.numeric(freqs))
colnames(msr) <- c("model","frequency")
msr <- msr[order(msr$frequency,decreasing = TRUE),] #sort models by most support
 
#pull estimates of other parameters when top model is in place 
out.est <- matrix(unlist(out$sims.list[-c(21:22)]), ncol = 60) #all parameter estimates excluding weights
colnames(out.est) <- c("eps.S.1", "eps.S.2", "eps.S.3", "eps.S.4", "eps.S.5", "eps.S.6", "eps.S.7", "eps.S.8", "eps.S.9", "eps.S.10", 
                       "eps.S.11", "eps.S.12", "eps.S.13", "eps.S.14", "eps.S.15", "eps.S.16", "eps.S.17", "eps.S.18", "eps.S.19", "eps.S.20",
                       "eps.gam.1", "eps.gam.2", "eps.gam.3", "eps.gam.4", "eps.gam.5", "eps.gam.6", "eps.gam.7", "eps.gam.8", "eps.gam.9", "eps.gam.10", 
                       "eps.gam.11", "eps.gam.12", "eps.gam.13", "eps.gam.14", "eps.gam.15", "eps.gam.16", "eps.gam.17", "eps.gam.18", "eps.gam.19", "eps.gam.20",
                       "sigma.S", "sigma.gam", "int.chla", "sigma.chla", "int.sst", "sigma.sst", "int.S", "int.gam", "mean.S", "mean.gam", "beta.S.npgo", "beta.gam.npgo",
                       "beta.S.pdo", "beta.gam.pdo", "beta.S.sst", "beta.gam.sst", "beta.S.chla", "beta.gam.chla", "tau.total", "deviance")
model.names <- as.numeric(models.out) #this removes leading zeros, so the best model now appears as 1000000; MUST BE NUMERIC to get means at end
max.weight.model <- model.names[1] #identify best model
out.est <- cbind(model.names, out.est) #add model names
best.model.out <- out.est[(out.est[,1] == max.weight.model),] #show all parameter estimates for iterations of best model
best.model.means <- as.matrix(colMeans(best.model.out[,-1])) #get mean parameter estimates
