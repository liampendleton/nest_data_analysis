library(here)

#read in saved results
out <- readRDS(here("Results/out.rds"))

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
msr <- msr[order(msr$as.numeric.freqs.,decreasing = TRUE),] #sort models by most support
msr <- cbind(msr, rep(NA, length(msr[,1])))
colnames(msr) <- c("model","frequency", "weight")

#get weights from frequencies
for (j in 1:length(msr[,1])){
  msr[j,3] <- msr[j,2]/sum(msr$frequency)
}
 
#pull estimates of other parameters when top model is in place 
out.est <- matrix(unlist(out$sims.list[-c(21:22)]), ncol = 60) #all parameter estimates excluding weights
colnames(out.est) <- c("eps.S.1", "eps.S.2", "eps.S.3", "eps.S.4", "eps.S.5", "eps.S.6", "eps.S.7", "eps.S.8", "eps.S.9", "eps.S.10", 
                       "eps.S.11", "eps.S.12", "eps.S.13", "eps.S.14", "eps.S.15", "eps.S.16", "eps.S.17", "eps.S.18", "eps.S.19", "eps.S.20",
                       "eps.gam.1", "eps.gam.2", "eps.gam.3", "eps.gam.4", "eps.gam.5", "eps.gam.6", "eps.gam.7", "eps.gam.8", "eps.gam.9", "eps.gam.10", 
                       "eps.gam.11", "eps.gam.12", "eps.gam.13", "eps.gam.14", "eps.gam.15", "eps.gam.16", "eps.gam.17", "eps.gam.18", "eps.gam.19", "eps.gam.20",
                       "sigma.S", "sigma.gam", "int.chla", "sigma.chla", "int.sst", "sigma.sst", "int.S", "int.gam", "mean.S", "mean.gam", "beta.S.npgo", "beta.gam.npgo",
                       "beta.S.pdo", "beta.gam.pdo", "beta.S.sst", "beta.gam.sst", "beta.S.chla", "beta.gam.chla", "tau.total", "deviance")

#Isolate parameter estimates from best model
out.bestmodel <- out.est[which(models.out== "01000000"),]
means <- apply(out.bestmodel,2,mean)#then get means 
CrI <- apply(out.bestmodel,2,function(x){quantile(x,probs = c(0.025,0.975))}) #then get 95% CrI
bestmodel.mean.CrI <- rbind(means, CrI) #bind the two

#second/second parameter estimates
second <- out.est[which(models.out== "00000000"),]
second.means <- apply(second,2,mean)
CrI.second <- apply(second,2,function(x){quantile(x,probs = c(0.025,0.975))})
second.mean.CrI <- rbind(second.means, CrI.second)

#avg between top two
bind.two <- rbind(out.bestmodel, second)
two.means <- apply(bind.two,2,mean)
two.avg.CrI <- apply(bind.two,2,function(x){quantile(x,probs = c(0.025,0.975))})
two.avg.mod <- rbind(two.means, two.avg.CrI)

#############################

#compile top, second, avg PHI
mean.survival <- c(bestmodel.mean.CrI[1,49], second.mean.CrI[1,49], two.avg.mod[1,49])
CrI_2.5 <- c(bestmodel.mean.CrI[2,49], second.mean.CrI[2,49], two.avg.mod[2,49])
CrI_97.5 <- c(bestmodel.mean.CrI[3,49], second.mean.CrI[3,49], two.avg.mod[3,49])
df <- data.frame(mean.survival, CrI_2.5, CrI_97.5)
df.survival <- cbind(c("Best Fit", "Second Best Fit", "Model Average"), df)
colnames(df.survival) <- c("Model", "Mean", "Lower_CrI", "Upper_CrI")

#compile top, second, avg GAMMA
mean.gam <- c(bestmodel.mean.CrI[1,50], second.mean.CrI[1,50], two.avg.mod[1,50])
CrI_2.5 <- c(bestmodel.mean.CrI[2,50], second.mean.CrI[2,50], two.avg.mod[2,50])
CrI_97.5 <- c(bestmodel.mean.CrI[3,50], second.mean.CrI[3,50], two.avg.mod[3,50])
df.gam <- data.frame(mean.gam, CrI_2.5, CrI_97.5)
df.gam <- cbind(c("Best Fit", "Second Best Fit", "Model Average"), df.gam)
colnames(df.gam) <- c("Model", "Mean", "Lower_CrI", "Upper_CrI")

#top two avg
toptwo <- rbind(out.bestmodel, second)
means <- apply(toptwo,2,mean)
CrI.toptwo <- apply(toptwo,2,function(x){quantile(x,probs = c(0.025,0.975))})
full.toptwo <- rbind(means,CrI.toptwo)


###################################
#Calculate posterior inclusion probabilities and Bayes factors for each model parameter 

Phi.inclusion <- out$sims.list$w.S
Gam.inclusion <- out$sims.list$w.gam

incl.Phi.CHLA <- mean(Phi.inclusion[,1])
BF.Phi.CHLA <- (incl.Phi.CHLA/(1-incl.Phi.CHLA))
incl.Phi.NPGO <- mean(Phi.inclusion[,2])
BF.Phi.NPGO <- (incl.Phi.NPGO/(1-incl.Phi.NPGO))
incl.Phi.PDO <- mean(Phi.inclusion[,3])
BF.Phi.PDO <- (incl.Phi.PDO/(1-incl.Phi.PDO))
incl.Phi.SST <- mean(Phi.inclusion[,4])
BF.Phi.SST <- (incl.Phi.SST/(1-incl.Phi.SST))

incl.Gam.CHLA <- mean(Gam.inclusion[,1])
BF.Gam.CHLA <- (incl.Gam.CHLA/(1-incl.Gam.CHLA))
incl.Gam.NPGO <- mean(Gam.inclusion[,2])
BF.Gam.NPGO <- (incl.Gam.NPGO/(1-incl.Gam.NPGO))
incl.Gam.PDO <- mean(Gam.inclusion[,3])
BF.Gam.PDO <- (incl.Gam.PDO/(1-incl.Gam.PDO))
incl.Gam.SST <- mean(Gam.inclusion[,4])
BF.Gam.SST <- (incl.Gam.SST/(1-incl.Gam.SST))

