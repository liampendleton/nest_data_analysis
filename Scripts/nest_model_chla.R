library(here)
install.packages(jagsUI)
library(jagsUI)
library(rjags)
library(tidyverse)
library(MCMCvis)

# read in data
# make sure to read in chla data from chla.R
nests <- read.csv(here("Data","model_input.csv"))

# model
cat("
model {

for(i in 1:n.nests){

  y[i] ~ dcat(omega[i,1:3]) #a categorical distribution is a multinomial distribution with an index of 1 (i.e., it is like rolling a die once and modeling the probabilities of getting a 1, 2, 3, 4, 5, or 6) 

  omega[i,1] <- (1-S[i])     #failure 
  omega[i,2] <- S[i]*(1-gamma[i])   #1 egg
  omega[i,3] <- S[i]*gamma[i]   #2 eggs 

  logit(S[i]) <- int.S + eps.S[year.rand[i]] + beta.S.chla * chla.1[year[i]] #LP
  logit(gamma[i]) <- int.gam + eps.gam[year.rand[i]] + beta.gam.chla * chla.1[year[i]] #LP

} 

for(z in 1:n.years){  #random effects to explain residual annual variation 
  eps.S[z] ~ dnorm(0,tau.S) 
  eps.gam[z] ~ dnorm(0,tau.gam)
} 


####################################
### Addressing missing chla data ###

l.est.chla[1] ~ dnorm(int.chla, tau.chla) #estimated first value of chla dataset
#loop over remaining months/years
for (q in 2:total.chla){
  l.est.chla[q] ~ dnorm(l.est.chla[q-1], tau.chla)
}

# address first time point
for(j in 1:total.chla){
  real.chla[j] <- exp(l.est.chla[j]) #exponentiating est.chla value; expected value of chla
}

## Address different timescales
# full-year; May(t-1):Apr(t)
for(p in 1:n.years.new){
  chla.1[p] <- mean(real.chla[((p*12)-11) : (p*12)])
}

# full-year; Oct(t-1):Sep(t)
for(p in 1:n.years.new){
  chla.2[p] <- mean(real.chla[((p*12)-6) : ((p*12)+5)])
}

# winter; Oct(t-1):Mar(t)
for(p in 1:n.years.new){
  chla.3[p] <- mean(real.chla[((p*12)-6) : ((p*12)-1)])
}

# pre-breed; Jan(t):Apr(t)
for(p in 1:n.years.new){
  chla.4[p] <- mean(real.chla[((p*12)-3) : (p*12)])
}

# breed; May(t):Sep(t)
for(p in 1:(n.years.new-1)){
  chla.5[p] <- mean(real.chla[((p*12)+1) : ((p*12)+5)])
}
##############
### Priors ###

int.chla ~ dnorm(0,1)
tau.S <- pow(sigma.S,-2)
sigma.S ~ dunif(0,10) 
tau.gam <- pow(sigma.gam,-2)
sigma.gam ~ dunif(0,10)
tau.chla <- pow(sigma.chla,-2) #LP
sigma.chla ~ dunif(0,30) #LP
beta.S.chla ~ dunif(-10,10) #LP
beta.gam.chla ~ dunif(-10,10) #LP
#tau.chla <- pow(sigma.chla,-2) #LP
#sigma.chla ~ dunif(0,30) #LP


int.S ~ dnorm(0,1) 
int.gam ~ dnorm(0,1)

mean.S <- 1/(1+exp(-(int.S)))
mean.gam <- 1/(1+exp(-(int.gam)))

}
",file = "nest_surv.txt")

############
### Data ###

source(here("scripts","pdo.r"))
#Writing out each different PDO covariates from the function 
pdo.year.1 <- pdo.fxn()$pdo1 #full-year first version; May(t-1) - Apr(t)
pdo.year.2 <- pdo.fxn()$pdo2 #full-year second version; Oct(t-1) - Sep(t) 
pdo.winter <- pdo.fxn()$pdo3 #winter; Oct(t-1) - Mar(t)
pdo.prebreed <- pdo.fxn()$pdo4 #pre-breeding season; Jan(t) - Apr(t)
pdo.breed <- pdo.fxn()$pdo5 #breeding season; May(t) - Sep(t)

source(here("Scripts", "npgo.r"))
#Writing out each different NPGO covariates from the function 
npgo.year.1 <- npgo.fxn()$npgo1
npgo.year.2 <- npgo.fxn()$npgo2
npgo.winter <- npgo.fxn()$npgo3
npgo.prebreed <- npgo.fxn()$npgo4
npgo.breed <- npgo.fxn()$npgo5

source(here("Scripts", "sst.r"))
#Writing out each different SST covariates from the function 
sst.year.1 <- sst.fxn()$sst1
sst.year.2 <- sst.fxn()$sst2
sst.winter <- sst.fxn()$sst3
sst.prebreed <- sst.fxn()$sst4
sst.breed <- sst.fxn()$sst5

source(here("Scripts", "chla.r"))
chla.s1 <- c(rep(NA,20),chla.data)
chla.s2 <- c(chla.s1,rep(NA,10))
est.chla <- chla.s2  #these data now go from May of 1995 through October of 2023
l.est.chla <- log(est.chla)

nests <- nests[-c(which(is.na(nests$outcome)==TRUE)),]
nests$outcome <- nests$outcome + 1

nests$year.new <- nests$year - 1995

data<-list(y = nests$outcome, year = nests$year.new,
           year.rand = as.numeric(as.factor(nests$year)),
           n.nests = dim(nests)[1], 
           n.years = length(unique(nests$year)),
           n.years.new = max(nests$year.new),
           total.chla = length(est.chla),
           l.est.chla = l.est.chla)

parameters<-c('int.S','mean.S', 'int.gam', 'mean.gam','real.chla','chla.1') #change out "chla.x" to any of the five timeframes

inits<-function() {list(int.S = runif(1)) }

##############
### Output ###

# mod.out <- jags.model("nest_surv.txt", data, inits, n.chains=3, n.adapt=100)
# out <- coda.samples(mod.out, parameters, n.iter=1000)

out <- jagsUI::jags(data = data ,
                    inits = inits,
                    parameters.to.save = parameters,
                    model.file = "nest_surv.txt",
                    n.chains = 3,
                    #n.thin = 1, 
                    n.iter = 10000 , 
                    n.burnin = 1000,
                    n.adapt = 100)

#code to examine the chla predictions by month (means only)
plot(1:332,out$summary[5:336,1])

# Extracting samples from MCMC
out$samples

# Extracting the summary jags data
out$summary

# Trace plots and density plots
mcmcs<- out$samples

MCMCtrace(mcmcs, params = 'int.S', type = 'both', ind = TRUE, pdf = TRUE,
          open_pdf = FALSE, filename = 'int.S.mcmcvis')

# WAIC
waic_mod <- jags.samples(out$model,
                         c("WAIC", "deviance"),
                         type = "mean",
                         n.iter = 10000,
                         n.burnin = 1000,
                         n.thin = 1, parameters = parameters)

waic_mod$p_waic <- waic_mod$WAIC
waic_mod$waic <- waic_mod$deviance + waic_mod$p_waic
tmp <- sapply(waic_mod, sum)
waic.m0 <- round(c(waic = tmp[["waic"]], p_waic = tmp[["p_waic"]]),1)
