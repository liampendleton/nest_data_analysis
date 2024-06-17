library(here)
install.packages(jagsUI)
library(jagsUI)
library(rjags)
library(tidyverse)
library(MCMCvis)

# model
cat("
model {

####################################
### Addressing missing chla data ###

l.est.chla[1] ~ dnorm(int.chla, tau.chla) #estimated first value of chla dataset
#loop over remaining months/years
for (q in 2:total.chla){
  l.est.chla[q] ~ dnorm(l.est.chla[q-1], tau.chla)
}
#back-transform
for(j in 1:total.chla){
  real.chla[j] <- exp(l.est.chla[j]) #exponentiating est.chla value; expected value of chla
}

## Address different timescales
# full-year; May(t-1):Apr(t)
for(p in 1:n.years.new){
  chla.year.1[p] <- mean(real.chla[((p*12)-11) : (p*12)])
}
# full-year; Oct(t-1):Sep(t)
for(p in 1:n.years.new){
  chla.year.2[p] <- mean(real.chla[((p*12)-6) : ((p*12)+5)])
}
# winter; Oct(t-1):Mar(t)
for(p in 1:n.years.new){
  chla.winter[p] <- mean(real.chla[((p*12)-6) : ((p*12)-1)])
}
# pre-breed; Jan(t):Apr(t)
for(p in 1:n.years.new){
  chla.pre[p] <- mean(real.chla[((p*12)-3) : (p*12)])
}
# breed; May(t):Sep(t)
for(p in 1:(n.years.new)){
  chla.breed[p] <- mean(real.chla[((p*12)+1) : ((p*12)+5)])
}

###################################
### Addressing missing sst data ###

#loop over remaining months/years
for (z in 2:total.sst){
  l.sst.data[z] ~ dnorm(l.sst.data[z-1], tau.sst)
}

# address first time point
for(k in 1:total.sst){
  real.sst[k] <- exp(l.sst.data[k]) #exponentiating est.sst value; expected value of sst
}

### Address different timescales ###
## full-year; May(t-1):Apr(t) ##
for(p in 1:n.years.new){
  sst.year.1[p] <- mean(real.sst[((p*12)-11) : (p*12)])
}

## full-year; Oct(t-1):Sep(t) ##
for(p in 1:n.years.new){
  sst.year.2[p] <- mean(real.sst[((p*12)-6) : ((p*12)+5)])
}

## winter; Oct(t-1):Mar(t) ##
for(p in 1:n.years.new){
  sst.winter[p] <- mean(real.sst[((p*12)-6) : ((p*12)-1)])
}

## pre-breed; Jan(t):Apr(t) ##
for(p in 1:n.years.new){
  sst.pre[p] <- mean(real.sst[((p*12)-3) : (p*12)])
}

## breed; May(t):Sep(t) ##
for(p in 1:(n.years.new)){
  sst.breed[p] <- mean(real.sst[((p*12)+1) : ((p*12)+5)])
}

##############
#Correlations#

##############
### Priors ###

int.chla ~ dnorm(0,1)
tau.chla <- pow(sigma.chla,-2)
sigma.chla ~ dunif(0,30)

int.sst ~ dnorm(0,1)
tau.sst <- pow(sigma.sst,-2)
sigma.sst ~ dunif(0,30)

}
",file = "cov_correlations.txt")

############
### Data ###

# Chla data
source(here("Scripts", "covariates", "chla.r"))
#adding NAs at the beginning and end of chla because 
#months of May 95 thru August 97 are missing from the front
#and months of June 22 thru Sept 23 are missing from the end
chla.data <- chla.fxn()$chla.data
chla.s1 <- c(rep(NA,28),chla.data,rep(NA,16)) #LP
l.est.chla <- log(chla.s1)

# SST data
source(here("Scripts", "covariates", "sst.r"))
#Writing out each different SST covariates from the function 
sst.data <- sst.fxn()$sst_vec
l.sst.data <- log(sst.data) #logged

# NPGO data
source(here("Scripts", "covariates", "npgo.r"))
#Writing out each different NPGO covariates from the function 
npgo.year.1 <- npgo.fxn()$npgo1
npgo.year.2 <- npgo.fxn()$npgo2
npgo.winter <- npgo.fxn()$npgo3
npgo.pre <- npgo.fxn()$npgo4
npgo.breed <- npgo.fxn()$npgo5

# PDO data
source(here("scripts", "covariates", "pdo.r"))
#Writing out each different PDO covariates from the function 
pdo.year.1 <- pdo.fxn()$pdo1 #full-year first version; May(t-1) - Apr(t)
pdo.year.2 <- pdo.fxn()$pdo2 #full-year second version; Oct(t-1) - Sep(t) 
pdo.winter <- pdo.fxn()$pdo3 #winter; Oct(t-1) - Mar(t)
pdo.pre <- pdo.fxn()$pdo4 #pre-breeding season; Jan(t) - Apr(t)
pdo.breed <- pdo.fxn()$pdo5 #breeding season; May(t) - Sep(t)

data<-list(#npgo.year.1 = npgo.year.1, npgo.year.2 = npgo.year.2, 
           #npgo.winter = npgo.winter, npgo.pre = npgo.pre, npgo.breed = npgo.breed,  
           #pdo.year.1 = pdo.year.1, pdo.year.2 = pdo.year.2, 
           #pdo.winter = pdo.winter, pdo.pre = pdo.pre, pdo.breed = pdo.breed,  
           total.chla = length(l.est.chla),
           l.est.chla = l.est.chla, 
           n.years.new = 28,
           total.sst = length(sst.data),
           l.sst.data = l.sst.data)

parameters<-c('sst.year.1','sst.year.2','sst.breed','sst.winter','sst.pre',
              'chla.year.1','chla.year.2','chla.breed','chla.winter','chla.pre')

inits<-function() {list(int.S = runif(1)) }

##############
### Output ###

out <- jagsUI::jags(data = data ,
                    inits = inits,
                    parameters.to.save = parameters,
                    model.file = "cov_correlations.txt",
                    n.chains = 3,
                    #n.thin = 1, 
                    n.iter = 50000, #play with this
                    n.burnin = 5000, #play with this
                    n.adapt = 100)

# Extracting samples from MCMC
out.sst.breed <- out$sims.list$sst.breed
sst.breed <- apply(out.sst.breed,2,mean)

cor(sst.breed,pdo.breed)
# Extracting the summary jags data
out$summary

