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

  logit(S[i]) <- int.S + eps.S[year.rand[i]] + beta.S.sst * sst.year.1[year[i]] #change out SST covariate to 1:5 options
  logit(gamma[i]) <- int.gam + eps.gam[year.rand[i]] + beta.gam.sst * sst.year.1[year[i]] #change out SST covariate to 1:5 options

} 

for(z in 1:n.years){  #random effects to explain residual annual variation 
  eps.S[z] ~ dnorm(0,tau.S) 
  eps.gam[z] ~ dnorm(0,tau.gam)
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
### Priors ###

int.sst ~ dnorm(0,1)
tau.S <- pow(sigma.S,-2)
sigma.S ~ dunif(0,10) 
tau.gam <- pow(sigma.gam,-2)
sigma.gam ~ dunif(0,10)
tau.sst <- pow(sigma.sst,-2)
sigma.sst ~ dunif(0,30)
beta.S.sst ~ dnorm(0,1)
beta.gam.sst ~ dnorm(0,1)


int.S ~ dnorm(0,1) 
int.gam ~ dnorm(0,1)

mean.S <- 1/(1+exp(-(int.S)))
mean.gam <- 1/(1+exp(-(int.gam)))

}
",file = "nest_surv_sst.txt")

############
### Data ###

source(here("Scripts", "sst.r"))
#Writing out each different SST covariates from the function 
sst.data <- sst.fxn()$sst_vec 
l.sst.data <- log(sst.data) #logged

nests <- nests[-c(which(is.na(nests$outcome)==TRUE)),]
nests$outcome <- nests$outcome + 1

nests$year.new <- nests$year - 1995

data<-list(y = nests$outcome, year = nests$year.new,
           year.rand = as.numeric(as.factor(nests$year)),
           n.nests = dim(nests)[1], 
           n.years = length(unique(nests$year)),
           n.years.new = max(nests$year.new),
           total.sst = length(sst.data),
           l.sst.data = l.sst.data)

parameters<-c('eps.S', 'eps.gam', 'int.sst', 'sigma.S', 'sigma.gam', 'sigma.sst', 'beta.S.sst', 'beta.gam.sst',
              'int.S', 'int.gam', 'mean.S', 'mean.gam')

inits<-function() {list(int.S = runif(1)) }

##############
### Output ###

out <- jagsUI::jags(data = data ,
                    inits = inits,
                    parameters.to.save = parameters,
                    model.file = "nest_surv_sst.txt",
                    n.chains = 3,
                    #n.thin = 1, 
                    n.iter = 50000, #play with this
                    n.burnin = 5000, #play with this
                    n.adapt = 100)

# Extracting samples from MCMC
out$samples

# Extracting the summary jags data
out$summary

# Trace plots and density plots
mcmcs<- out$samples

MCMCtrace(mcmcs, type = 'both', ind = TRUE, pdf = FALSE)

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
print(waic.m0)