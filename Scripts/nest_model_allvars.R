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

##add the wts/coefficients/covariates into these GLMs
  logit(S[i]) <- int.S + eps.S[year.rand[i]] + w.S[1]*beta.S.chla * chla.year.1[year[i]] + w.S[2]*beta.S.npgo * npgo.pre[year[i]]
  logit(gamma[i]) <- int.gam + eps.gam[year.rand[i]] + w.gam[1]*beta.gam.chla * chla.year.1[year[i]] + w.gam[2]*beta.gam.npgo * npgo.pre[year[i]]

} 

for(z in 1:n.years){  #random effects to explain residual annual variation 
  eps.S[z] ~ dnorm(0,tau.S) 
  eps.gam[z] ~ dnorm(0,tau.gam)
} 

for(i in 1:4){
  w.S[i] ~ dbern(0.5)
  w.gam[i] ~ dbern(0.5)
}

tau.total ~ dgamma(3.29,7.8)
tau.model.S <- tau.total/K.S
K.S <- w.S[1] + w.S[2] + w.S[3] + w.S[4] 
tau.model.gam <- tau.total/K.gam
K.gam <- w.gam[1] + w.gam[2] + w.gam[3] + w.gam[4]


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
  chla.year.1[p] <- mean(real.chla[((p*12)-11) : (p*12)])
}

##add the sampling module for sst 

##############
### Priors ###

int.chla ~ dnorm(0,1)
tau.S <- pow(sigma.S,-2)
sigma.S ~ dunif(0,10) 
tau.gam <- pow(sigma.gam,-2)
sigma.gam ~ dunif(0,10)
tau.chla <- pow(sigma.chla,-2)
sigma.chla ~ dunif(0,30)
beta.S.chla ~ dnorm(0,tau.model.S)
beta.gam.chla ~ dnorm(0,tau.model.gam)
beta.S.npgo ~ dnorm(0,tau.model.S)
beta.gam.npgo ~ dnorm(0,tau.model.gam)
#add these pieces for SST and PDO 

int.S ~ dnorm(0,1) 
int.gam ~ dnorm(0,1)

mean.S <- 1/(1+exp(-(int.S)))
mean.gam <- 1/(1+exp(-(int.gam)))

}
",file = "nest_surv_chla.txt")

############
### Data ###

source(here("Scripts", "chla.r"))
#adding NAs at the beginning and end of chla because 
#months of May 95 thru August 97 are missing from the front
#and months of June 22 thru Sept 23 are missing from the end
chla.data <- chla.fxn()$chla.data
chla.s1 <- c(rep(NA,28),chla.data,rep(NA,16)) #LP
l.est.chla <- log(chla.s1)


source(here("Scripts", "npgo.r"))
npgo.pre <- npgo.fxn()$npgo4

##add the covariate data into the model 

nests <- nests[-c(which(is.na(nests$outcome)==TRUE)),]
nests$outcome <- nests$outcome + 1

nests$year.new <- nests$year - 1995

data<-list(y = nests$outcome, year = nests$year.new,
           year.rand = as.numeric(as.factor(nests$year)),
           n.nests = dim(nests)[1], 
           n.years = length(unique(nests$year)),
           n.years.new = max(nests$year.new),
           total.chla = length(l.est.chla),
           l.est.chla = l.est.chla,
           npgo.pre = npgo.pre)

#track all the parameters, including weights and coefficients 

parameters<-c('eps.S', 'eps.gam', 'int.chla', 'sigma.S', 'sigma.gam', 'sigma.chla',
              'beta.S.chla', 'beta.gam.chla', 'int.S', 'int.gam', 'mean.S', 'mean.gam', 'beta.S.npgo',
              'beta.gam.npgo','w.S','w.gam')

inits<-function() {list(int.S = runif(1)) }

##############
### Output ###

out <- jagsUI::jags(data = data ,
                    inits = inits,
                    parameters.to.save = parameters,
                    model.file = "nest_surv_chla.txt",
                    n.chains = 3,
                    #n.thin = 1, 
                    n.iter = 5000, #play with this
                    n.burnin = 500, #play with this
                    n.adapt = 100)

# Extracting samples from MCMC
out$samples

# Extracting the summary jags data
out$summary

# Trace plots and density plots
mcmcs<- out$samples

MCMCtrace(mcmcs, type = 'both', ind = TRUE, pdf = FALSE)

# WAIC
# This probably won't run along with everything else. Might need to select and run individually after running model.
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