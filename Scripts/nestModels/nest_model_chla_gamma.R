library(here)
install.packages(jagsUI)
library(jagsUI)
library(rjags)
library(tidyverse)
library(MCMCvis)

# model
cat("
model {

for(i in 1:n.nests){

  y[i] ~ dcat(omega[i,1:3]) #a categorical distribution is a multinomial distribution with an index of 1 (i.e., it is like rolling a die once and modeling the probabilities of getting a 1, 2, 3, 4, 5, or 6) 

  omega[i,1] <- (1-S[i])     #failure 
  omega[i,2] <- S[i]*(1-gamma[i])   #1 egg
  omega[i,3] <- S[i]*gamma[i]   #2 eggs 

  logit(S[i]) <- int.S + eps.S[year.rand[i]]
  logit(gamma[i]) <- int.gam + eps.gam[year.rand[i]] + beta.gam.chla * chla.year.1[year[i]] #change out chla covariate to 1:5 options

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

##############
### Priors ###

int.chla ~ dnorm(0,1)
tau.S <- pow(sigma.S,-2)
sigma.S ~ dunif(0,10) 
tau.gam <- pow(sigma.gam,-2)
sigma.gam ~ dunif(0,10)
tau.chla <- pow(sigma.chla,-2)
sigma.chla ~ dunif(0,30)
beta.gam.chla ~ dnorm(0,1)

int.S ~ dnorm(0,1) 
int.gam ~ dnorm(0,1)

mean.S <- 1/(1+exp(-(int.S)))
mean.gam <- 1/(1+exp(-(int.gam)))

}
",file = "nest_surv_chla.txt")

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

# Nest data
nests <- read.csv(here("Data", "model_input.csv"))
nests$outcome <- nests$outcome + 1 #categorical distributions don't like 0s

nests$year.new <- nests$year - 1995

data<-list(y = nests$outcome, year = nests$year.new,
           year.rand = as.numeric(as.factor(nests$year)),
           n.nests = dim(nests)[1], 
           n.years = length(unique(nests$year)),
           n.years.new = max(nests$year.new),
           total.chla = length(l.est.chla),
           l.est.chla = l.est.chla)

parameters<-c('eps.S', 'eps.gam', 'int.chla', 'sigma.S', 'sigma.gam', 'sigma.chla',
              'beta.gam.chla', 'int.S', 'int.gam', 'mean.S', 'mean.gam')

inits<-function() {list(int.S = runif(1)) }

##############
### Output ###

out <- jagsUI::jags(data = data ,
                    inits = inits,
                    parameters.to.save = parameters,
                    model.file = "nest_surv_chla.txt",
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
