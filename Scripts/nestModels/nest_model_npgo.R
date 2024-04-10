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

  logit(S[i]) <- int.S + eps.S[year.rand[i]] + beta.S.npgo * npgo.data[year[i]] #Unlike chla.R and sst.R, this can stay the same. See line 71
  logit(gamma[i]) <- int.gam + eps.gam[year.rand[i]] + beta.gam.npgo * npgo.data[year[i]] #Do not change. See line 71

} 

for(z in 1:n.years){  #random effects to explain residual annual variation 
  eps.S[z] ~ dnorm(0,tau.S) 
  eps.gam[z] ~ dnorm(0,tau.gam)
} 

##############
### Priors ###

tau.S <- pow(sigma.S,-2)
sigma.S ~ dunif(0,10) 
tau.gam <- pow(sigma.gam,-2)
sigma.gam ~ dunif(0,10)
beta.S.npgo ~ dnorm(0,1)
beta.gam.npgo ~ dnorm(0,1)

int.S ~ dnorm(0,1) 
int.gam ~ dnorm(0,1)

mean.S <- 1/(1+exp(-(int.S)))
mean.gam <- 1/(1+exp(-(int.gam)))

}
",file = "nest_surv_npgo.txt")

############
### Data ###

# NPGO data
source(here("Scripts", "covariates", "npgo.r"))
#Writing out each different NPGO covariates from the function 
npgo.year.1 <- npgo.fxn()$npgo1
npgo.year.2 <- npgo.fxn()$npgo2
npgo.winter <- npgo.fxn()$npgo3
npgo.pre <- npgo.fxn()$npgo4
npgo.breed <- npgo.fxn()$npgo5

# Nest data
nests <- read.csv(here("Data", "model_input.csv"))
nests$outcome <- nests$outcome + 1 #categorical distributions don't like 0s

nests$year.new <- nests$year - 1995

data<-list(y = nests$outcome, year = nests$year.new,
           year.rand = as.numeric(as.factor(nests$year)),
           n.nests = dim(nests)[1], 
           n.years = length(unique(nests$year)),
           npgo.data = npgo.year.1) #change out which NPGO version you want here

parameters<-c('eps.S', 'eps.gam', 'sigma.S', 'sigma.gam', 'beta.S.npgo', 'beta.gam.npgo',
              'int.S', 'int.gam', 'mean.S', 'mean.gam')

inits<-function() {list(int.S = runif(1)) }

##############
### Output ###

out <- jagsUI::jags(data = data ,
                    inits = inits,
                    parameters.to.save = parameters,
                    model.file = "nest_surv_npgo.txt",
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