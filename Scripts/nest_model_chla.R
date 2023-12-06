library(here)
install.packages(jagsUI)
library(jagsUI)
library(rjags)
library(tidyverse)
library(MCMCvis)

#read in data 
nests <- read.csv(here("Data","model_input.csv"))


cat("
model {

for(i in 1:n.nests){

  y[i] ~ dcat(omega[i,1:3]) #a categorical distribution is a multinomial distribution with an index of 1 (i.e., it is like rolling a die once and modeling the probabilities of getting a 1, 2, 3, 4, 5, or 6) 

  omega[i,1] <- (1-S[i])     #failure 
  omega[i,2] <- S[i]*(1-gamma[i])   #1 egg
  omega[i,3] <- S[i]*gamma[i]   #2 eggs 

  logit(S[i]) <- int.S + eps.S[year.rand[i]] + beta.S.chla * chla[year[i]] #LP
  logit(gamma[i]) <- int.gam + eps.gam[year.rand[i]] + beta.gam.chla * chla[year[i]] #LP

} 

for(y in 1:n.years){  #random effects to explain residual annual variation 
  eps.S[y] ~ dnorm(0,tau.S) 
  eps.gam[y] ~ dnorm(0,tau.gam)
} 

####################################
### Addressing missing chla data ###
# vectorize chla data

# address first time point
est.chla[1] ~ dnorm(0, 0.001) #estimated first value of chla dataset
expd.chla[1] <- exp(est.chla[1]) #exponentiating est.chla value; expected value of chla in normal space

#loop over remaining months/years
for (y in 2:total.chla){
  est.chla[y] ~ dnorm(est.chla[y-1], inv_q)
  expd.chla[y] <- exp(est.chla[y])
}

##############
### Priors ###

tau.S <- pow(sigma.S,-2)
sigma.S ~ dunif(0,10) 
tau.gam <- pow(sigma.gam,-2)
sigma.gam ~ dunif(0,10)
tau.chla <- pow(sigma.chla,-2) #LP
sigma.chla ~ dunif(0,30) #LP
beta.S.chla ~ dunif(-10,10) #LP
beta.gam.chla ~ dunif(-10,10) #LP
inv_q ~ dgamma(0.001, 0.001) # prior on the process precision; LP


int.S ~ dnorm(0,1) 
int.gam ~ dnorm(0,1)

mean.S <- 1/(1+exp(-(int.S)))
mean.gam <- 1/(1+exp(-(int.gam)))

}
",file = "nest_surv.txt")

############
### Data ###

# Make sure to run "source" on chla.R file
est.chla <- t(as.matrix(chla[,2:13]))
est.chla <- log(as.numeric(est.chla))

nests <- nests[-c(which(is.na(nests$outcome)==TRUE)),]
nests$outcome <- nests$outcome + 1

data<-list(y = nests$outcome, year = nests$year - 1995,
           year.rand = as.numeric(as.factor(nests$year)),
           n.nests = dim(nests)[1], 
           n.years = length(unique(nests$year)),
           total.chla = est.chla
           )

parameters<-c('int.S','mean.S', 'int.gam', 'mean.gam') #add more parameters to track

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
                         n.thin = 1)

waic_mod$p_waic <- waic_mod$WAIC
waic_mod$waic <- waic_mod$deviance + waic_mod$p_waic
tmp <- sapply(waic_mod, sum)
waic.m0 <- round(c(waic = tmp[["waic"]], p_waic = tmp[["p_waic"]]),1)



