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

  logit(S[i]) <- int.S + eps.S[year.rand[i]] + beta.S.pdo * pdo.data[year[i]] #Unlike chla.R and sst.R, this can stay the same. See line 73
  logit(gamma[i]) <- int.gam + eps.gam[year.rand[i]] + beta.gam.pdo * pdo.data[year[i]] #Do not change. See line 73

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
beta.S.pdo ~ dnorm(0,1)
beta.gam.pdo ~ dnorm(0,1)

int.S ~ dnorm(0,1) 
int.gam ~ dnorm(0,1)

mean.S <- 1/(1+exp(-(int.S)))
mean.gam <- 1/(1+exp(-(int.gam)))

}
",file = "nest_surv_pdo.txt")

############
### Data ###

source(here("scripts","pdo.r"))
#Writing out each different PDO covariates from the function 
pdo.year.1 <- pdo.fxn()$pdo1 #full-year first version; May(t-1) - Apr(t)
pdo.year.2 <- pdo.fxn()$pdo2 #full-year second version; Oct(t-1) - Sep(t) 
pdo.winter <- pdo.fxn()$pdo3 #winter; Oct(t-1) - Mar(t)
pdo.pre <- pdo.fxn()$pdo4 #pre-breeding season; Jan(t) - Apr(t)
pdo.breed <- pdo.fxn()$pdo5 #breeding season; May(t) - Sep(t)

nests <- nests[-c(which(is.na(nests$outcome)==TRUE)),]
nests$outcome <- nests$outcome + 1

nests$year.new <- nests$year - 1995

data<-list(y = nests$outcome, year = nests$year.new,
           year.rand = as.numeric(as.factor(nests$year)),
           n.nests = dim(nests)[1], 
           n.years = length(unique(nests$year)),
           pdo.data = pdo.year.1) #change out which pdo version you want here

parameters<-c('eps.S', 'eps.gam', 'sigma.S', 'sigma.gam', 'beta.S.pdo', 'beta.gam.pdo',
              'int.S', 'int.gam', 'mean.S', 'mean.gam')

inits<-function() {list(int.S = runif(1)) }

##############
### Output ###

out <- jagsUI::jags(data = data ,
                    inits = inits,
                    parameters.to.save = parameters,
                    model.file = "nest_surv_pdo.txt",
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