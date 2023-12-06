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

  logit(S[i]) <- int.S + eps.S[year.rand[i]] + beta.S.sst * sst[year[i]] #note addition of chla
  logit(gamma[i]) <- int.gam + eps.gam[year.rand[i]] + beta.gam.sst * sst[year[i]] #same here; chla

} 

for(y in 1:n.years){  #random effects to explain residual annual variation 
  eps.S[y] ~ dnorm(0,tau.S) 
  eps.gam[y] ~ dnorm(0,tau.gam)
} 

tau.S <- pow(sigma.S,-2)
sigma.S ~ dunif(0,10) 
tau.gam <- pow(sigma.gam,-2)
sigma.gam ~ dunif(0,10)
# tau.chla1 <- pow(sigma.chla1,-2) #LP
# sigma.chla1 ~ dunif(0,30) #LP
beta.S.sst ~ dunif(-10,10) #LP
beta.gam.sst ~ dunif(-10,10) #LP

int.S ~ dnorm(0,1) 
int.gam ~ dnorm(0,1)

mean.S <- 1/(1+exp(-(int.S)))
mean.gam <- 1/(1+exp(-(int.gam)))

}
",file = "nest_surv.txt")


nests <- nests[-c(which(is.na(nests$outcome)==TRUE)),]
nests$outcome <- nests$outcome + 1
# nests$year <- nests$year - 1995
# nests$year <- as.numeric(as.factor(nests$year))




data<-list(y = nests$outcome, year = nests$year - 1995,
           year.rand = as.numeric(as.factor(nests$year)),
           n.nests = dim(nests)[1], 
           n.years = length(unique(nests$year)),
           sst = pre_sst[,2])

parameters<-c('int.S','mean.S', 'int.gam', 'mean.gam') #add more parameters to track

inits<-function() {list(int.S = runif(1)) }

##This version takes about 3-5 minutes to run, but ultimately needs to be run longer
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

#Extracting samples from MCMC
out$samples

#Extracting the summary jags data
out$summary

#trace plots and density plots
mcmcs<- out$samples

MCMCtrace(mcmcs, params = 'int.S', type = 'both', ind = TRUE, pdf = TRUE,
          open_pdf = FALSE, filename = 'int.S.mcmcvis')

# #WAIC
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



