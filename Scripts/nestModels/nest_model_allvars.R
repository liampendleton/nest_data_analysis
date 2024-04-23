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

  logit(S[i]) <- int.S + eps.S[year.rand[i]] + w.S[1]*beta.S.chla * chla.year.2[year[i]] + w.S[2]*beta.S.npgo * npgo.year.1[year[i]] + w.S[3]*beta.S.pdo * pdo.pre[year[i]] + w.S[4]*beta.S.sst * sst.pre[year[i]]
  logit(gamma[i]) <- int.gam + eps.gam[year.rand[i]] + w.gam[1]*beta.gam.chla * chla.year.1[year[i]] + w.gam[2]*beta.gam.npgo * npgo.pre[year[i]] + w.gam[3]*beta.gam.pdo * pdo.winter[year[i]] + w.gam[4]*beta.gam.sst * sst.year.1[year[i]]

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
K.S <- w.S[1] + w.S[2] + w.S[3] + w.S[4] + 1
tau.model.gam <- tau.total/K.gam
K.gam <- w.gam[1] + w.gam[2] + w.gam[3] + w.gam[4] + 1


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

# full-year; May(t-1):Apr(t)
for(p in 1:n.years.new){
  chla.year.1[p] <- mean(real.chla[((p*12)-11) : (p*12)])
}

# full-year; Oct(t-1):Sep(t)
for(p in 1:n.years.new){
  chla.year.2[p] <- mean(real.chla[((p*12)-6) : ((p*12)+5)])
}

### Addressing missing SST data ### 

#loop over remaining months/years
for (z in 2:total.sst){
  l.sst.data[z] ~ dnorm(l.sst.data[z-1], tau.sst)
}

# address first time point
for(k in 1:total.sst){
  real.sst[k] <- exp(l.sst.data[k]) #exponentiating est.sst value; expected value of sst
}

# full-year; May(t-1):Apr(t)
for(p in 1:n.years.new){
  sst.year.1[p] <- mean(real.sst[((p*12)-11) : (p*12)])
}

## pre-breed; Jan(t):Apr(t) ##
for(p in 1:n.years.new){
  sst.pre[p] <- mean(real.sst[((p*12)-3) : (p*12)])
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
beta.S.chla ~ dnorm(0,tau.model.S)
beta.gam.chla ~ dnorm(0,tau.model.gam)
beta.S.npgo ~ dnorm(0,tau.model.S)
beta.gam.npgo ~ dnorm(0,tau.model.gam)
beta.S.pdo ~ dnorm(0,tau.model.S)
beta.gam.pdo ~ dnorm(0,tau.model.gam)
int.sst ~ dnorm(0,1)
tau.sst <- pow(sigma.sst,-2)
sigma.sst ~ dunif(0,30)
beta.S.sst ~ dnorm(0,tau.model.S)
beta.gam.sst ~ dnorm(0,tau.model.gam)

int.S ~ dnorm(0,1) 
int.gam ~ dnorm(0,1)

mean.S <- 1/(1+exp(-(int.S)))
mean.gam <- 1/(1+exp(-(int.gam)))

}
",file = "nest_surv_allvars.txt")

############
### Data ###

# Chla
source(here("Scripts", "covariates", "chla.r"))
#adding NAs at the beginning and end of chla because 
#months of May 95 thru August 97 are missing from the front
#and months of June 22 thru Sept 23 are missing from the end
chla.data <- chla.fxn()$chla.data
chla.s1 <- c(rep(NA,28),chla.data,rep(NA,16)) #LP
l.est.chla <- log(chla.s1)

# NPGO
source(here("Scripts", "covariates", "npgo.r"))
npgo.year.1 <- npgo.fxn()$npgo1
npgo.pre <- npgo.fxn()$npgo4

# SST
source(here("Scripts", "covariates", "sst.r"))
#Writing out each different SST covariates from the function 
sst.data <- sst.fxn()$sst_vec 
l.sst.data <- log(sst.data) #logged

# PDO
source(here("scripts", "covariates", "pdo.r"))
#Writing out each different PDO covariates from the function
pdo.pre <- pdo.fxn()$pdo4
pdo.winter <- pdo.fxn()$pdo3 #winter; Oct(t-1) - Mar(t)

# Nest data
nests <- read.csv(here("Data","model_input.csv"))
nests$outcome <- nests$outcome + 1 #categorical distributions don't like 0s

nests$year.new <- nests$year - 1995

data<-list(y = nests$outcome, year = nests$year.new,
           year.rand = as.numeric(as.factor(nests$year)),
           n.nests = dim(nests)[1], 
           n.years = length(unique(nests$year)),
           n.years.new = max(nests$year.new),
           total.chla = length(l.est.chla),
           l.est.chla = l.est.chla,
           total.sst = length(sst.data),
           l.sst.data = l.sst.data,
           pdo.winter = pdo.winter,
           npgo.year.1 = npgo.year.1,
           npgo.pre = npgo.pre,
           pdo.pre = pdo.pre)

#track all the parameters, including weights and coefficients 

parameters<-c('eps.S', 'eps.gam', 'sigma.S', 'sigma.gam', 'int.chla', 'sigma.chla','int.sst', 'sigma.sst', 
              'int.S', 'int.gam', 'mean.S', 'mean.gam', 'beta.S.npgo',
              'beta.gam.npgo','beta.S.pdo', 'beta.gam.pdo', 
              'beta.S.sst', 'beta.gam.sst','beta.S.chla', 'beta.gam.chla','w.S','w.gam', 'tau.total')

inits<-function() {list(int.S = runif(1), w.gam = c(1,1,1,1), w.S = c(1,1,1,1)) }

##############
### Output ###

out <- jagsUI::jags(data = data ,
                    inits = inits,
                    parameters.to.save = parameters,
                    model.file = "nest_surv_allvars.txt",
                    n.chains = 3,
                    #n.thin = 1, 
                    n.iter = 50000, #play with this
                    n.burnin = 5000, #play with this
                    n.adapt = 100)

# Extracting the summary jags data
out$summary

# Trace plots and density plots
mcmcs<- out$samples

MCMCtrace(mcmcs, type = 'both', ind = TRUE, pdf = FALSE)

# WAIC
waic_mod <- jags.samples(out$model,
                         c("WAIC", "deviance"),
                         type = "mean",
                         n.iter = 100000,
                         n.burnin = 10000,
                         n.thin = 1, parameters = parameters)

waic_mod$p_waic <- waic_mod$WAIC
waic_mod$waic <- waic_mod$deviance + waic_mod$p_waic
tmp <- sapply(waic_mod, sum)
waic.m0 <- round(c(waic = tmp[["waic"]], p_waic = tmp[["p_waic"]]),1)
print(waic.m0)

saveRDS(out, here("Results", "out.rds"))
