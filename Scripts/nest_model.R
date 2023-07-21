library(here)
library(rjags)


#read in data 
nests <- read.csv(here("Data","model_input.csv"))


cat("
model {

for(i in 1:n.nests){

  y[i] ~ dcat(omega[i,1:3]) #a categorical distribution is a multinomial distribution with an index of 1 (i.e., it is like rolling a die once and modeling the probabilities of getting a 1, 2, 3, 4, 5, or 6) 

  omega[i,1] <- (1-S[i])     #failure 
  omega[i,2] <- S[i]*(1-gamma[i])   #1 egg
  omega[i,3] <- S[i]*gamma[i]   #2 eggs 

  logit(S[i]) <- int.S + eps.S[year[i]]
  logit(gamma[i]) <- int.gam + eps.gam[year[i]]
} 

for(y in 1:n.years){  #random effects to explain residual annual variation 
  eps.S[y] ~ dnorm(0,tau.S) 
  eps.gam[y] ~ dnorm(0,tau.gam)
} 

tau.S <- pow(sigma.S,-2)
sigma.S ~ dunif(0,10) 
tau.gam <- pow(sigma.gam,-2)
sigma.gam ~ dunif(0,10) 

int.S ~ dnorm(0,1) 
int.gam ~ dnorm(0,1) 

mean.S <- 1/(1+exp(-(int.S)))

}
",file = "nest_surv.txt")


nests <- nests[-c(which(is.na(nests$outcome)==TRUE)),]
nests$outcome <- nests$outcome + 1 
nests$year <- nests$year - 1995 
nests$year <- as.numeric(as.factor(nests$year))


data<-list(y = nests$outcome, year = nests$year, n.nests = dim(nests)[1], n.years = length(unique(nests$year)))

parameters<-c('int.S','mean.S')

inits<-function() {list(int.S = runif(1)) }

##This version takes about 3-5 minutes to run, but ultimately needs to be run longer
mod.out <- jags.model("nest_surv.txt", data, inits, n.chains=3, n.adapt=100)
out <- coda.samples(mod.out, parameters, n.iter=1000)

