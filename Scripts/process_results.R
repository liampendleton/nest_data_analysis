#bind all ws together 
weights.out <- cbind(as.matrix(out$sims.list$w.S), as.matrix(out$sims.list$w.gam))

#paste the ws together to get model names 
models.out <- rep(NA,nrow(weights.out))
for(i in 1:nrow(weights.out)){
  models.out[i] <- paste(weights.out[i,1],weights.out[i,2],weights.out[i,3],weights.out[i,4],weights.out[i,5],weights.out[i,6],weights.out[i,7],weights.out[i,8],sep="")
}
#get frequency of each model 
table <- table(models.out)
model.names <- names(table)
freqs <- rep(NA,dim(table))
for(i in 1:dim(table)){
  freqs[i] <- table[i][[1]]
}

#create a new object 
msr <- data.frame(model.names,as.numeric(freqs))
colnames(msr) <- c("model","frequency")
msr <- msr[order(msr$frequency,decreasing = TRUE),]

#pull estimates of other parameters when top model is in place 
  

