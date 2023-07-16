library(here)

#nestling period 
#29 to 54? need to go to get the primary sources (see Birds of the World: Period from Hatching to Departure)

#total range 
low.fledge <- 29
high.fledge <- 54

#read in full dataset
nestbox.init <- read.csv(here("Data","PI_Nest_Data_Full.csv"))

#keep selected columns
nestbox_data <- data.frame(nestbox.init$YEAR,nestbox.init$DATE,nestbox.init$BOX_ID,nestbox.init$Number_Eggs,nestbox.init$Number_Live_Chicks)
colnames(nestbox_data) <- c("Year","Date","Box_ID","Number_Eggs","Number_Chicks")
nestbox_data$Date <- as.Date(nestbox_data$Date,format="%Y-%m-%d")

#create a nest record ID 
nestbox_data$Record <- paste(nestbox_data$Year,nestbox_data$Box_ID,sep="_")

#delete 2023 for now 
nestbox_data <- nestbox_data[-c(which(nestbox_data$Year == 2023)),]

#make NAs = 0 
nestbox_data$Number_Eggs[which(is.na(nestbox_data$Number_Eggs == TRUE))] <- 0 
nestbox_data$Number_Chicks[which(is.na(nestbox_data$Number_Chicks == TRUE))] <- 0 

#unique nest record IDs 
nests <- unique(nestbox_data$Record)

#find all IDs where an egg or chick was never observed 
indicator <- rep(NA,length(nests))

for(i in 1:length(nests)){
  data <- nestbox_data[which(nestbox_data$Record == nests[i]),]
  #add the max counts of eggs and chicks 
  indicator[i] <- max(data$Number_Chicks) + max(data$Number_Eggs)
}

#note that one of the indicators gives us a max of 10
#this seems to be a simple data entry error - correct here  
table(indicator)
nestbox_data[which(nestbox_data$Record == nests[which(indicator == 10)]),]
nestbox_data$Number_Eggs[which(nestbox_data$Number_Eggs == 10)] <- 0 

#delete those nests where the indicator is 0, where there were never eggs or chicks 
nest_data <- nestbox_data[-c(which(is.element(nestbox_data$Record,nests[which(indicator==0)] ))),]

#create the pared down nests list 
nests2 <- unique(nest_data$Record)

#set up to record outcome and also set up a warning for suspicious records 
outcome <- warning <- rep(NA,length(nests2))

#go through nests to get outcome 
for(j in 1:length(nests2)){
  data <- nest_data[which(nest_data$Record == nests2[j]),]
  empty <- data[which(data$Number_Eggs + data$Number_Chicks == 0), ] 
  eggs <- data[which(data$Number_Eggs > 0),]
  chicks <- data[which(data$Number_Chicks > 0),]
  both <- data[intersect(which(data$Number_Chicks > 0),which(data$Number_Eggs > 0)),]

  #if there was an egg and chick at the same time, note as a warning, to be sure it is handled correctly
  if(nrow(both > 0)){
    warning[j] <- 1 
  }
  
  #were chicks ever seen? if no, outcome = 0 
  if(nrow(chicks) == 0){
    outcome[j] <- 0 
  }else{
  
    #censor the nest if it was never observed after chicks were gone   
    if(max(data$Date) == max(chicks$Date)){
      outcome[j] <- NA
    }else{ 
      
      #when were eggs last observed? 
      last.eggs <- max((eggs$Date))
    
      #when were chicks last observed?         
      last.chicks <- max(chicks$Date) 
    
      #when was the nest first empty after chicks? 
      first.appfledge <- empty$Date[min(which(empty$Date > last.chicks))]
    
      #did the nest reach minimum age? 
      min.age <- first.appfledge - last.eggs
     
      if(min.age > low.fledge){
        outcome[j] <- 1*chicks$Number_Chicks[which(chicks$Date == max(chicks$Date))] 
      }else{
        outcome[j] <- 0 
      }
    
      #did the nest reach an unusually high age, so worth another look? 
      max.age <- first.appfledge - last.empty 
    
      if(max.age > high.range){
        warning[j] <- 1 
      } 
    }
  }
}

#create the output data set 
year <- substring(nests2,1,4)
output <- data.frame(year,nests2,outcome)

#delete the nests that are being censored - do this once all done and we feel good about the data  
#output <- output[-c(which(is.na(output$outcome == TRUE))), ]

