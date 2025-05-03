library(here)
library(dplyr)

### DATA FORMATTING
#make warnings errors so loops will terminate 
options(warn = 2)  

#hatchling period - see Birds of the World: Period from Hatching to Departure
#total range 
low.fledge <- 29
high.fledge <- 54

nestbox.init <- read.csv(here("Data","PI_Nest_Data_Full_with2024.csv"))
nestbox_data <- data.frame(nestbox.init$YEAR,nestbox.init$DATE,nestbox.init$BOX_ID,nestbox.init$Number_Eggs,nestbox.init$Number_Live_Chicks)
colnames(nestbox_data) <- c("Year","Date","Box_ID","Number_Eggs","Number_Chicks")
nestbox_data$Date <- as.Date(nestbox_data$Date,format="%m/%d/%Y")

#all the date entries for nests checked on 6-18-2010 are entered as 6-18-2020; fixing here 
nestbox_data$Date[which(nestbox_data$Date == "2020-06-18")] <- "2010-06-18"

#create a nest record ID 
nestbox_data$Record <- paste(nestbox_data$Year,nestbox_data$Box_ID,sep="_")

### DATA VALIDATION
#Identify chicks that appear without prior record of eggs
chicks_no_eggs <- nestbox_data %>%
  group_by(Record) %>%
  arrange(Date) %>%
  mutate(chicks_no_eggs = any(Number_Chicks > 0 & all(Number_Eggs == 0))) %>%
  summarize(chicks_no_eggs = any(chicks_no_eggs, na.rm = TRUE)) %>%
  filter(chicks_no_eggs == TRUE)

#Anomalous nests that should be removed
#these have already been removed in this dataset 
#nestbox_data <- nestbox_data[-c(which(nestbox_data$Record == "1999_78a")),]#this nest has no eggs but then chicks appear 
#nestbox_data <- nestbox_data[-c(which(nestbox_data$Record == "1999_56")),]#this nest has no eggs but then chicks appear  
#nestbox_data <- nestbox_data[-c(which(nestbox_data$Record == "1996_65")),]#this nest has no eggs but then chicks appear  
#nestbox_data <- nestbox_data[-c(which(nestbox_data$Record == "1996_60")),]#this nest has no eggs but then chicks appear  
#nestbox_data <- nestbox_data[-c(which(nestbox_data$Record == "1999_57")),]#this nest has no eggs but then chicks appear, then disappear and then reappear!  

#SJC: CHECKED ABOVE IN ORIGINAL DATA  
#SJC: 1999_78a I AGREE THIS IS A PROBLEM 
#SJC: 1999_57 I AGREE THIS IS A PROBLEM  
#SJC: 1999_56 I AGREE THIS IS A PROBLEM  
#SJC: 1999_65 THIS NEST IS REPEATED IN LIST ABOVE. WHY? IN ORIGINAL DATA, THIS NEST HAD EGGS ON 14 JUNE #LP: I agree; removed 
#SJC: 1999_60 IN ORIGINAL DATA, THIS NEST HAD EGGS ON 9 JULY #LP: I think you mean 96_60? There was no 99_60 listed here

#SJC: WHAT ARE YOU DOING ABOUT THIS? WHAT STEPS ARE YOU TAKING WITH WHAT YOU FIND HERE? PLEASE COMMENT CODE 
#Check to see if two nests ever occur in the same box in a year
two_nests <- nestbox_data %>%
  group_by(Record) %>%
  arrange(Date) %>%
  mutate(transition = ifelse(Number_Chicks > 0 & #if there are ever chicks
        lead(Number_Chicks, default = 0) == 0 & #and the next row has no chicks
        lead(Number_Chicks, 2, default = 0) > 0, #and then any row after that has chicks again
      TRUE, FALSE)) %>% #report TRUE, otherwise FALSE
  summarize(two_nest_occurance = any(transition, na.rm = TRUE)) %>%
  filter(two_nest_occurance == TRUE)

#unique nest record IDs 
nests <- unique(nestbox_data$Record)

#find all IDs where an egg or chick was never observed 
indicator <- rep(NA,length(nests))

for(i in 1:length(nests)){
  data <- nestbox_data[which(nestbox_data$Record == nests[i]),]
  indicator[i] <- max(data$Number_Chicks) + max(data$Number_Eggs) #add the max counts of eggs and chicks 
}

#note that one of the indicators gives us a max of 10
#this seems to be a simple data entry error - correct here  
table(indicator)
nestbox_data[which(nestbox_data$Record == nests[which(indicator == 10)]),] #Shows an entry of 9  eggs
#SJC: THIS IS NOT FIXING THE ERROR - IT IS THE INDICATOR THAT EQUALS 10, NOT THE NUMBER OF EGGS - CHECK TO MAKE SURE YOUR FIX WORKS #LP: The number of eggs = 9. I address that below.
nestbox_data[which(nestbox_data$Number_Eggs == 9),] #show me the row where 9 eggs are reported
egg_9 <- which(nestbox_data$Number_Eggs == 9) #give me the index where this occurs; record row number
nestbox_data$Number_Eggs[which(nestbox_data$Number_Eggs == 9)] <- 0
nestbox_data[egg_9,] #check that row to make sure it worked
nestbox_data[nestbox_data$Record == "2010_51",] #check records to make sure everything looks okay



#delete those nests where the indicator is 0, where there were never eggs or chicks; no point in monitoring these nests 
nest_data <- nestbox_data[-c(which(is.element(nestbox_data$Record,nests[which(indicator==0)] ))),]

#create the pared down nests list 
nests2 <- unique(nest_data$Record)

#set up to record outcome and also set up a warning for records that need another look 
outcome <- warning <- rep(NA,length(nests2))

#loop through nests to get outcome 
for(j in 1:length(nests2)){
  #by nest: all data, empty records, egg records, chick records, and records with both egg and chick  
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
  
    #prepare to censor the nest if chicks were still in it at the last observation    
#SJC: WHAT ABOUT CENSORING THE NEST IF CHICKS WERE IN IT AT THE FIRST OBSERVATION, WHICH IS THE CASE WITH SOME OF THE NESTS ON LINES 30 - 35? #LP: None of the nests in those lines feature chicks at first observation, unless I'm missing something?
#SJC: WHY IS THAT FIX MADE THERE INSTEAD OF HERE?     #LP: I feel tempted to leave things as is instead of merging things if they already work fine. Are you okay with this?
    if(max(data$Date) == max(chicks$Date)){
      outcome[j] <- NA
    }else{ 
      
      #when were eggs last observed? 
      last.eggs <- max((eggs$Date))
    
      #when were chicks last observed?         
      last.chicks <- max(chicks$Date) 
    
      #when was the nest first empty after chicks? 
      first.appfledge <- empty$Date[min(which(empty$Date > last.chicks))]
    
      #what was the minimum age of fledglings when the nest was first observed empty  
      min.age <- first.appfledge - last.eggs
     
      #set outcome based on comparison to min.age 
      if(min.age > low.fledge){
        outcome[j] <- 1*chicks$Number_Chicks[which(chicks$Date == max(chicks$Date))] 
      }else{
        outcome[j] <- 0 
      }
    
      #did the nest reach an unusually high age, so worth another look? 
      max.age <- first.appfledge - last.eggs 
    
      if(max.age > high.fledge){
        warning[j] <- 1 
      } 
    }
  }
}

# Create the output data set 
year <- substring(nests2,1,4)
output <- data.frame(year,nests2,outcome)
output.withNAs <- output

year_table <- table(output.withNAs$year)
nests_monitored <- as.vector(year_table)

### DELETE the nests that are being censored - nests censored if final observation featured chick and no conclusion can be drawn 
no_finale <- output[c(which(is.na(output$outcome == TRUE))),] #These are nests where an outcome could not be determined because birds were in box up through the final observation
output <- output[-c(which(is.na(output$outcome == TRUE))), ]
output.withNAs <- output


# Summarize outcomes per year
summary <- output %>%
  group_by(year) %>%
  summarise(No_chicks = sum(outcome == 0),
            One_chick = sum(outcome == 1),
            Two_chicks = sum(outcome == 2))

print(summary)

#NUMBER OF CHICKS FLEDGED
fledged <- sum(summary[,3]) + (2*(sum(summary[,4])))

# Record data
write.csv(output,file = here("Data","model_input.csv"),row.names = FALSE)
