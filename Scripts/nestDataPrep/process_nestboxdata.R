library(here)
library(dplyr)



#######################
### DATA FORMATTING ###
## Read in nesting data. Identify and address erroneous observations, then determine outcomes based on the amount of time chicks were known to be alive before disappearing. 

#Read in historic data
nestbox.init <- read.csv(here("Data", "Data_Unprocessed", "PI_Nest_Data_Full.csv"))
nestbox_data <- data.frame(nestbox.init$X,nestbox.init$DATE,nestbox.init$BOX_ID,nestbox.init$Number_Eggs,nestbox.init$Number_Live_Chicks) #isolate what we need
colnames(nestbox_data) <- c("Year","Date","Box_ID","Number_Eggs","Number_Chicks")

#Read in 2024 data
nestbox_24 <- read.csv(here("Data","Data_Unprocessed", "PI_Nest_Data_Full-24_NestBox.csv"))
nestbox_24 <- data.frame(nestbox_24$Year, nestbox_24$Date, nestbox_24$Box_ID, nestbox_24$Number_Eggs, nestbox_24$Number_Live_Chicks) #Pull relevant data
colnames(nestbox_24) <- c("Year","Date","Box_ID","Number_Eggs","Number_Chicks") #Standardize naming
nestbox_24$Date <- format(as.Date(nestbox_24$Date, format="%m/%d/%Y"), "%Y-%m-%d") #Match date format to historic data

#Append datasets
nestbox_data <- rbind(nestbox_data, nestbox_24) #Append 2024 data to historic data
nestbox_data$Date <- as.Date(nestbox_data$Date,format="%Y-%m-%d") #Convert class to date

#Create a nest record ID 
nestbox_data$Record <- paste(nestbox_data$Year,nestbox_data$Box_ID,sep="_")


#######################
### DATA VALIDATION ###
## Go trough a series of steps to identify issues in data.

#1) All the date entries for nests checked on 6-18-2010 are entered as 6-18-2020; fixing here 
nestbox_data$Date[which(nestbox_data$Date == "2020-06-18")] <- "2010-06-18"

#2) Identify cases where chicks are recorded without prior egg observation
chicks_no_eggs <- nestbox_data %>%
  group_by(Record) %>%
  arrange(Date) %>%
  mutate(chicks_no_eggs = any(Number_Chicks > 0 & all(Number_Eggs == 0))) %>% #Any time where chicks appear when there is no mention of eggs
  summarize(chicks_no_eggs = any(chicks_no_eggs, na.rm = TRUE)) %>%
  filter(chicks_no_eggs == TRUE)

    #2.1) Address the findings from above; remove from dataset since no fate can reasonably be determined
    nestbox_data <- nestbox_data[-c(which(nestbox_data$Record == "1996_60")),]
    nestbox_data <- nestbox_data[-c(which(nestbox_data$Record == "1996_65")),]  
    nestbox_data <- nestbox_data[-c(which(nestbox_data$Record == "1999_56")),]  
    nestbox_data <- nestbox_data[-c(which(nestbox_data$Record == "1999_78a")),] 
    nestbox_data <- nestbox_data[-c(which(nestbox_data$Record == "1999_57")),]#this nest has no eggs but then chicks appear, then disappear and then reappear!  

#3) Identify cases where two nests occur in the same box in a given year
two_nests <- nestbox_data %>%
  group_by(Record) %>%
  arrange(Date) %>%
  mutate(transition = ifelse(Number_Chicks > 0 & #If chicks appear in a box,
                             lead(Number_Chicks, default = 0) == 0 & #and the next observation reported no chicks,
                             lead(Number_Chicks, 2, default = 0) > 0, #and chicks appear again later,
                             TRUE, FALSE)) %>% #report TRUE, otherwise FALSE.
  summarize(two_nest_occurance = any(transition, na.rm = TRUE)) %>%
  filter(two_nest_occurance == TRUE)


#4) Where are there more than two eggs?
nestbox_data[which(nestbox_data$Number_Eggs >2),] #Three cases. Address below.

    #4.1) First error
    nestbox_data[(which(nestbox_data$Record == "2004_55")),] #Two entries of 3 eggs amid other entries of 2, so
    nestbox_data$Number_Eggs[which(nestbox_data$Number_Eggs == 3)] <- 2 #change 3 to 2.

    #4.2 Second error
    nestbox_data[(which(nestbox_data$Record == "2010_51")),] #One entry of 9 eggs amid other entries of 0, so
    nestbox_data$Number_Eggs[which(nestbox_data$Number_Eggs == 9)] <- 0 #change 9 to 0.

    #4.3) Run the same check code.
    nestbox_data[which(nestbox_data$Number_Eggs >2),] #Seems good!


#5) Where are there more than two chicks?
nestbox_data[which(nestbox_data$Number_Chicks >2),] #Nowhere!


## Now, filter out nest histories where no activity occurred.
nest_data <- nestbox_data %>%
  group_by(Record) %>%
  filter(max(Number_Eggs, na.rm = TRUE) + max(Number_Chicks, na.rm = TRUE) > 0) %>%
  ungroup()



###############################
### DETERMINE NEST OUTCOMES ###
## Use a loop that runs through each record to determine most probably outcome of a nest given how long chicks were known to be alive before disappearing (fledging)

#Track each record 
nest_IDs <- unique(nest_data$Record)

#Set temporal bounds on hatchling period - see Birds of the World: Period from Hatching to Departure
low.fledge <- 29 #Minimum days before fledging
high.fledge <- 54 #Maximum days before fledging

#Record nest outcomes and set up a warning for records that need another look 
outcome <- warning <- rep(NA,length(nest_IDs))
options(warn = 2) #Make warnings errors so loops will terminate 

#Loop through each nest record and determine outcome 
for(j in 1:length(nest_IDs)){
  #Per nest: all data, empty records, egg records, chick records, and records with both egg and chick  
  data <- nest_data[which(nest_data$Record == nest_IDs[j]),] #Obtain data for individual record
  empty <- data[which(data$Number_Eggs + data$Number_Chicks == 0), ] #Which records are completely empty?
  eggs <- data[which(data$Number_Eggs > 0),] #Which have eggs?
  chicks <- data[which(data$Number_Chicks > 0),] #Which have chicks?
  both <- data[intersect(which(data$Number_Chicks > 0),which(data$Number_Eggs > 0)),] #Which have both eggs and chicks?
  
  #If a nest had an egg and chick at the same time, note as a warning so it can be assessed later
  if(nrow(both > 0)){
    warning[j] <- 1 
  }
  
  #Were chicks ever seen? If not, outcome = 0 
  if(nrow(chicks) == 0){
    outcome[j] <- 0 
  }else{
    
    #Record/censor nest if chicks were still present at final observation    
    if(max(data$Date) == max(chicks$Date)){
      outcome[j] <- NA
    }else{ 
      
      #When were eggs last observed?
      last.eggs <- max((eggs$Date))
      
      #When were chicks last observed?         
      last.chicks <- max(chicks$Date) 
      
      #When was the nest first empty after chicks? 
      first.appfledge <- empty$Date[min(which(empty$Date > last.chicks))]
      
      #What was the minimum age of chicks when the nest was first observed empty?  
      min.age <- first.appfledge - last.eggs
      
      #Determine fledging outcome based on comparison to min.age
      if(min.age > low.fledge){
        outcome[j] <- 1*chicks$Number_Chicks[which(chicks$Date == max(chicks$Date))] 
      }else{
        outcome[j] <- 0 
      }
      
      #Did chicks reach an unusually high age? If so, worth another look? 
      max.age <- first.appfledge - last.eggs 
      
      if(max.age > high.fledge){
        warning[j] <- 1 
      } 
    }
  }
}

#Create output data set 
year <- substring(nest_IDs,1,4)
output <- data.frame(year,nest_IDs,outcome)

#Omit nests where chicks were present through final observation, thus no conclusion could be drawn 
output <- output[-c(which(is.na(output$outcome == TRUE))), ] #Omit nests where chicks were present through final observation; no conclusion could be drawn

#Summarize outcomes per year
summary <- output %>%
  group_by(year) %>%
  summarise(No_chicks = sum(outcome == 0),
            One_chick = sum(outcome == 1),
            Two_chicks = sum(outcome == 2))

#Determine number of chicks fledged per nest
fledged <- sum(summary[,3]) + (2*(sum(summary[,4])))

# Record data
write.csv(output,file = here("Data","Data_Processed", "model_input.csv"),row.names = FALSE)
