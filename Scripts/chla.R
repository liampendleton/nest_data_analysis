library(here)
library(tidyverse)
library(dplyr)
library(readr)

#####
#ChlA
chla1 <- read_csv(here("Data", "erdMH1chlamday_ff42_85c8_cd0c.csv"), na = c("NaN", "NA"))
chla2 <- read_csv(here("Data", "erdSW2018chlamday_29f3_6396_25da.csv"), na = c("NaN", "NA"))

chla1 <- chla1[-1,] #get rid of extra headers
chla2 <- chla2[-1,]

rownames(chla1) = seq(length=nrow(chla1)) #reset row numbers
rownames(chla2) = seq(length=nrow(chla2))

#1997
chla_1997_09 <- chla2 %>% filter(time == "1997-09-16T00:00:00Z" & !is.na(chlorophyll)) #Isolate data from specific year and month
chla_1997_09 <- chla_1997_09$chlorophyll #Isolate chlorophyll data
chla_1997_09 <- as.numeric(chla_1997_09) #Convert to numeric
chla_1997_09 <- mean(chla_1997_09) #Get monthly average

chla_1997_10 <- chla2 %>% filter(time == "1997-10-16T00:00:00Z" & !is.na(chlorophyll))
chla_1997_10 <- chla_1997_10$chlorophyll
chla_1997_10 <- as.numeric(chla_1997_10)
chla_1997_10 <- mean(chla_1997_10)

chla_1997_11 <- chla2 %>% filter(time == "1997-11-16T00:00:00Z" & !is.na(chlorophyll))
chla_1997_11 <- chla_1997_11$chlorophyll
chla_1997_11 <- as.numeric(chla_1997_11)
chla_1997_11 <- mean(chla_1997_11)

chla_1997 <- c(rep(NA, 8), chla_1997_09, chla_1997_10, chla_1997_11, NA) #Compile data; fill in leading empty slots
chla_1997 <- as.numeric(chla_1997)

#1998
chla_1998_01 <- chla2 %>% filter(time == "1998-01-16T00:00:00Z" & !is.na(chlorophyll))
chla_1998_01 <- chla_1998_01$chlorophyll
chla_1998_01 <- as.numeric(chla_1998_01)
chla_1998_01 <- mean(chla_1998_01)

chla_1998_02 <- chla2 %>% filter(time == "1998-02-16T00:00:00Z" & !is.na(chlorophyll))
chla_1998_02 <- chla_1998_02$chlorophyll
chla_1998_02 <- as.numeric(chla_1998_02)
chla_1998_02 <- mean(chla_1998_02)

chla_1998_03 <- chla2 %>% filter(time == "1998-03-16T00:00:00Z" & !is.na(chlorophyll))
chla_1998_03 <- chla_1998_03$chlorophyll
chla_1998_03 <- as.numeric(chla_1998_03)
chla_1998_03 <- mean(chla_1998_03)

chla_1998_04 <- chla2 %>% filter(time == "1998-04-16T00:00:00Z" & !is.na(chlorophyll))
chla_1998_04 <- chla_1998_04$chlorophyll
chla_1998_04 <- as.numeric(chla_1998_04)
chla_1998_04 <- mean(chla_1998_04)

chla_1998_05 <- chla2 %>% filter(time == "1998-05-16T00:00:00Z" & !is.na(chlorophyll))
chla_1998_05 <- chla_1998_05$chlorophyll
chla_1998_05 <- as.numeric(chla_1998_05)
chla_1998_05 <- mean(chla_1998_05)

chla_1998_06 <- chla2 %>% filter(time == "1998-06-16T00:00:00Z" & !is.na(chlorophyll))
chla_1998_06 <- chla_1998_06$chlorophyll
chla_1998_06 <- as.numeric(chla_1998_06)
chla_1998_06 <- mean(chla_1998_06)

chla_1998_07 <- chla2 %>% filter(time == "1998-07-16T00:00:00Z" & !is.na(chlorophyll))
chla_1998_07 <- chla_1998_07$chlorophyll
chla_1998_07 <- as.numeric(chla_1998_07)
chla_1998_07 <- mean(chla_1998_07)

chla_1998_08 <- chla2 %>% filter(time == "1998-08-16T00:00:00Z" & !is.na(chlorophyll))
chla_1998_08 <- chla_1998_08$chlorophyll
chla_1998_08 <- as.numeric(chla_1998_08)
chla_1998_08 <- mean(chla_1998_08)

chla_1998_09 <- chla2 %>% filter(time == "1998-09-16T00:00:00Z" & !is.na(chlorophyll))
chla_1998_09 <- chla_1998_09$chlorophyll
chla_1998_09 <- as.numeric(chla_1998_09)
chla_1998_09 <- mean(chla_1998_09)

chla_1998_10 <- chla2 %>% filter(time == "1998-10-16T00:00:00Z" & !is.na(chlorophyll))
chla_1998_10 <- chla_1998_10$chlorophyll
chla_1998_10 <- as.numeric(chla_1998_10)
chla_1998_10 <- mean(chla_1998_10)

chla_1998_11 <- chla2 %>% filter(time == "1998-11-16T00:00:00Z" & !is.na(chlorophyll))
chla_1998_11 <- chla_1998_11$chlorophyll
chla_1998_11 <- as.numeric(chla_1998_11)
chla_1998_11 <- mean(chla_1998_11)

chla_1998 <- c(chla_1998_01, chla_1998_02, chla_1998_03, chla_1998_04, chla_1998_05, chla_1998_06, chla_1998_07, chla_1998_08, chla_1998_09, chla_1998_10, chla_1998_11, NA)
chla_1998 <- as.numeric(chla_1998)

#1999
chla_1999_01 <- chla2 %>% filter(time == "1999-01-16T00:00:00Z" & !is.na(chlorophyll))
chla_1999_01 <- chla_1999_01$chlorophyll
chla_1999_01 <- as.numeric(chla_1999_01)
chla_1999_01 <- mean(chla_1999_01)

chla_1999_02 <- chla2 %>% filter(time == "1999-02-16T00:00:00Z" & !is.na(chlorophyll))
chla_1999_02 <- chla_1999_02$chlorophyll
chla_1999_02 <- as.numeric(chla_1999_02)
chla_1999_02 <- mean(chla_1999_02)

chla_1999_03 <- chla2 %>% filter(time == "1999-03-16T00:00:00Z" & !is.na(chlorophyll))
chla_1999_03 <- chla_1999_03$chlorophyll
chla_1999_03 <- as.numeric(chla_1999_03)
chla_1999_03 <- mean(chla_1999_03)

chla_1999_04 <- chla2 %>% filter(time == "1999-04-16T00:00:00Z" & !is.na(chlorophyll))
chla_1999_04 <- chla_1999_04$chlorophyll
chla_1999_04 <- as.numeric(chla_1999_04)
chla_1999_04 <- mean(chla_1999_04)

chla_1999_05 <- chla2 %>% filter(time == "1999-05-16T00:00:00Z" & !is.na(chlorophyll))
chla_1999_05 <- chla_1999_05$chlorophyll
chla_1999_05 <- as.numeric(chla_1999_05)
chla_1999_05 <- mean(chla_1999_05)

chla_1999_06 <- chla2 %>% filter(time == "1999-06-16T00:00:00Z" & !is.na(chlorophyll))
chla_1999_06 <- chla_1999_06$chlorophyll
chla_1999_06 <- as.numeric(chla_1999_06)
chla_1999_06 <- mean(chla_1999_06)

chla_1999_07 <- chla2 %>% filter(time == "1999-07-16T00:00:00Z" & !is.na(chlorophyll))
chla_1999_07 <- chla_1999_07$chlorophyll
chla_1999_07 <- as.numeric(chla_1999_07)
chla_1999_07 <- mean(chla_1999_07)

chla_1999_08 <- chla2 %>% filter(time == "1999-08-16T00:00:00Z" & !is.na(chlorophyll))
chla_1999_08 <- chla_1999_08$chlorophyll
chla_1999_08 <- as.numeric(chla_1999_08)
chla_1999_08 <- mean(chla_1999_08)

chla_1999_09 <- chla2 %>% filter(time == "1999-09-16T00:00:00Z" & !is.na(chlorophyll))
chla_1999_09 <- chla_1999_09$chlorophyll
chla_1999_09 <- as.numeric(chla_1999_09)
chla_1999_09 <- mean(chla_1999_09)

chla_1999_10 <- chla2 %>% filter(time == "1999-10-16T00:00:00Z" & !is.na(chlorophyll))
chla_1999_10 <- chla_1999_10$chlorophyll
chla_1999_10 <- as.numeric(chla_1999_10)
chla_1999_10 <- mean(chla_1999_10)

chla_1999_11 <- chla2 %>% filter(time == "1999-11-16T00:00:00Z" & !is.na(chlorophyll))
chla_1999_11 <- chla_1999_11$chlorophyll
chla_1999_11 <- as.numeric(chla_1999_11)
chla_1999_11 <- mean(chla_1999_11)

chla_1999 <- c(chla_1999_01, chla_1999_02, chla_1999_03, chla_1999_04, chla_1999_05, chla_1999_06, chla_1999_07, chla_1999_08, chla_1999_09, chla_1999_10, chla_1999_11, NA)
chla_1999 <- as.numeric(chla_1999)

#2000
chla_2000_01 <- chla2 %>% filter(time == "2000-01-16T00:00:00Z" & !is.na(chlorophyll))
chla_2000_01 <- chla_2000_01$chlorophyll
chla_2000_01 <- as.numeric(chla_2000_01)
chla_2000_01 <- mean(chla_2000_01)

chla_2000_02 <- chla2 %>% filter(time == "2000-02-16T00:00:00Z" & !is.na(chlorophyll))
chla_2000_02 <- chla_2000_02$chlorophyll
chla_2000_02 <- as.numeric(chla_2000_02)
chla_2000_02 <- mean(chla_2000_02)

chla_2000_03 <- chla2 %>% filter(time == "2000-03-16T00:00:00Z" & !is.na(chlorophyll))
chla_2000_03 <- chla_2000_03$chlorophyll
chla_2000_03 <- as.numeric(chla_2000_03)
chla_2000_03 <- mean(chla_2000_03)

chla_2000_04 <- chla2 %>% filter(time == "2000-04-16T00:00:00Z" & !is.na(chlorophyll))
chla_2000_04 <- chla_2000_04$chlorophyll
chla_2000_04 <- as.numeric(chla_2000_04)
chla_2000_04 <- mean(chla_2000_04)

chla_2000_05 <- chla2 %>% filter(time == "2000-05-16T00:00:00Z" & !is.na(chlorophyll))
chla_2000_05 <- chla_2000_05$chlorophyll
chla_2000_05 <- as.numeric(chla_2000_05)
chla_2000_05 <- mean(chla_2000_05)

chla_2000_06 <- chla2 %>% filter(time == "2000-06-16T00:00:00Z" & !is.na(chlorophyll))
chla_2000_06 <- chla_2000_06$chlorophyll
chla_2000_06 <- as.numeric(chla_2000_06)
chla_2000_06 <- mean(chla_2000_06)

chla_2000_07 <- chla2 %>% filter(time == "2000-07-16T00:00:00Z" & !is.na(chlorophyll))
chla_2000_07 <- chla_2000_07$chlorophyll
chla_2000_07 <- as.numeric(chla_2000_07)
chla_2000_07 <- mean(chla_2000_07)

chla_2000_08 <- chla2 %>% filter(time == "2000-08-16T00:00:00Z" & !is.na(chlorophyll))
chla_2000_08 <- chla_2000_08$chlorophyll
chla_2000_08 <- as.numeric(chla_2000_08)
chla_2000_08 <- mean(chla_2000_08)

chla_2000_09 <- chla2 %>% filter(time == "2000-09-16T00:00:00Z" & !is.na(chlorophyll))
chla_2000_09 <- chla_2000_09$chlorophyll
chla_2000_09 <- as.numeric(chla_2000_09)
chla_2000_09 <- mean(chla_2000_09)

chla_2000_10 <- chla2 %>% filter(time == "2000-10-16T00:00:00Z" & !is.na(chlorophyll))
chla_2000_10 <- chla_2000_10$chlorophyll
chla_2000_10 <- as.numeric(chla_2000_10)
chla_2000_10 <- mean(chla_2000_10)

chla_2000_11 <- chla2 %>% filter(time == "2000-11-16T00:00:00Z" & !is.na(chlorophyll))
chla_2000_11 <- chla_2000_11$chlorophyll
chla_2000_11 <- as.numeric(chla_2000_11)
chla_2000_11 <- mean(chla_2000_11)

chla_2000 <- c(chla_2000_01, chla_2000_02, chla_2000_03, chla_2000_04, chla_2000_05, chla_2000_06, chla_2000_07, chla_2000_08, chla_2000_09, chla_2000_10, chla_2000_11, NA)
chla_2000 <- as.numeric(chla_2000)

#2001
chla_2001_01 <- chla2 %>% filter(time == "2001-01-16T00:00:00Z" & !is.na(chlorophyll))
chla_2001_01 <- chla_2001_01$chlorophyll
chla_2001_01 <- as.numeric(chla_2001_01)
chla_2001_01 <- mean(chla_2001_01)

chla_2001_02 <- chla2 %>% filter(time == "2001-02-16T00:00:00Z" & !is.na(chlorophyll))
chla_2001_02 <- chla_2001_02$chlorophyll
chla_2001_02 <- as.numeric(chla_2001_02)
chla_2001_02 <- mean(chla_2001_02)

chla_2001_03 <- chla2 %>% filter(time == "2001-03-16T00:00:00Z" & !is.na(chlorophyll))
chla_2001_03 <- chla_2001_03$chlorophyll
chla_2001_03 <- as.numeric(chla_2001_03)
chla_2001_03 <- mean(chla_2001_03)

chla_2001_04 <- chla2 %>% filter(time == "2001-04-16T00:00:00Z" & !is.na(chlorophyll))
chla_2001_04 <- chla_2001_04$chlorophyll
chla_2001_04 <- as.numeric(chla_2001_04)
chla_2001_04 <- mean(chla_2001_04)

chla_2001_05 <- chla2 %>% filter(time == "2001-05-16T00:00:00Z" & !is.na(chlorophyll))
chla_2001_05 <- chla_2001_05$chlorophyll
chla_2001_05 <- as.numeric(chla_2001_05)
chla_2001_05 <- mean(chla_2001_05)

chla_2001_06 <- chla2 %>% filter(time == "2001-06-16T00:00:00Z" & !is.na(chlorophyll))
chla_2001_06 <- chla_2001_06$chlorophyll
chla_2001_06 <- as.numeric(chla_2001_06)
chla_2001_06 <- mean(chla_2001_06)

chla_2001_07 <- chla2 %>% filter(time == "2001-07-16T00:00:00Z" & !is.na(chlorophyll))
chla_2001_07 <- chla_2001_07$chlorophyll
chla_2001_07 <- as.numeric(chla_2001_07)
chla_2001_07 <- mean(chla_2001_07)

chla_2001_08 <- chla2 %>% filter(time == "2001-08-16T00:00:00Z" & !is.na(chlorophyll))
chla_2001_08 <- chla_2001_08$chlorophyll
chla_2001_08 <- as.numeric(chla_2001_08)
chla_2001_08 <- mean(chla_2001_08)

chla_2001_09 <- chla2 %>% filter(time == "2001-09-16T00:00:00Z" & !is.na(chlorophyll))
chla_2001_09 <- chla_2001_09$chlorophyll
chla_2001_09 <- as.numeric(chla_2001_09)
chla_2001_09 <- mean(chla_2001_09)

chla_2001_10 <- chla2 %>% filter(time == "2001-10-16T00:00:00Z" & !is.na(chlorophyll))
chla_2001_10 <- chla_2001_10$chlorophyll
chla_2001_10 <- as.numeric(chla_2001_10)
chla_2001_10 <- mean(chla_2001_10)

chla_2001_11 <- chla2 %>% filter(time == "2001-11-16T00:00:00Z" & !is.na(chlorophyll))
chla_2001_11 <- chla_2001_11$chlorophyll
chla_2001_11 <- as.numeric(chla_2001_11)
chla_2001_11 <- mean(chla_2001_11)

chla_2001 <- c(chla_2001_01, chla_2001_02, chla_2001_03, chla_2001_04, chla_2001_05, chla_2001_06, chla_2001_07, chla_2001_08, chla_2001_09, chla_2001_10, chla_2001_11, NA)
chla_2001 <- as.numeric(chla_2001)

#2002
chla_2002_01 <- chla2 %>% filter(time == "2002-01-16T00:00:00Z" & !is.na(chlorophyll))
chla_2002_01 <- chla_2002_01$chlorophyll
chla_2002_01 <- as.numeric(chla_2002_01)
chla_2002_01 <- mean(chla_2002_01)

chla_2002_02 <- chla2 %>% filter(time == "2002-02-16T00:00:00Z" & !is.na(chlorophyll))
chla_2002_02 <- chla_2002_02$chlorophyll
chla_2002_02 <- as.numeric(chla_2002_02)
chla_2002_02 <- mean(chla_2002_02)

chla_2002_03 <- chla2 %>% filter(time == "2002-03-16T00:00:00Z" & !is.na(chlorophyll))
chla_2002_03 <- chla_2002_03$chlorophyll
chla_2002_03 <- as.numeric(chla_2002_03)
chla_2002_03 <- mean(chla_2002_03)

chla_2002_04 <- chla2 %>% filter(time == "2002-04-16T00:00:00Z" & !is.na(chlorophyll))
chla_2002_04 <- chla_2002_04$chlorophyll
chla_2002_04 <- as.numeric(chla_2002_04)
chla_2002_04 <- mean(chla_2002_04)

chla_2002_05 <- chla2 %>% filter(time == "2002-05-16T00:00:00Z" & !is.na(chlorophyll))
chla_2002_05 <- chla_2002_05$chlorophyll
chla_2002_05 <- as.numeric(chla_2002_05)
chla_2002_05 <- mean(chla_2002_05)

chla_2002_06 <- chla2 %>% filter(time == "2002-06-16T00:00:00Z" & !is.na(chlorophyll))
chla_2002_06 <- chla_2002_06$chlorophyll
chla_2002_06 <- as.numeric(chla_2002_06)
chla_2002_06 <- mean(chla_2002_06)

chla_2002_07 <- chla2 %>% filter(time == "2002-07-16T00:00:00Z" & !is.na(chlorophyll))
chla_2002_07 <- chla_2002_07$chlorophyll
chla_2002_07 <- as.numeric(chla_2002_07)
chla_2002_07 <- mean(chla_2002_07)

chla_2002_08 <- chla2 %>% filter(time == "2002-08-16T00:00:00Z" & !is.na(chlorophyll))
chla_2002_08 <- chla_2002_08$chlorophyll
chla_2002_08 <- as.numeric(chla_2002_08)
chla_2002_08 <- mean(chla_2002_08)

chla_2002_09 <- chla2 %>% filter(time == "2002-09-16T00:00:00Z" & !is.na(chlorophyll))
chla_2002_09 <- chla_2002_09$chlorophyll
chla_2002_09 <- as.numeric(chla_2002_09)
chla_2002_09 <- mean(chla_2002_09)

chla_2002_10 <- chla2 %>% filter(time == "2002-10-16T00:00:00Z" & !is.na(chlorophyll))
chla_2002_10 <- chla_2002_10$chlorophyll
chla_2002_10 <- as.numeric(chla_2002_10)
chla_2002_10 <- mean(chla_2002_10)

chla_2002_11 <- chla2 %>% filter(time == "2002-11-16T00:00:00Z" & !is.na(chlorophyll))
chla_2002_11 <- chla_2002_11$chlorophyll
chla_2002_11 <- as.numeric(chla_2002_11)
chla_2002_11 <- mean(chla_2002_11)

chla_2002 <- c(chla_2002_01, chla_2002_02, chla_2002_03, chla_2002_04, chla_2002_05, chla_2002_06, chla_2002_07, chla_2002_08, chla_2002_09, chla_2002_10, chla_2002_11, NA)
chla_2002 <- as.numeric(chla_2002)

#2003
chla_2003_01 <- chla2 %>% filter(time == "2003-01-16T00:00:00Z" & !is.na(chlorophyll))
chla_2003_01 <- chla_2003_01$chlorophyll
chla_2003_01 <- as.numeric(chla_2003_01)
chla_2003_01 <- mean(chla_2003_01)

chla_2003_02 <- chla2 %>% filter(time == "2003-02-16T00:00:00Z" & !is.na(chlorophyll))
chla_2003_02 <- chla_2003_02$chlorophyll
chla_2003_02 <- as.numeric(chla_2003_02)
chla_2003_02 <- mean(chla_2003_02)

chla_2003_03 <- chla2 %>% filter(time == "2003-03-16T00:00:00Z" & !is.na(chlorophyll))
chla_2003_03 <- chla_2003_03$chlorophyll
chla_2003_03 <- as.numeric(chla_2003_03)
chla_2003_03 <- mean(chla_2003_03)

chla_2003_04 <- chla2 %>% filter(time == "2003-04-16T00:00:00Z" & !is.na(chlorophyll))
chla_2003_04 <- chla_2003_04$chlorophyll
chla_2003_04 <- as.numeric(chla_2003_04)
chla_2003_04 <- mean(chla_2003_04)

chla_2003_05 <- chla2 %>% filter(time == "2003-05-16T00:00:00Z" & !is.na(chlorophyll))
chla_2003_05 <- chla_2003_05$chlorophyll
chla_2003_05 <- as.numeric(chla_2003_05)
chla_2003_05 <- mean(chla_2003_05)

chla_2003_06 <- chla2 %>% filter(time == "2003-06-16T00:00:00Z" & !is.na(chlorophyll))
chla_2003_06 <- chla_2003_06$chlorophyll
chla_2003_06 <- as.numeric(chla_2003_06)
chla_2003_06 <- mean(chla_2003_06)

chla_2003_07 <- chla2 %>% filter(time == "2003-07-16T00:00:00Z" & !is.na(chlorophyll))
chla_2003_07 <- chla_2003_07$chlorophyll
chla_2003_07 <- as.numeric(chla_2003_07)
chla_2003_07 <- mean(chla_2003_07)

chla_2003_08 <- chla2 %>% filter(time == "2003-08-16T00:00:00Z" & !is.na(chlorophyll))
chla_2003_08 <- chla_2003_08$chlorophyll
chla_2003_08 <- as.numeric(chla_2003_08)
chla_2003_08 <- mean(chla_2003_08)

chla_2003_09 <- chla2 %>% filter(time == "2003-09-16T00:00:00Z" & !is.na(chlorophyll))
chla_2003_09 <- chla_2003_09$chlorophyll
chla_2003_09 <- as.numeric(chla_2003_09)
chla_2003_09 <- mean(chla_2003_09)

chla_2003_10 <- chla2 %>% filter(time == "2003-10-16T00:00:00Z" & !is.na(chlorophyll))
chla_2003_10 <- chla_2003_10$chlorophyll
chla_2003_10 <- as.numeric(chla_2003_10)
chla_2003_10 <- mean(chla_2003_10)

chla_2003_11 <- chla2 %>% filter(time == "2003-11-16T00:00:00Z" & !is.na(chlorophyll))
chla_2003_11 <- chla_2003_11$chlorophyll
chla_2003_11 <- as.numeric(chla_2003_11)
chla_2003_11 <- mean(chla_2003_11)

chla_2003 <- c(chla_2003_01, chla_2003_02, chla_2003_03, chla_2003_04, chla_2003_05, chla_2003_06, chla_2003_07, chla_2003_08, chla_2003_09, chla_2003_10, chla_2003_11, NA)
chla_2003 <- as.numeric(chla_2003)

#2004
chla_2004_01 <- chla2 %>% filter(time == "2004-01-16T00:00:00Z" & !is.na(chlorophyll))
chla_2004_01 <- chla_2004_01$chlorophyll
chla_2004_01 <- as.numeric(chla_2004_01)
chla_2004_01 <- mean(chla_2004_01)

chla_2004_02 <- chla2 %>% filter(time == "2004-02-16T00:00:00Z" & !is.na(chlorophyll))
chla_2004_02 <- chla_2004_02$chlorophyll
chla_2004_02 <- as.numeric(chla_2004_02)
chla_2004_02 <- mean(chla_2004_02)

chla_2004_03 <- chla2 %>% filter(time == "2004-03-16T00:00:00Z" & !is.na(chlorophyll))
chla_2004_03 <- chla_2004_03$chlorophyll
chla_2004_03 <- as.numeric(chla_2004_03)
chla_2004_03 <- mean(chla_2004_03)

chla_2004_04 <- chla2 %>% filter(time == "2004-04-16T00:00:00Z" & !is.na(chlorophyll))
chla_2004_04 <- chla_2004_04$chlorophyll
chla_2004_04 <- as.numeric(chla_2004_04)
chla_2004_04 <- mean(chla_2004_04)

chla_2004_05 <- chla2 %>% filter(time == "2004-05-16T00:00:00Z" & !is.na(chlorophyll))
chla_2004_05 <- chla_2004_05$chlorophyll
chla_2004_05 <- as.numeric(chla_2004_05)
chla_2004_05 <- mean(chla_2004_05)

chla_2004_06 <- chla2 %>% filter(time == "2004-06-16T00:00:00Z" & !is.na(chlorophyll))
chla_2004_06 <- chla_2004_06$chlorophyll
chla_2004_06 <- as.numeric(chla_2004_06)
chla_2004_06 <- mean(chla_2004_06)

chla_2004_07 <- chla2 %>% filter(time == "2004-07-16T00:00:00Z" & !is.na(chlorophyll))
chla_2004_07 <- chla_2004_07$chlorophyll
chla_2004_07 <- as.numeric(chla_2004_07)
chla_2004_07 <- mean(chla_2004_07)

chla_2004_08 <- chla2 %>% filter(time == "2004-08-16T00:00:00Z" & !is.na(chlorophyll))
chla_2004_08 <- chla_2004_08$chlorophyll
chla_2004_08 <- as.numeric(chla_2004_08)
chla_2004_08 <- mean(chla_2004_08)

chla_2004_09 <- chla2 %>% filter(time == "2004-09-16T00:00:00Z" & !is.na(chlorophyll))
chla_2004_09 <- chla_2004_09$chlorophyll
chla_2004_09 <- as.numeric(chla_2004_09)
chla_2004_09 <- mean(chla_2004_09)

chla_2004_10 <- chla2 %>% filter(time == "2004-10-16T00:00:00Z" & !is.na(chlorophyll))
chla_2004_10 <- chla_2004_10$chlorophyll
chla_2004_10 <- as.numeric(chla_2004_10)
chla_2004_10 <- mean(chla_2004_10)

chla_2004_11 <- chla2 %>% filter(time == "2004-11-16T00:00:00Z" & !is.na(chlorophyll))
chla_2004_11 <- chla_2004_11$chlorophyll
chla_2004_11 <- as.numeric(chla_2004_11)
chla_2004_11 <- mean(chla_2004_11)

chla_2004 <- c(chla_2004_01, chla_2004_02, chla_2004_03, chla_2004_04, chla_2004_05, chla_2004_06, chla_2004_07, chla_2004_08, chla_2004_09, chla_2004_10, chla_2004_11, NA)
chla_2004 <- as.numeric(chla_2004)

#2005

chla_2005_02 <- chla2 %>% filter(time == "2005-02-16T00:00:00Z" & !is.na(chlorophyll))
chla_2005_02 <- chla_2005_02$chlorophyll
chla_2005_02 <- as.numeric(chla_2005_02)
chla_2005_02 <- mean(chla_2005_02)

chla_2005_03 <- chla2 %>% filter(time == "2005-03-16T00:00:00Z" & !is.na(chlorophyll))
chla_2005_03 <- chla_2005_03$chlorophyll
chla_2005_03 <- as.numeric(chla_2005_03)
chla_2005_03 <- mean(chla_2005_03)

chla_2005_04 <- chla2 %>% filter(time == "2005-04-16T00:00:00Z" & !is.na(chlorophyll))
chla_2005_04 <- chla_2005_04$chlorophyll
chla_2005_04 <- as.numeric(chla_2005_04)
chla_2005_04 <- mean(chla_2005_04)

chla_2005_05 <- chla2 %>% filter(time == "2005-05-16T00:00:00Z" & !is.na(chlorophyll))
chla_2005_05 <- chla_2005_05$chlorophyll
chla_2005_05 <- as.numeric(chla_2005_05)
chla_2005_05 <- mean(chla_2005_05)

chla_2005_06 <- chla2 %>% filter(time == "2005-06-16T00:00:00Z" & !is.na(chlorophyll))
chla_2005_06 <- chla_2005_06$chlorophyll
chla_2005_06 <- as.numeric(chla_2005_06)
chla_2005_06 <- mean(chla_2005_06)

chla_2005_07 <- chla2 %>% filter(time == "2005-07-16T00:00:00Z" & !is.na(chlorophyll))
chla_2005_07 <- chla_2005_07$chlorophyll
chla_2005_07 <- as.numeric(chla_2005_07)
chla_2005_07 <- mean(chla_2005_07)

chla_2005_08 <- chla2 %>% filter(time == "2005-08-16T00:00:00Z" & !is.na(chlorophyll))
chla_2005_08 <- chla_2005_08$chlorophyll
chla_2005_08 <- as.numeric(chla_2005_08)
chla_2005_08 <- mean(chla_2005_08)

chla_2005_09 <- chla2 %>% filter(time == "2005-09-16T00:00:00Z" & !is.na(chlorophyll))
chla_2005_09 <- chla_2005_09$chlorophyll
chla_2005_09 <- as.numeric(chla_2005_09)
chla_2005_09 <- mean(chla_2005_09)

chla_2005_10 <- chla2 %>% filter(time == "2005-10-16T00:00:00Z" & !is.na(chlorophyll))
chla_2005_10 <- chla_2005_10$chlorophyll
chla_2005_10 <- as.numeric(chla_2005_10)
chla_2005_10 <- mean(chla_2005_10)

chla_2005_11 <- chla2 %>% filter(time == "2005-11-16T00:00:00Z" & !is.na(chlorophyll))
chla_2005_11 <- chla_2005_11$chlorophyll
chla_2005_11 <- as.numeric(chla_2005_11)
chla_2005_11 <- mean(chla_2005_11)

chla_2005 <- c(NA, chla_2005_02, chla_2005_03, chla_2005_04, chla_2005_05, chla_2005_06, chla_2005_07, chla_2005_08, chla_2005_09, chla_2005_10, chla_2005_11, NA)
chla_2005 <- as.numeric(chla_2005)

#2006
chla_2006_01 <- chla2 %>% filter(time == "2006-01-16T00:00:00Z" & !is.na(chlorophyll))
chla_2006_01 <- chla_2006_01$chlorophyll
chla_2006_01 <- as.numeric(chla_2006_01)
chla_2006_01 <- mean(chla_2006_01)

chla_2006_02 <- chla2 %>% filter(time == "2006-02-16T00:00:00Z" & !is.na(chlorophyll))
chla_2006_02 <- chla_2006_02$chlorophyll
chla_2006_02 <- as.numeric(chla_2006_02)
chla_2006_02 <- mean(chla_2006_02)

chla_2006_03 <- chla2 %>% filter(time == "2006-03-16T00:00:00Z" & !is.na(chlorophyll))
chla_2006_03 <- chla_2006_03$chlorophyll
chla_2006_03 <- as.numeric(chla_2006_03)
chla_2006_03 <- mean(chla_2006_03)

chla_2006_04 <- chla2 %>% filter(time == "2006-04-16T00:00:00Z" & !is.na(chlorophyll))
chla_2006_04 <- chla_2006_04$chlorophyll
chla_2006_04 <- as.numeric(chla_2006_04)
chla_2006_04 <- mean(chla_2006_04)

chla_2006_05 <- chla2 %>% filter(time == "2006-05-16T00:00:00Z" & !is.na(chlorophyll))
chla_2006_05 <- chla_2006_05$chlorophyll
chla_2006_05 <- as.numeric(chla_2006_05)
chla_2006_05 <- mean(chla_2006_05)

chla_2006_06 <- chla2 %>% filter(time == "2006-06-16T00:00:00Z" & !is.na(chlorophyll))
chla_2006_06 <- chla_2006_06$chlorophyll
chla_2006_06 <- as.numeric(chla_2006_06)
chla_2006_06 <- mean(chla_2006_06)

chla_2006_07 <- chla2 %>% filter(time == "2006-07-16T00:00:00Z" & !is.na(chlorophyll))
chla_2006_07 <- chla_2006_07$chlorophyll
chla_2006_07 <- as.numeric(chla_2006_07)
chla_2006_07 <- mean(chla_2006_07)

chla_2006_08 <- chla2 %>% filter(time == "2006-08-16T00:00:00Z" & !is.na(chlorophyll))
chla_2006_08 <- chla_2006_08$chlorophyll
chla_2006_08 <- as.numeric(chla_2006_08)
chla_2006_08 <- mean(chla_2006_08)

chla_2006_09 <- chla2 %>% filter(time == "2006-09-16T00:00:00Z" & !is.na(chlorophyll))
chla_2006_09 <- chla_2006_09$chlorophyll
chla_2006_09 <- as.numeric(chla_2006_09)
chla_2006_09 <- mean(chla_2006_09)

chla_2006_10 <- chla2 %>% filter(time == "2006-10-16T00:00:00Z" & !is.na(chlorophyll))
chla_2006_10 <- chla_2006_10$chlorophyll
chla_2006_10 <- as.numeric(chla_2006_10)
chla_2006_10 <- mean(chla_2006_10)

chla_2006_11 <- chla2 %>% filter(time == "2006-11-16T00:00:00Z" & !is.na(chlorophyll))
chla_2006_11 <- chla_2006_11$chlorophyll
chla_2006_11 <- as.numeric(chla_2006_11)
chla_2006_11 <- mean(chla_2006_11)

chla_2006 <- c(chla_2006_01, chla_2006_02, chla_2006_03, chla_2006_04, chla_2006_05, chla_2006_06, chla_2006_07, chla_2006_08, chla_2006_09, chla_2006_10, chla_2006_11, NA)
chla_2006 <- as.numeric(chla_2006)

#2007
chla_2007_01 <- chla2 %>% filter(time == "2007-01-16T00:00:00Z" & !is.na(chlorophyll))
chla_2007_01 <- chla_2007_01$chlorophyll
chla_2007_01 <- as.numeric(chla_2007_01)
chla_2007_01 <- mean(chla_2007_01)

chla_2007_02 <- chla2 %>% filter(time == "2007-02-16T00:00:00Z" & !is.na(chlorophyll))
chla_2007_02 <- chla_2007_02$chlorophyll
chla_2007_02 <- as.numeric(chla_2007_02)
chla_2007_02 <- mean(chla_2007_02)

chla_2007_03 <- chla2 %>% filter(time == "2007-03-16T00:00:00Z" & !is.na(chlorophyll))
chla_2007_03 <- chla_2007_03$chlorophyll
chla_2007_03 <- as.numeric(chla_2007_03)
chla_2007_03 <- mean(chla_2007_03)

chla_2007_04 <- chla2 %>% filter(time == "2007-04-16T00:00:00Z" & !is.na(chlorophyll))
chla_2007_04 <- chla_2007_04$chlorophyll
chla_2007_04 <- as.numeric(chla_2007_04)
chla_2007_04 <- mean(chla_2007_04)

chla_2007_05 <- chla2 %>% filter(time == "2007-05-16T00:00:00Z" & !is.na(chlorophyll))
chla_2007_05 <- chla_2007_05$chlorophyll
chla_2007_05 <- as.numeric(chla_2007_05)
chla_2007_05 <- mean(chla_2007_05)

chla_2007_06 <- chla2 %>% filter(time == "2007-06-16T00:00:00Z" & !is.na(chlorophyll))
chla_2007_06 <- chla_2007_06$chlorophyll
chla_2007_06 <- as.numeric(chla_2007_06)
chla_2007_06 <- mean(chla_2007_06)

chla_2007_07 <- chla2 %>% filter(time == "2007-07-16T00:00:00Z" & !is.na(chlorophyll))
chla_2007_07 <- chla_2007_07$chlorophyll
chla_2007_07 <- as.numeric(chla_2007_07)
chla_2007_07 <- mean(chla_2007_07)

chla_2007_08 <- chla2 %>% filter(time == "2007-08-16T00:00:00Z" & !is.na(chlorophyll))
chla_2007_08 <- chla_2007_08$chlorophyll
chla_2007_08 <- as.numeric(chla_2007_08)
chla_2007_08 <- mean(chla_2007_08)

chla_2007_09 <- chla2 %>% filter(time == "2007-09-16T00:00:00Z" & !is.na(chlorophyll))
chla_2007_09 <- chla_2007_09$chlorophyll
chla_2007_09 <- as.numeric(chla_2007_09)
chla_2007_09 <- mean(chla_2007_09)

chla_2007_10 <- chla2 %>% filter(time == "2007-10-16T00:00:00Z" & !is.na(chlorophyll))
chla_2007_10 <- chla_2007_10$chlorophyll
chla_2007_10 <- as.numeric(chla_2007_10)
chla_2007_10 <- mean(chla_2007_10)

chla_2007_11 <- chla2 %>% filter(time == "2007-11-16T00:00:00Z" & !is.na(chlorophyll))
chla_2007_11 <- chla_2007_11$chlorophyll
chla_2007_11 <- as.numeric(chla_2007_11)
chla_2007_11 <- mean(chla_2007_11)

chla_2007 <- c(chla_2007_01, chla_2007_02, chla_2007_03, chla_2007_04, chla_2007_05, chla_2007_06, chla_2007_07, chla_2007_08, chla_2007_09, chla_2007_10, chla_2007_11, NA)
chla_2007 <- as.numeric(chla_2007)

#2008

chla_2008_04 <- chla2 %>% filter(time == "2008-04-16T00:00:00Z" & !is.na(chlorophyll))
chla_2008_04 <- chla_2008_04$chlorophyll
chla_2008_04 <- as.numeric(chla_2008_04)
chla_2008_04 <- mean(chla_2008_04)

chla_2008_05 <- chla2 %>% filter(time == "2008-05-16T00:00:00Z" & !is.na(chlorophyll))
chla_2008_05 <- chla_2008_05$chlorophyll
chla_2008_05 <- as.numeric(chla_2008_05)
chla_2008_05 <- mean(chla_2008_05)

chla_2008_06 <- chla2 %>% filter(time == "2008-06-16T00:00:00Z" & !is.na(chlorophyll))
chla_2008_06 <- chla_2008_06$chlorophyll
chla_2008_06 <- as.numeric(chla_2008_06)
chla_2008_06 <- mean(chla_2008_06)

chla_2008_08 <- chla2 %>% filter(time == "2008-08-16T00:00:00Z" & !is.na(chlorophyll))
chla_2008_08 <- chla_2008_08$chlorophyll
chla_2008_08 <- as.numeric(chla_2008_08)
chla_2008_08 <- mean(chla_2008_08)

chla_2008_09 <- chla2 %>% filter(time == "2008-09-16T00:00:00Z" & !is.na(chlorophyll))
chla_2008_09 <- chla_2008_09$chlorophyll
chla_2008_09 <- as.numeric(chla_2008_09)
chla_2008_09 <- mean(chla_2008_09)

chla_2008_10 <- chla2 %>% filter(time == "2008-10-16T00:00:00Z" & !is.na(chlorophyll))
chla_2008_10 <- chla_2008_10$chlorophyll
chla_2008_10 <- as.numeric(chla_2008_10)
chla_2008_10 <- mean(chla_2008_10)

chla_2008 <- c(NA, NA, NA, chla_2008_04, chla_2008_05, chla_2008_06, NA, chla_2008_08, chla_2008_09, chla_2008_10, NA, NA)
chla_2008 <- as.numeric(chla_2008)

#2009
chla_2009_01 <- chla2 %>% filter(time == "2009-01-16T00:00:00Z" & !is.na(chlorophyll))
chla_2009_01 <- chla_2009_01$chlorophyll
chla_2009_01 <- as.numeric(chla_2009_01)
chla_2009_01 <- mean(chla_2009_01)

chla_2009_02 <- chla2 %>% filter(time == "2009-02-16T00:00:00Z" & !is.na(chlorophyll))
chla_2009_02 <- chla_2009_02$chlorophyll
chla_2009_02 <- as.numeric(chla_2009_02)
chla_2009_02 <- mean(chla_2009_02)

chla_2009_03 <- chla2 %>% filter(time == "2009-03-16T00:00:00Z" & !is.na(chlorophyll))
chla_2009_03 <- chla_2009_03$chlorophyll
chla_2009_03 <- as.numeric(chla_2009_03)
chla_2009_03 <- mean(chla_2009_03)

chla_2009_04 <- chla2 %>% filter(time == "2009-04-16T00:00:00Z" & !is.na(chlorophyll))
chla_2009_04 <- chla_2009_04$chlorophyll
chla_2009_04 <- as.numeric(chla_2009_04)
chla_2009_04 <- mean(chla_2009_04)

chla_2009_06 <- chla2 %>% filter(time == "2009-06-16T00:00:00Z" & !is.na(chlorophyll))
chla_2009_06 <- chla_2009_06$chlorophyll
chla_2009_06 <- as.numeric(chla_2009_06)
chla_2009_06 <- mean(chla_2009_06)

chla_2009_07 <- chla2 %>% filter(time == "2009-07-16T00:00:00Z" & !is.na(chlorophyll))
chla_2009_07 <- chla_2009_07$chlorophyll
chla_2009_07 <- as.numeric(chla_2009_07)
chla_2009_07 <- mean(chla_2009_07)

chla_2009_08 <- chla2 %>% filter(time == "2009-08-16T00:00:00Z" & !is.na(chlorophyll))
chla_2009_08 <- chla_2009_08$chlorophyll
chla_2009_08 <- as.numeric(chla_2009_08)
chla_2009_08 <- mean(chla_2009_08)

chla_2009_10 <- chla2 %>% filter(time == "2009-10-16T00:00:00Z" & !is.na(chlorophyll))
chla_2009_10 <- chla_2009_10$chlorophyll
chla_2009_10 <- as.numeric(chla_2009_10)
chla_2009_10 <- mean(chla_2009_10)

chla_2009 <- c(chla_2009_01, chla_2009_02, chla_2009_03, chla_2009_04, NA, chla_2009_06, chla_2009_07, chla_2009_08, NA, chla_2009_10, NA, NA)
chla_2009 <- as.numeric(chla_2009)

#2010

chla_2010_02 <- chla2 %>% filter(time == "2010-02-16T00:00:00Z" & !is.na(chlorophyll))
chla_2010_02 <- chla_2010_02$chlorophyll
chla_2010_02 <- as.numeric(chla_2010_02)
chla_2010_02 <- mean(chla_2010_02)

chla_2010_03 <- chla2 %>% filter(time == "2010-03-16T00:00:00Z" & !is.na(chlorophyll))
chla_2010_03 <- chla_2010_03$chlorophyll
chla_2010_03 <- as.numeric(chla_2010_03)
chla_2010_03 <- mean(chla_2010_03)

chla_2010_04 <- chla2 %>% filter(time == "2010-04-16T00:00:00Z" & !is.na(chlorophyll))
chla_2010_04 <- chla_2010_04$chlorophyll
chla_2010_04 <- as.numeric(chla_2010_04)
chla_2010_04 <- mean(chla_2010_04)

chla_2010_05 <- chla2 %>% filter(time == "2010-05-16T00:00:00Z" & !is.na(chlorophyll))
chla_2010_05 <- chla_2010_05$chlorophyll
chla_2010_05 <- as.numeric(chla_2010_05)
chla_2010_05 <- mean(chla_2010_05)

chla_2010_06 <- chla2 %>% filter(time == "2010-06-16T00:00:00Z" & !is.na(chlorophyll))
chla_2010_06 <- chla_2010_06$chlorophyll
chla_2010_06 <- as.numeric(chla_2010_06)
chla_2010_06 <- mean(chla_2010_06)

chla_2010_07 <- chla2 %>% filter(time == "2010-07-16T00:00:00Z" & !is.na(chlorophyll))
chla_2010_07 <- chla_2010_07$chlorophyll
chla_2010_07 <- as.numeric(chla_2010_07)
chla_2010_07 <- mean(chla_2010_07)

chla_2010_08 <- chla2 %>% filter(time == "2010-08-16T00:00:00Z" & !is.na(chlorophyll))
chla_2010_08 <- chla_2010_08$chlorophyll
chla_2010_08 <- as.numeric(chla_2010_08)
chla_2010_08 <- mean(chla_2010_08)

chla_2010_09 <- chla2 %>% filter(time == "2010-09-16T00:00:00Z" & !is.na(chlorophyll))
chla_2010_09 <- chla_2010_09$chlorophyll
chla_2010_09 <- as.numeric(chla_2010_09)
chla_2010_09 <- mean(chla_2010_09)

chla_2010_10 <- chla2 %>% filter(time == "2010-10-16T00:00:00Z" & !is.na(chlorophyll))
chla_2010_10 <- chla_2010_10$chlorophyll
chla_2010_10 <- as.numeric(chla_2010_10)
chla_2010_10 <- mean(chla_2010_10)

chla_2010 <- c(NA, chla_2010_02, chla_2010_03, chla_2010_04, chla_2010_05, chla_2010_06, chla_2010_07, chla_2010_08, chla_2010_09, chla_2010_10, NA, NA)
chla_2010 <- as.numeric(chla_2010)

#2011
chla_2011_01 <- chla1 %>% filter(time == "2011-01-16T00:00:00Z" & !is.na(chlorophyll))
chla_2011_01 <- chla_2011_01$chlorophyll
as.numeric(chla_2011_01)

chla_2011_02 <- chla1 %>% filter(time == "2011-02-16T00:00:00Z" & !is.na(chlorophyll))
chla_2011_02 <- chla_2011_02$chlorophyll
chla_2011_02 <- as.numeric(chla_2011_02)
chla_2011_02 <- mean(chla_2011_02)

chla_2011_03 <- chla1 %>% filter(time == "2011-03-16T00:00:00Z" & !is.na(chlorophyll))
chla_2011_03 <- chla_2011_03$chlorophyll
chla_2011_03 <- as.numeric(chla_2011_03)
chla_2011_03 <- mean(chla_2011_03)

chla_2011_04 <- chla1 %>% filter(time == "2011-04-16T00:00:00Z" & !is.na(chlorophyll))
chla_2011_04 <- chla_2011_04$chlorophyll
chla_2011_04 <- as.numeric(chla_2011_04)
chla_2011_04 <- mean(chla_2011_04)

chla_2011_05 <- chla1 %>% filter(time == "2011-05-16T00:00:00Z" & !is.na(chlorophyll))
chla_2011_05 <- chla_2011_05$chlorophyll
chla_2011_05 <- as.numeric(chla_2011_05)
chla_2011_05 <- mean(chla_2011_05)

chla_2011_06 <- chla1 %>% filter(time == "2011-06-16T00:00:00Z" & !is.na(chlorophyll))
chla_2011_06 <- chla_2011_06$chlorophyll
chla_2011_06 <- as.numeric(chla_2011_06)
chla_2011_06 <- mean(chla_2011_06)

chla_2011_07 <- chla1 %>% filter(time == "2011-07-16T00:00:00Z" & !is.na(chlorophyll))
chla_2011_07 <- chla_2011_07$chlorophyll
chla_2011_07 <- as.numeric(chla_2011_07)
chla_2011_07 <- mean(chla_2011_07)

chla_2011_08 <- chla1 %>% filter(time == "2011-08-16T00:00:00Z" & !is.na(chlorophyll))
chla_2011_08 <- chla_2011_08$chlorophyll
chla_2011_08 <- as.numeric(chla_2011_08)
chla_2011_08 <- mean(chla_2011_08)

chla_2011_09 <- chla1 %>% filter(time == "2011-09-16T00:00:00Z" & !is.na(chlorophyll))
chla_2011_09 <- chla_2011_09$chlorophyll
chla_2011_09 <- as.numeric(chla_2011_09)
chla_2011_09 <- mean(chla_2011_09)

chla_2011_10 <- chla1 %>% filter(time == "2011-10-16T00:00:00Z" & !is.na(chlorophyll))
chla_2011_10 <- chla_2011_10$chlorophyll
chla_2011_10 <- as.numeric(chla_2011_10)
chla_2011_10 <- mean(chla_2011_10)

chla_2011_11 <- chla1 %>% filter(time == "2011-11-16T00:00:00Z" & !is.na(chlorophyll))
chla_2011_11 <- chla_2011_11$chlorophyll
chla_2011_11 <- as.numeric(chla_2011_11)
chla_2011_11 <- mean(chla_2011_11)

chla_2011 <- c(chla_2011_01, chla_2011_02, chla_2011_03, chla_2011_04, chla_2011_05, chla_2011_06, chla_2011_07, chla_2011_08, chla_2011_09, chla_2011_10, chla_2011_11, NA)
chla_2011 <- as.numeric(chla_2011)

#2012
chla_2012_01 <- chla1 %>% filter(time == "2012-01-16T00:00:00Z" & !is.na(chlorophyll))
chla_2012_01 <- chla_2012_01$chlorophyll
chla_2012_01 <- as.numeric(chla_2012_01)
chla_2012_01 <- mean(chla_2012_01)

chla_2012_02 <- chla1 %>% filter(time == "2012-02-16T00:00:00Z" & !is.na(chlorophyll))
chla_2012_02 <- chla_2012_02$chlorophyll
chla_2012_02 <- as.numeric(chla_2012_02)
chla_2012_02 <- mean(chla_2012_02)

chla_2012_03 <- chla1 %>% filter(time == "2012-03-16T00:00:00Z" & !is.na(chlorophyll))
chla_2012_03 <- chla_2012_03$chlorophyll
chla_2012_03 <- as.numeric(chla_2012_03)
chla_2012_03 <- mean(chla_2012_03)

chla_2012_04 <- chla1 %>% filter(time == "2012-04-16T00:00:00Z" & !is.na(chlorophyll))
chla_2012_04 <- chla_2012_04$chlorophyll
chla_2012_04 <- as.numeric(chla_2012_04)
chla_2012_04 <- mean(chla_2012_04)

chla_2012_05 <- chla1 %>% filter(time == "2012-05-16T00:00:00Z" & !is.na(chlorophyll))
chla_2012_05 <- chla_2012_05$chlorophyll
chla_2012_05 <- as.numeric(chla_2012_05)
chla_2012_05 <- mean(chla_2012_05)

chla_2012_06 <- chla1 %>% filter(time == "2012-06-16T00:00:00Z" & !is.na(chlorophyll))
chla_2012_06 <- chla_2012_06$chlorophyll
chla_2012_06 <- as.numeric(chla_2012_06)
chla_2012_06 <- mean(chla_2012_06)

chla_2012_07 <- chla1 %>% filter(time == "2012-07-16T00:00:00Z" & !is.na(chlorophyll))
chla_2012_07 <- chla_2012_07$chlorophyll
chla_2012_07 <- as.numeric(chla_2012_07)
chla_2012_07 <- mean(chla_2012_07)

chla_2012_08 <- chla1 %>% filter(time == "2012-08-16T00:00:00Z" & !is.na(chlorophyll))
chla_2012_08 <- chla_2012_08$chlorophyll
chla_2012_08 <- as.numeric(chla_2012_08)
chla_2012_08 <- mean(chla_2012_08)

chla_2012_09 <- chla1 %>% filter(time == "2012-09-16T00:00:00Z" & !is.na(chlorophyll))
chla_2012_09 <- chla_2012_09$chlorophyll
chla_2012_09 <- as.numeric(chla_2012_09)
chla_2012_09 <- mean(chla_2012_09)

chla_2012_10 <- chla1 %>% filter(time == "2012-10-16T00:00:00Z" & !is.na(chlorophyll))
chla_2012_10 <- chla_2012_10$chlorophyll
chla_2012_10 <- as.numeric(chla_2012_10)
chla_2012_10 <- mean(chla_2012_10)

chla_2012_11 <- chla1 %>% filter(time == "2012-11-16T00:00:00Z" & !is.na(chlorophyll))
chla_2012_11 <- chla_2012_11$chlorophyll
chla_2012_11 <- as.numeric(chla_2012_11)
chla_2012_11 <- mean(chla_2012_11)

chla_2012 <- c(chla_2012_01, chla_2012_02, chla_2012_03, chla_2012_04, chla_2012_05, chla_2012_06, chla_2012_07, chla_2012_08, chla_2012_09, chla_2012_10, chla_2012_11, NA)
chla_2012 <- as.numeric(chla_2012)

#2013
chla_2013_01 <- chla1 %>% filter(time == "2013-01-16T00:00:00Z" & !is.na(chlorophyll))
chla_2013_01 <- chla_2013_01$chlorophyll
chla_2013_01 <- as.numeric(chla_2013_01)
chla_2013_01 <- mean(chla_2013_01)

chla_2013_02 <- chla1 %>% filter(time == "2013-02-16T00:00:00Z" & !is.na(chlorophyll))
chla_2013_02 <- chla_2013_02$chlorophyll
chla_2013_02 <- as.numeric(chla_2012_03)
chla_2013_02 <- mean(chla_2013_02)

chla_2013_03 <- chla1 %>% filter(time == "2013-03-16T00:00:00Z" & !is.na(chlorophyll))
chla_2013_03 <- chla_2013_03$chlorophyll
chla_2013_03 <- as.numeric(chla_2013_03)
chla_2013_03 <- mean(chla_2013_03)

chla_2013_04 <- chla1 %>% filter(time == "2013-04-16T00:00:00Z" & !is.na(chlorophyll))
chla_2013_04 <- chla_2013_04$chlorophyll
chla_2013_04 <- as.numeric(chla_2013_04)
chla_2013_04 <- mean(chla_2013_04)

chla_2013_05 <- chla1 %>% filter(time == "2013-05-16T00:00:00Z" & !is.na(chlorophyll))
chla_2013_05 <- chla_2013_05$chlorophyll
chla_2013_05 <- as.numeric(chla_2013_05)
chla_2013_05 <- mean(chla_2013_05)

chla_2013_06 <- chla1 %>% filter(time == "2013-06-16T00:00:00Z" & !is.na(chlorophyll))
chla_2013_06 <- chla_2013_06$chlorophyll
chla_2013_06 <- as.numeric(chla_2013_06)
chla_2013_06 <- mean(chla_2013_06)

chla_2013_07 <- chla1 %>% filter(time == "2013-07-16T00:00:00Z" & !is.na(chlorophyll))
chla_2013_07 <- chla_2013_07$chlorophyll
chla_2013_07 <- as.numeric(chla_2013_07)
chla_2013_07 <- mean(chla_2013_07)

chla_2013_08 <- chla1 %>% filter(time == "2013-08-16T00:00:00Z" & !is.na(chlorophyll))
chla_2013_08 <- chla_2013_08$chlorophyll
chla_2013_08 <- as.numeric(chla_2013_08)
chla_2013_08 <- mean(chla_2013_08)

chla_2013_09 <- chla1 %>% filter(time == "2013-09-16T00:00:00Z" & !is.na(chlorophyll))
chla_2013_09 <- chla_2013_09$chlorophyll
chla_2013_09 <- as.numeric(chla_2013_09)
chla_2013_09 <- mean(chla_2013_09)

chla_2013_10 <- chla1 %>% filter(time == "2013-10-16T00:00:00Z" & !is.na(chlorophyll))
chla_2013_10 <- chla_2013_10$chlorophyll
chla_2013_10 <- as.numeric(chla_2013_10)
chla_2013_10 <- mean(chla_2013_10)

chla_2013_11 <- chla1 %>% filter(time == "2013-11-16T00:00:00Z" & !is.na(chlorophyll))
chla_2013_11 <- chla_2013_11$chlorophyll
chla_2013_11 <- as.numeric(chla_2013_11)
chla_2013_11 <- mean(chla_2013_11)

chla_2013 <- c(chla_2013_01, chla_2013_02, chla_2013_03, chla_2013_04, chla_2013_05, chla_2013_06, chla_2013_07, chla_2013_08, chla_2013_09, chla_2013_10, chla_2013_11, NA)
chla_2013 <- as.numeric(chla_2013)

#2012
chla_2014_01 <- chla1 %>% filter(time == "2014-01-16T00:00:00Z" & !is.na(chlorophyll))
chla_2014_01 <- chla_2014_01$chlorophyll
chla_2014_01 <- as.numeric(chla_2014_01)
chla_2014_01 <- mean(chla_2014_01)

chla_2014_02 <- chla1 %>% filter(time == "2014-02-16T00:00:00Z" & !is.na(chlorophyll))
chla_2014_02 <- chla_2014_02$chlorophyll
chla_2014_02 <- as.numeric(chla_2014_02)
chla_2014_02 <- mean(chla_2014_02)

chla_2014_03 <- chla1 %>% filter(time == "2014-03-16T00:00:00Z" & !is.na(chlorophyll))
chla_2014_03 <- chla_2014_03$chlorophyll
chla_2014_03 <- as.numeric(chla_2014_03)
chla_2014_03 <- mean(chla_2014_03)

chla_2014_04 <- chla1 %>% filter(time == "2014-04-16T00:00:00Z" & !is.na(chlorophyll))
chla_2014_04 <- chla_2014_04$chlorophyll
chla_2014_04 <- as.numeric(chla_2014_04)
chla_2014_04 <- mean(chla_2014_04)

chla_2014_05 <- chla1 %>% filter(time == "2014-05-16T00:00:00Z" & !is.na(chlorophyll))
chla_2014_05 <- chla_2014_05$chlorophyll
chla_2014_05 <- as.numeric(chla_2014_05)
chla_2014_05 <- mean(chla_2014_05)

chla_2014_06 <- chla1 %>% filter(time == "2014-06-16T00:00:00Z" & !is.na(chlorophyll))
chla_2014_06 <- chla_2014_06$chlorophyll
chla_2014_06 <- as.numeric(chla_2014_06)
chla_2014_06 <- mean(chla_2014_06)

chla_2014_07 <- chla1 %>% filter(time == "2014-07-16T00:00:00Z" & !is.na(chlorophyll))
chla_2014_07 <- chla_2014_07$chlorophyll
chla_2014_07 <- as.numeric(chla_2014_07)
chla_2014_07 <- mean(chla_2014_07)

chla_2014_08 <- chla1 %>% filter(time == "2014-08-16T00:00:00Z" & !is.na(chlorophyll))
chla_2014_08 <- chla_2014_08$chlorophyll
chla_2014_08 <- as.numeric(chla_2014_08)
chla_2014_08 <- mean(chla_2014_08)

chla_2014_09 <- chla1 %>% filter(time == "2014-09-16T00:00:00Z" & !is.na(chlorophyll))
chla_2014_09 <- chla_2014_09$chlorophyll
chla_2014_09 <- as.numeric(chla_2014_09)
chla_2014_09 <- mean(chla_2014_09)

chla_2014_10 <- chla1 %>% filter(time == "2014-10-16T00:00:00Z" & !is.na(chlorophyll))
chla_2014_10 <- chla_2014_10$chlorophyll
chla_2014_10 <- as.numeric(chla_2014_10)
chla_2014_10 <- mean(chla_2014_10)

chla_2014_11 <- chla1 %>% filter(time == "2014-11-16T00:00:00Z" & !is.na(chlorophyll))
chla_2014_11 <- chla_2014_11$chlorophyll
chla_2014_11 <- as.numeric(chla_2014_11)
chla_2014_11 <- mean(chla_2014_11)

chla_2014 <- c(chla_2014_01, chla_2014_02, chla_2014_03, chla_2014_04, chla_2014_05, chla_2014_06, chla_2014_07, chla_2014_08, chla_2014_09, chla_2014_10, chla_2014_11, NA)
chla_2014 <- as.numeric(chla_2014)

#2015
chla_2015_01 <- chla1 %>% filter(time == "2015-01-16T00:00:00Z" & !is.na(chlorophyll))
chla_2015_01 <- chla_2015_01$chlorophyll
chla_2015_01 <- as.numeric(chla_2015_01)
chla_2015_01 <- mean(chla_2015_01)

chla_2015_02 <- chla1 %>% filter(time == "2015-02-16T00:00:00Z" & !is.na(chlorophyll))
chla_2015_02 <- chla_2015_02$chlorophyll
chla_2015_02 <- as.numeric(chla_2015_02)
chla_2015_02 <- mean(chla_2015_02)

chla_2015_03 <- chla1 %>% filter(time == "2015-03-16T00:00:00Z" & !is.na(chlorophyll))
chla_2015_03 <- chla_2015_03$chlorophyll
chla_2015_03 <- as.numeric(chla_2015_03)
chla_2015_03 <- mean(chla_2015_03)

chla_2015_04 <- chla1 %>% filter(time == "2015-04-16T00:00:00Z" & !is.na(chlorophyll))
chla_2015_04 <- chla_2015_04$chlorophyll
chla_2015_04 <- as.numeric(chla_2015_04)
chla_2015_04 <- mean(chla_2015_04)

chla_2015_05 <- chla1 %>% filter(time == "2015-05-16T00:00:00Z" & !is.na(chlorophyll))
chla_2015_05 <- chla_2015_05$chlorophyll
chla_2015_05 <- as.numeric(chla_2015_05)
chla_2015_05 <- mean(chla_2015_05)

chla_2015_06 <- chla1 %>% filter(time == "2015-06-16T00:00:00Z" & !is.na(chlorophyll))
chla_2015_06 <- chla_2015_06$chlorophyll
chla_2015_06 <- as.numeric(chla_2015_06)
chla_2015_06 <- mean(chla_2015_06)

chla_2015_07 <- chla1 %>% filter(time == "2015-07-16T00:00:00Z" & !is.na(chlorophyll))
chla_2015_07 <- chla_2015_07$chlorophyll
chla_2015_07 <- as.numeric(chla_2015_07)
chla_2015_07 <- mean(chla_2015_07)

chla_2015_08 <- chla1 %>% filter(time == "2015-08-16T00:00:00Z" & !is.na(chlorophyll))
chla_2015_08 <- chla_2015_08$chlorophyll
chla_2015_08 <- as.numeric(chla_2015_08)
chla_2015_08 <- mean(chla_2015_08)

chla_2015_09 <- chla1 %>% filter(time == "2015-09-16T00:00:00Z" & !is.na(chlorophyll))
chla_2015_09 <- chla_2015_09$chlorophyll
chla_2015_09 <- as.numeric(chla_2015_09)
chla_2015_09 <- mean(chla_2015_09)

chla_2015_10 <- chla1 %>% filter(time == "2015-10-16T00:00:00Z" & !is.na(chlorophyll))
chla_2015_10 <- chla_2015_10$chlorophyll
chla_2015_10 <- as.numeric(chla_2015_10)
chla_2015_10 <- mean(chla_2015_10)

chla_2015_11 <- chla1 %>% filter(time == "2015-11-16T00:00:00Z" & !is.na(chlorophyll))
chla_2015_11 <- chla_2015_11$chlorophyll
chla_2015_11 <- as.numeric(chla_2015_11)
chla_2015_11 <- mean(chla_2015_11)

chla_2015 <- c(chla_2015_01, chla_2015_02, chla_2015_03, chla_2015_04, chla_2015_05, chla_2015_06, chla_2015_07, chla_2015_08, chla_2015_09, chla_2015_10, chla_2015_11, NA)
chla_2015 <- as.numeric(chla_2015)

#2016
chla_2016_01 <- chla1 %>% filter(time == "2016-01-16T00:00:00Z" & !is.na(chlorophyll))
chla_2016_01 <- chla_2016_01$chlorophyll
chla_2016_01 <- as.numeric(chla_2016_01)
chla_2016_01 <- mean(chla_2016_01)

chla_2016_02 <- chla1 %>% filter(time == "2016-02-16T00:00:00Z" & !is.na(chlorophyll))
chla_2016_02 <- chla_2016_02$chlorophyll
chla_2016_02 <- as.numeric(chla_2016_02)
chla_2016_02 <- mean(chla_2016_02)

chla_2016_03 <- chla1 %>% filter(time == "2016-03-16T00:00:00Z" & !is.na(chlorophyll))
chla_2016_03 <- chla_2016_03$chlorophyll
chla_2016_03 <- as.numeric(chla_2016_03)
chla_2016_03 <- mean(chla_2016_03)

chla_2016_04 <- chla1 %>% filter(time == "2016-04-16T00:00:00Z" & !is.na(chlorophyll))
chla_2016_04 <- chla_2016_04$chlorophyll
chla_2016_04 <- as.numeric(chla_2016_04)
chla_2016_04 <- mean(chla_2016_04)

chla_2016_05 <- chla1 %>% filter(time == "2016-05-16T00:00:00Z" & !is.na(chlorophyll))
chla_2016_05 <- chla_2016_05$chlorophyll
chla_2016_05 <- as.numeric(chla_2016_05)
chla_2016_05 <- mean(chla_2016_05)

chla_2016_06 <- chla1 %>% filter(time == "2016-06-16T00:00:00Z" & !is.na(chlorophyll))
chla_2016_06 <- chla_2016_06$chlorophyll
chla_2016_06 <- as.numeric(chla_2016_06)
chla_2016_06 <- mean(chla_2016_06)

chla_2016_07 <- chla1 %>% filter(time == "2016-07-16T00:00:00Z" & !is.na(chlorophyll))
chla_2016_07 <- chla_2016_07$chlorophyll
chla_2016_07 <- as.numeric(chla_2016_07)
chla_2016_07 <- mean(chla_2016_07)

chla_2016_08 <- chla1 %>% filter(time == "2016-08-16T00:00:00Z" & !is.na(chlorophyll))
chla_2016_08 <- chla_2016_08$chlorophyll
chla_2016_08 <- as.numeric(chla_2016_08)
chla_2016_08 <- mean(chla_2016_08)

chla_2016_09 <- chla1 %>% filter(time == "2016-09-16T00:00:00Z" & !is.na(chlorophyll))
chla_2016_09 <- chla_2016_09$chlorophyll
chla_2016_09 <- as.numeric(chla_2016_09)
chla_2016_09 <- mean(chla_2016_09)

chla_2016_10 <- chla1 %>% filter(time == "2016-10-16T00:00:00Z" & !is.na(chlorophyll))
chla_2016_10 <- chla_2016_10$chlorophyll
chla_2016_10 <- as.numeric(chla_2016_10)
chla_2016_10 <- mean(chla_2016_10)

chla_2016_11 <- chla1 %>% filter(time == "2016-11-16T00:00:00Z" & !is.na(chlorophyll))
chla_2016_11 <- chla_2016_11$chlorophyll
chla_2016_11 <- as.numeric(chla_2016_11)
chla_2016_11 <- mean(chla_2016_11)

chla_2016 <- c(chla_2016_01, chla_2016_02, chla_2016_03, chla_2016_04, chla_2016_05, chla_2016_06, chla_2016_07, chla_2016_08, chla_2016_09, chla_2016_10, chla_2016_11, NA)
chla_2016 <- as.numeric(chla_2016)

#2017
chla_2017_01 <- chla1 %>% filter(time == "2017-01-16T00:00:00Z" & !is.na(chlorophyll))
chla_2017_01 <- chla_2017_01$chlorophyll
chla_2017_01 <- as.numeric(chla_2017_01)
chla_2017_01 <- mean(chla_2017_01)

chla_2017_02 <- chla1 %>% filter(time == "2017-02-16T00:00:00Z" & !is.na(chlorophyll))
chla_2017_02 <- chla_2017_02$chlorophyll
chla_2017_02 <- as.numeric(chla_2017_02)
chla_2017_02 <- mean(chla_2017_02)

chla_2017_03 <- chla1 %>% filter(time == "2017-03-16T00:00:00Z" & !is.na(chlorophyll))
chla_2017_03 <- chla_2017_03$chlorophyll
chla_2017_03 <- as.numeric(chla_2017_03)
chla_2017_03 <- mean(chla_2017_03)

chla_2017_04 <- chla1 %>% filter(time == "2017-04-16T00:00:00Z" & !is.na(chlorophyll))
chla_2017_04 <- chla_2017_04$chlorophyll
chla_2017_04 <- as.numeric(chla_2017_04)
chla_2017_04 <- mean(chla_2017_04)

chla_2017_05 <- chla1 %>% filter(time == "2017-05-16T00:00:00Z" & !is.na(chlorophyll))
chla_2017_05 <- chla_2017_05$chlorophyll
chla_2017_05 <- as.numeric(chla_2017_05)
chla_2017_05 <- mean(chla_2017_05)

chla_2017_06 <- chla1 %>% filter(time == "2017-06-16T00:00:00Z" & !is.na(chlorophyll))
chla_2017_06 <- chla_2017_06$chlorophyll
chla_2017_06 <- as.numeric(chla_2017_06)
chla_2017_06 <- mean(chla_2017_06)

chla_2017_07 <- chla1 %>% filter(time == "2017-07-16T00:00:00Z" & !is.na(chlorophyll))
chla_2017_07 <- chla_2017_07$chlorophyll
chla_2017_07 <- as.numeric(chla_2017_07)
chla_2017_07 <- mean(chla_2017_07)

chla_2017_08 <- chla1 %>% filter(time == "2017-08-16T00:00:00Z" & !is.na(chlorophyll))
chla_2017_08 <- chla_2017_08$chlorophyll
chla_2017_08 <- as.numeric(chla_2017_08)
chla_2017_08 <- mean(chla_2017_08)

chla_2017_09 <- chla1 %>% filter(time == "2017-09-16T00:00:00Z" & !is.na(chlorophyll))
chla_2017_09 <- chla_2017_09$chlorophyll
chla_2017_09 <- as.numeric(chla_2017_09)
chla_2017_09 <- mean(chla_2017_09)

chla_2017_10 <- chla1 %>% filter(time == "2017-10-16T00:00:00Z" & !is.na(chlorophyll))
chla_2017_10 <- chla_2017_10$chlorophyll
chla_2017_10 <- as.numeric(chla_2017_10)
chla_2017_10 <- mean(chla_2017_10)

chla_2017_11 <- chla1 %>% filter(time == "2017-11-16T00:00:00Z" & !is.na(chlorophyll))
chla_2017_11 <- chla_2017_11$chlorophyll
chla_2017_11 <- as.numeric(chla_2017_11)
chla_2017_11 <- mean(chla_2017_11)

chla_2017 <- c(chla_2017_01, chla_2017_02, chla_2017_03, chla_2017_04, chla_2017_05, chla_2017_06, chla_2017_07, chla_2017_08, chla_2017_09, chla_2017_10, chla_2017_11, NA)
chla_2017 <- as.numeric(chla_2017)

#2018
chla_2018_01 <- chla1 %>% filter(time == "2018-01-16T00:00:00Z" & !is.na(chlorophyll))
chla_2018_01 <- chla_2018_01$chlorophyll
chla_2018_01 <- as.numeric(chla_2018_01)
chla_2018_01 <- mean(chla_2018_01)

chla_2018_02 <- chla1 %>% filter(time == "2018-02-16T00:00:00Z" & !is.na(chlorophyll))
chla_2018_02 <- chla_2018_02$chlorophyll
chla_2018_02 <- as.numeric(chla_2018_02)
chla_2018_02 <- mean(chla_2018_02)

chla_2018_03 <- chla1 %>% filter(time == "2018-03-16T00:00:00Z" & !is.na(chlorophyll))
chla_2018_03 <- chla_2018_03$chlorophyll
chla_2018_03 <- as.numeric(chla_2018_03)
chla_2018_03 <- mean(chla_2018_03)

chla_2018_04 <- chla1 %>% filter(time == "2018-04-16T00:00:00Z" & !is.na(chlorophyll))
chla_2018_04 <- chla_2018_04$chlorophyll
chla_2018_04 <- as.numeric(chla_2018_04)
chla_2018_04 <- mean(chla_2018_04)

chla_2018_05 <- chla1 %>% filter(time == "2018-05-16T00:00:00Z" & !is.na(chlorophyll))
chla_2018_05 <- chla_2018_05$chlorophyll
chla_2018_05 <- as.numeric(chla_2018_05)
chla_2018_05 <- mean(chla_2018_05)

chla_2018_06 <- chla1 %>% filter(time == "2018-06-16T00:00:00Z" & !is.na(chlorophyll))
chla_2018_06 <- chla_2018_06$chlorophyll
chla_2018_06 <- as.numeric(chla_2018_06)
chla_2018_06 <- mean(chla_2018_06)

chla_2018_07 <- chla1 %>% filter(time == "2018-07-16T00:00:00Z" & !is.na(chlorophyll))
chla_2018_07 <- chla_2018_07$chlorophyll
chla_2018_07 <- as.numeric(chla_2018_07)
chla_2018_07 <- mean(chla_2018_07)

chla_2018_08 <- chla1 %>% filter(time == "2018-08-16T00:00:00Z" & !is.na(chlorophyll))
chla_2018_08 <- chla_2018_08$chlorophyll
chla_2018_08 <- as.numeric(chla_2018_08)
chla_2018_08 <- mean(chla_2018_08)

chla_2018_09 <- chla1 %>% filter(time == "2018-09-16T00:00:00Z" & !is.na(chlorophyll))
chla_2018_09 <- chla_2018_09$chlorophyll
chla_2018_09 <- as.numeric(chla_2018_09)
chla_2018_09 <- mean(chla_2018_09)

chla_2018_10 <- chla1 %>% filter(time == "2018-10-16T00:00:00Z" & !is.na(chlorophyll))
chla_2018_10 <- chla_2018_10$chlorophyll
chla_2018_10 <- as.numeric(chla_2018_10)
chla_2018_10 <- mean(chla_2018_10)

chla_2018_11 <- chla1 %>% filter(time == "2018-11-16T00:00:00Z" & !is.na(chlorophyll))
chla_2018_11 <- chla_2018_11$chlorophyll
chla_2018_11 <- as.numeric(chla_2018_11)
chla_2018_11 <- mean(chla_2018_11)

chla_2018 <- c(chla_2018_01, chla_2018_02, chla_2018_03, chla_2018_04, chla_2018_05, chla_2018_06, chla_2018_07, chla_2018_08, chla_2018_09, chla_2018_10, chla_2018_11, NA)
chla_2018 <- as.numeric(chla_2018)

#2019
chla_2019_01 <- chla1 %>% filter(time == "2019-01-16T00:00:00Z" & !is.na(chlorophyll))
chla_2019_01 <- chla_2019_01$chlorophyll
chla_2019_01 <- as.numeric(chla_2019_01)
chla_2019_01 <- mean(chla_2019_01)

chla_2019_02 <- chla1 %>% filter(time == "2019-02-16T00:00:00Z" & !is.na(chlorophyll))
chla_2019_02 <- chla_2019_02$chlorophyll
chla_2019_02 <- as.numeric(chla_2019_02)
chla_2019_02 <- mean(chla_2019_02)

chla_2019_03 <- chla1 %>% filter(time == "2019-03-16T00:00:00Z" & !is.na(chlorophyll))
chla_2019_03 <- chla_2019_03$chlorophyll
chla_2019_03 <- as.numeric(chla_2019_03)
chla_2019_03 <- mean(chla_2019_03)

chla_2019_04 <- chla1 %>% filter(time == "2019-04-16T00:00:00Z" & !is.na(chlorophyll))
chla_2019_04 <- chla_2019_04$chlorophyll
chla_2019_04 <- as.numeric(chla_2019_04)
chla_2019_04 <- mean(chla_2019_04)

chla_2019_05 <- chla1 %>% filter(time == "2019-05-16T00:00:00Z" & !is.na(chlorophyll))
chla_2019_05 <- chla_2019_05$chlorophyll
chla_2019_05 <- as.numeric(chla_2019_05)
chla_2019_05 <- mean(chla_2019_05)

chla_2019_06 <- chla1 %>% filter(time == "2019-06-16T00:00:00Z" & !is.na(chlorophyll))
chla_2019_06 <- chla_2019_06$chlorophyll
chla_2019_06 <- as.numeric(chla_2019_06)
chla_2019_06 <- mean(chla_2019_06)

chla_2019_07 <- chla1 %>% filter(time == "2019-07-16T00:00:00Z" & !is.na(chlorophyll))
chla_2019_07 <- chla_2019_07$chlorophyll
chla_2019_07 <- as.numeric(chla_2019_07)
chla_2019_07 <- mean(chla_2019_07)

chla_2019_08 <- chla1 %>% filter(time == "2019-08-16T00:00:00Z" & !is.na(chlorophyll))
chla_2019_08 <- chla_2019_08$chlorophyll
chla_2019_08 <- as.numeric(chla_2019_08)
chla_2019_08 <- mean(chla_2019_08)

chla_2019_09 <- chla1 %>% filter(time == "2019-09-16T00:00:00Z" & !is.na(chlorophyll))
chla_2019_09 <- chla_2019_09$chlorophyll
chla_2019_09 <- as.numeric(chla_2019_09)
chla_2019_09 <- mean(chla_2019_09)

chla_2019_10 <- chla1 %>% filter(time == "2019-10-16T00:00:00Z" & !is.na(chlorophyll))
chla_2019_10 <- chla_2019_10$chlorophyll
chla_2019_10 <- as.numeric(chla_2019_10)
chla_2019_10 <- mean(chla_2019_10)

chla_2019_11 <- chla1 %>% filter(time == "2019-11-16T00:00:00Z" & !is.na(chlorophyll))
chla_2019_11 <- chla_2019_11$chlorophyll
chla_2019_11 <- as.numeric(chla_2019_11)
chla_2019_11 <- mean(chla_2019_11)

chla_2019 <- c(chla_2019_01, chla_2019_02, chla_2019_03, chla_2019_04, chla_2019_05, chla_2019_06, chla_2019_07, chla_2019_08, chla_2019_09, chla_2019_10, chla_2019_11, NA)
chla_2019 <- as.numeric(chla_2019)

#2020
chla_2020_01 <- chla1 %>% filter(time == "2020-01-16T00:00:00Z" & !is.na(chlorophyll))
chla_2020_01 <- chla_2020_01$chlorophyll
chla_2020_01 <- as.numeric(chla_2020_01)
chla_2020_01 <- mean(chla_2020_01)

chla_2020_02 <- chla1 %>% filter(time == "2020-02-16T00:00:00Z" & !is.na(chlorophyll))
chla_2020_02 <- chla_2020_02$chlorophyll
chla_2020_02 <- as.numeric(chla_2020_02)
chla_2020_02 <- mean(chla_2020_02)

chla_2020_03 <- chla1 %>% filter(time == "2020-03-16T00:00:00Z" & !is.na(chlorophyll))
chla_2020_03 <- chla_2020_03$chlorophyll
chla_2020_03 <- as.numeric(chla_2020_03)
chla_2020_03 <- mean(chla_2020_03)

chla_2020_04 <- chla1 %>% filter(time == "2020-04-16T00:00:00Z" & !is.na(chlorophyll))
chla_2020_04 <- chla_2020_04$chlorophyll
chla_2020_04 <- as.numeric(chla_2020_04)
chla_2020_04 <- mean(chla_2020_04)

chla_2020_05 <- chla1 %>% filter(time == "2020-05-16T00:00:00Z" & !is.na(chlorophyll))
chla_2020_05 <- chla_2020_05$chlorophyll
chla_2020_05 <- as.numeric(chla_2020_05)
chla_2020_05 <- mean(chla_2020_05)

chla_2020_06 <- chla1 %>% filter(time == "2020-06-16T00:00:00Z" & !is.na(chlorophyll))
chla_2020_06 <- chla_2020_06$chlorophyll
chla_2020_06 <- as.numeric(chla_2020_06)
chla_2020_06 <- mean(chla_2020_06)

chla_2020_07 <- chla1 %>% filter(time == "2020-07-16T00:00:00Z" & !is.na(chlorophyll))
chla_2020_07 <- chla_2020_07$chlorophyll
chla_2020_07 <- as.numeric(chla_2020_07)
chla_2020_07 <- mean(chla_2020_07)

chla_2020_08 <- chla1 %>% filter(time == "2020-08-16T00:00:00Z" & !is.na(chlorophyll))
chla_2020_08 <- chla_2020_08$chlorophyll
chla_2020_08 <- as.numeric(chla_2020_08)
chla_2020_08 <- mean(chla_2020_08)

chla_2020_09 <- chla1 %>% filter(time == "2020-09-16T00:00:00Z" & !is.na(chlorophyll))
chla_2020_09 <- chla_2020_09$chlorophyll
chla_2020_09 <- as.numeric(chla_2020_09)
chla_2020_09 <- mean(chla_2020_09)

chla_2020_10 <- chla1 %>% filter(time == "2020-10-16T00:00:00Z" & !is.na(chlorophyll))
chla_2020_10 <- chla_2020_10$chlorophyll
chla_2020_10 <- as.numeric(chla_2020_10)
chla_2020_10 <- mean(chla_2020_10)

chla_2020_11 <- chla1 %>% filter(time == "2020-11-16T00:00:00Z" & !is.na(chlorophyll))
chla_2020_11 <- chla_2020_11$chlorophyll
chla_2020_11 <- as.numeric(chla_2020_11)
chla_2020_11 <- mean(chla_2020_11)

chla_2020 <- c(chla_2020_01, chla_2020_02, chla_2020_03, chla_2020_04, chla_2020_05, chla_2020_06, chla_2020_07, chla_2020_08, chla_2020_09, chla_2020_10, chla_2020_11, NA)
chla_2020 <- as.numeric(chla_2020)

#2021
chla_2021_01 <- chla1 %>% filter(time == "2021-01-16T00:00:00Z" & !is.na(chlorophyll))
chla_2021_01 <- chla_2021_01$chlorophyll
chla_2021_01 <- as.numeric(chla_2021_01)
chla_2021_01 <- mean(chla_2021_01)

chla_2021_02 <- chla1 %>% filter(time == "2021-02-16T00:00:00Z" & !is.na(chlorophyll))
chla_2021_02 <- chla_2021_02$chlorophyll
chla_2021_02 <- as.numeric(chla_2021_02)
chla_2021_02 <- mean(chla_2021_02)

chla_2021_03 <- chla1 %>% filter(time == "2021-03-16T00:00:00Z" & !is.na(chlorophyll))
chla_2021_03 <- chla_2021_03$chlorophyll
chla_2021_03 <- as.numeric(chla_2021_03)
chla_2021_03 <- mean(chla_2021_03)

chla_2021_04 <- chla1 %>% filter(time == "2021-04-16T00:00:00Z" & !is.na(chlorophyll))
chla_2021_04 <- chla_2021_04$chlorophyll
chla_2021_04 <- as.numeric(chla_2021_04)
chla_2021_04 <- mean(chla_2021_04)

chla_2021_05 <- chla1 %>% filter(time == "2021-05-16T00:00:00Z" & !is.na(chlorophyll))
chla_2021_05 <- chla_2021_05$chlorophyll
chla_2021_05 <- as.numeric(chla_2021_05)
chla_2021_05 <- mean(chla_2021_05)

chla_2021_06 <- chla1 %>% filter(time == "2021-06-16T00:00:00Z" & !is.na(chlorophyll))
chla_2021_06 <- chla_2021_06$chlorophyll
chla_2021_06 <- as.numeric(chla_2021_06)
chla_2021_06 <- mean(chla_2021_06)

chla_2021_07 <- chla1 %>% filter(time == "2021-07-16T00:00:00Z" & !is.na(chlorophyll))
chla_2021_07 <- chla_2021_07$chlorophyll
chla_2021_07 <- as.numeric(chla_2021_07)
chla_2021_07 <- mean(chla_2021_07)

chla_2021_08 <- chla1 %>% filter(time == "2021-08-16T00:00:00Z" & !is.na(chlorophyll))
chla_2021_08 <- chla_2021_08$chlorophyll
chla_2021_08 <- as.numeric(chla_2021_08)
chla_2021_08 <- mean(chla_2021_08)

chla_2021_09 <- chla1 %>% filter(time == "2021-09-16T00:00:00Z" & !is.na(chlorophyll))
chla_2021_09 <- chla_2021_09$chlorophyll
chla_2021_09 <- as.numeric(chla_2021_09)
chla_2021_09 <- mean(chla_2021_09)

chla_2021_10 <- chla1 %>% filter(time == "2021-10-16T00:00:00Z" & !is.na(chlorophyll))
chla_2021_10 <- chla_2021_10$chlorophyll
chla_2021_10 <- as.numeric(chla_2021_10)
chla_2021_10 <- mean(chla_2021_10)

chla_2021_11 <- chla1 %>% filter(time == "2021-11-16T00:00:00Z" & !is.na(chlorophyll))
chla_2021_11 <- chla_2021_11$chlorophyll
chla_2021_11 <- as.numeric(chla_2021_11)
chla_2021_11 <- mean(chla_2021_11)

chla_2021 <- c(chla_2021_01, chla_2021_02, chla_2021_03, chla_2021_04, chla_2021_05, chla_2021_06, chla_2021_07, chla_2021_08, chla_2021_09, chla_2021_10, chla_2021_11, NA)
chla_2021 <- as.numeric(chla_2021)

#2022
chla_2022_01 <- chla1 %>% filter(time == "2022-01-16T00:00:00Z" & !is.na(chlorophyll))
chla_2022_01 <- chla_2022_01$chlorophyll
chla_2022_01 <- as.numeric(chla_2022_01)
chla_2022_01 <- mean(chla_2022_01)

chla_2022_02 <- chla1 %>% filter(time == "2022-02-16T00:00:00Z" & !is.na(chlorophyll))
chla_2022_02 <- chla_2022_02$chlorophyll
chla_2022_02 <- as.numeric(chla_2022_02)
chla_2022_02 <- mean(chla_2022_02)

chla_2022_03 <- chla1 %>% filter(time == "2022-03-16T00:00:00Z" & !is.na(chlorophyll))
chla_2022_03 <- chla_2022_03$chlorophyll
chla_2022_03 <- as.numeric(chla_2022_03)
chla_2022_03 <- mean(chla_2022_03)

chla_2022_04 <- chla1 %>% filter(time == "2022-04-16T00:00:00Z" & !is.na(chlorophyll))
chla_2022_04 <- chla_2022_04$chlorophyll
chla_2022_04 <- as.numeric(chla_2022_04)
chla_2022_04 <- mean(chla_2022_04)

chla_2022_05 <- chla1 %>% filter(time == "2022-05-16T00:00:00Z" & !is.na(chlorophyll))
chla_2022_05 <- chla_2022_05$chlorophyll
chla_2022_05 <- as.numeric(chla_2022_05)
chla_2022_05 <- mean(chla_2022_05)

chla_2022 <- c(chla_2022_01, chla_2022_02, chla_2022_03, chla_2022_04, chla_2022_05, NA, NA, NA, NA, NA, NA, NA)
chla_2022 <- as.numeric(chla_2022)

#####
#Bind vectors
chla_years <- rbind(chla_1997, chla_1998, chla_1999, chla_2000, chla_2001, chla_2002, chla_2003, chla_2004, chla_2005, chla_2006, chla_2007, chla_2008, chla_2009, chla_2010, chla_2011, chla_2012, chla_2013, chla_2014, chla_2015, chla_2016, chla_2017, chla_2018, chla_2019, chla_2020, chla_2021, chla_2022)

#Row names
chla_row_head <- seq(1997, 2022, 1)
chla <- cbind(chla_row_head, chla_years)
rownames(chla) <- seq(1, 26, 1)

#Column names
chla_col_head <- c("Year", "Jan", "Feb", "Mar", "Apr", "May", "June", "Jul", "Aug", "Sept", "Oct", "Nov", "Dec")
colnames(chla) <- chla_col_head
chla <- as.data.frame(chla)

###############################################################################
#chla full-year; May(t-1)-April(t)
full98 <- cbind(chla[1,6:13], chla[2,2:5])
full99 <- cbind(chla[2,6:13], chla[3,2:5])
full00 <- cbind(chla[3,6:13], chla[4,2:5])
full01 <- cbind(chla[4,6:13], chla[5,2:5])
full02 <- cbind(chla[5,6:13], chla[6,2:5])
full03 <- cbind(chla[6,6:13], chla[7,2:5])
full04 <- cbind(chla[7,6:13], chla[8,2:5])
full05 <- cbind(chla[8,6:13], chla[9,2:5])
full06 <- cbind(chla[9,6:13], chla[10,2:5])
full07 <- cbind(chla[10,6:13], chla[11,2:5])
full08 <- cbind(chla[11,6:13], chla[12,2:5])
full09 <- cbind(chla[12,6:13], chla[13,2:5])
full10 <- cbind(chla[13,6:13], chla[14,2:5])
full11 <- cbind(chla[14,6:13], chla[15,2:5])
full12 <- cbind(chla[15,6:13], chla[16,2:5])
full13 <- cbind(chla[16,6:13], chla[17,2:5])
full14 <- cbind(chla[17,6:13], chla[18,2:5])
full15 <- cbind(chla[18,6:13], chla[19,2:5])
full16 <- cbind(chla[19,6:13], chla[20,2:5])
full17 <- cbind(chla[20,6:13], chla[21,2:5])
full18 <- cbind(chla[21,6:13], chla[22,2:5])
full19 <- cbind(chla[22,6:13], chla[23,2:5])
full20 <- cbind(chla[23,6:13], chla[24,2:5])
full21 <- cbind(chla[24,6:13], chla[25,2:5])
full22 <- cbind(chla[25,6:13], chla[26,2:5])


pi_year_rnames <- seq(1998, 2022, 1)
year_1_chla <- rbind(full98, full99, full00, full01, full02, full03, full04, full05, full06, full07, full08, full09, full10, full11, full12, full13, full14, full15, full16, full17, full18, full19, full20, full21, full22)
year_1_chla <- cbind(pi_year_rnames, year_1_chla)
chla_holder <- rep(NA, nrow(year_1_chla))
year_1_chla <- cbind(year_1_chla, chla_holder)


year_1_chla$chla_holder[1:25] <- rowMeans(year_1_chla[,2:13], na.rm=TRUE)
year_1_chla <- year_1_chla[,c(1,14)]
colnames(year_1_chla) <- c("Year", "chla")

#chla full-year; Oct(t-1)-Sept(t)
full.2.98 <- cbind(chla[1,11:13], chla[2,2:10])
full.2.99 <- cbind(chla[2,11:13], chla[3,2:10])
full.2.00 <- cbind(chla[3,11:13], chla[4,2:10])
full.2.01 <- cbind(chla[4,11:13], chla[5,2:10])
full.2.02 <- cbind(chla[5,11:13], chla[6,2:10])
full.2.03 <- cbind(chla[6,11:13], chla[7,2:10])
full.2.04 <- cbind(chla[7,11:13], chla[8,2:10])
full.2.05 <- cbind(chla[8,11:13], chla[9,2:10])
full.2.06 <- cbind(chla[9,11:13], chla[10,2:10])
full.2.07 <- cbind(chla[10,11:13], chla[11,2:10])
full.2.08 <- cbind(chla[11,11:13], chla[12,2:10])
full.2.09 <- cbind(chla[12,11:13], chla[13,2:10])
full.2.10 <- cbind(chla[13,11:13], chla[14,2:10])
full.2.11 <- cbind(chla[14,11:13], chla[15,2:10])
full.2.12 <- cbind(chla[15,11:13], chla[16,2:10])
full.2.13 <- cbind(chla[16,11:13], chla[17,2:10])
full.2.14 <- cbind(chla[17,11:13], chla[18,2:10])
full.2.15 <- cbind(chla[18,11:13], chla[19,2:10])
full.2.16 <- cbind(chla[19,11:13], chla[20,2:10])
full.2.17 <- cbind(chla[20,11:13], chla[21,2:10])
full.2.18 <- cbind(chla[21,11:13], chla[22,2:10])
full.2.19 <- cbind(chla[22,11:13], chla[23,2:10])
full.2.20 <- cbind(chla[23,11:13], chla[24,2:10])
full.2.21 <- cbind(chla[24,11:13], chla[25,2:10])
full.2.22 <- cbind(chla[25,11:13], chla[26,2:10])

year_2_chla <- rbind(full.2.98, full.2.99, full.2.00, full.2.01, full.2.02, full.2.03, full.2.04, full.2.05, full.2.06, full.2.07, full.2.08, full.2.09, full.2.10, full.2.11, full.2.12, full.2.13, full.2.14, full.2.15, full.2.16, full.2.17, full.2.18, full.2.19, full.2.20, full.2.21, full.2.22)
year_2_chla <- cbind(pi_year_rnames, year_2_chla)
year_2_chla <- cbind(year_2_chla, chla_holder)

year_2_chla$chla_holder[1:25] <- rowMeans(year_2_chla[,2:13], na.rm=TRUE)
year_2_chla <- year_2_chla[,c(1,14)]
colnames(year_2_chla) <- c("Year", "chla")

####
#chla pre-breeding season; Jan-Apr
chla_holder <- rep(NA, dim(chla)[1])
pre_chla <- cbind(chla[,1:5], chla_holder)
pre_chla$chla_holder <- rowMeans(pre_chla[,2:5], na.rm=TRUE)
pre_chla <- pre_chla[,c(1,6)]
colnames(pre_chla) <- c("Year", "chla")

####
#chla breeding season; May - Sept
breed_chla <- cbind(chla[,c(1,6:10)], chla_holder)
breed_chla$chla_holder <- rowMeans(breed_chla[,2:6], na.rm=TRUE)
breed_chla <- breed_chla[,c(1,7)]
colnames(breed_chla) <- c("Year", "chla")

####
#winter; Oct-Mar
winter97 <- cbind(chla[1,11:13], chla[2,2:4])
winter98 <- cbind(chla[2,11:13], chla[3,2:4])
winter99 <- cbind(chla[3,11:13], chla[4,2:4])
winter00 <- cbind(chla[4,11:13], chla[5,2:4])
winter01 <- cbind(chla[5,11:13], chla[6,2:4])
winter02 <- cbind(chla[6,11:13], chla[7,2:4])
winter03 <- cbind(chla[7,11:13], chla[8,2:4])
winter04 <- cbind(chla[8,11:13], chla[9,2:4])
winter05 <- cbind(chla[9,11:13], chla[10,2:4])
winter06 <- cbind(chla[10,11:13], chla[11,2:4])
winter07 <- cbind(chla[11,11:13], chla[12,2:4])
winter08 <- cbind(chla[12,11:13], chla[13,2:4])
winter09 <- cbind(chla[13,11:13], chla[14,2:4])
winter10 <- cbind(chla[14,11:13], chla[15,2:4])
winter11 <- cbind(chla[15,11:13], chla[16,2:4])
winter12 <- cbind(chla[16,11:13], chla[17,2:4])
winter13 <- cbind(chla[17,11:13], chla[18,2:4])
winter14 <- cbind(chla[18,11:13], chla[19,2:4])
winter15 <- cbind(chla[19,11:13], chla[20,2:4])
winter16 <- cbind(chla[20,11:13], chla[21,2:4])
winter17 <- cbind(chla[21,11:13], chla[22,2:4])
winter18 <- cbind(chla[22,11:13], chla[23,2:4])
winter19 <- cbind(chla[23,11:13], chla[24,2:4])
winter20 <- cbind(chla[24,11:13], chla[25,2:4])
winter21 <- cbind(chla[25,11:13], chla[26,2:4])
winter22 <- cbind(chla[26,11:13], chla[27,2:4])

pi_winter_rnames <- seq(1997, 2022, 1)
winter_chla <- rbind(winter97, winter98, winter99, winter00, winter01, winter02, winter03, winter04, winter05, winter06, winter07, winter08, winter09, winter10, winter11, winter12, winter13, winter14, winter15, winter16, winter17, winter18, winter19, winter20, winter21, winter22)
winter_chla <- cbind(pi_winter_rnames, winter_chla)
winter_chla <- cbind(winter_chla, chla_holder)

winter_chla$chla_holder <- rowMeans(winter_chla[,2:7], na.rm=TRUE)
winter_chla <- winter_chla[,c(1,8)]
colnames(winter_chla) <- c("Year", "chla")

####
#clean things up in environment
# rm(chla)
rm(pi_full_chla)
rm(winter96)
rm(winter97)
rm(winter98)
rm(winter99)
rm(winter00)
rm(winter01)
rm(winter02)
rm(winter03)
rm(winter04)
rm(winter05)
rm(winter06)
rm(winter07)
rm(winter08)
rm(winter09)
rm(winter10)
rm(winter11)
rm(winter12)
rm(winter13)
rm(winter14)
rm(winter15)
rm(winter16)
rm(winter17)
rm(winter18)
rm(winter19)
rm(winter20)
rm(winter21)
rm(winter22)
rm(winter23)
rm(chla_holder)
rm(pi_winter_rnames)
rm(full96)
rm(full97)
rm(full98)
rm(full99)
rm(full00)
rm(full01)
rm(full02)
rm(full03)
rm(full04)
rm(full05)
rm(full06)
rm(full07)
rm(full08)
rm(full09)
rm(full10)
rm(full11)
rm(full12)
rm(full13)
rm(full14)
rm(full15)
rm(full16)
rm(full17)
rm(full18)
rm(full19)
rm(full20)
rm(full21)
rm(full22)
rm(full23)
rm(pi_full_rnames)
rm(full.2.96)
rm(full.2.97)
rm(full.2.98)
rm(full.2.99)
rm(full.2.00)
rm(full.2.01)
rm(full.2.02)
rm(full.2.03)
rm(full.2.04)
rm(full.2.05)
rm(full.2.06)
rm(full.2.07)
rm(full.2.08)
rm(full.2.09)
rm(full.2.10)
rm(full.2.11)
rm(full.2.12)
rm(full.2.13)
rm(full.2.14)
rm(full.2.15)
rm(full.2.16)
rm(full.2.17)
rm(full.2.18)
rm(full.2.19)
rm(full.2.20)
rm(full.2.21)
rm(full.2.22)
rm(full.2.23)
rm(pi_year_rnames)
rm(chla_1997_01)
rm(chla_1997_02)
rm(chla_1997_03)
rm(chla_1997_04)
rm(chla_1997_05)
rm(chla_1997_06)
rm(chla_1997_07)
rm(chla_1997_08)
rm(chla_1997_09)
rm(chla_1997_10)
rm(chla_1997_11)
rm(chla_1998_01)
rm(chla_1998_02)
rm(chla_1998_03)
rm(chla_1998_04)
rm(chla_1998_05)
rm(chla_1998_06)
rm(chla_1998_07)
rm(chla_1998_08)
rm(chla_1998_09)
rm(chla_1998_10)
rm(chla_1998_11)
rm(chla_1999_01)
rm(chla_1999_02)
rm(chla_1999_03)
rm(chla_1999_04)
rm(chla_1999_05)
rm(chla_1999_06)
rm(chla_1999_07)
rm(chla_1999_08)
rm(chla_1999_09)
rm(chla_1999_10)
rm(chla_1999_11)
rm(chla_2000_01)
rm(chla_2000_02)
rm(chla_2000_03)
rm(chla_2000_04)
rm(chla_2000_05)
rm(chla_2000_06)
rm(chla_2000_07)
rm(chla_2000_08)
rm(chla_2000_09)
rm(chla_2000_10)
rm(chla_2000_11)
rm(chla_2001_01)
rm(chla_2001_02)
rm(chla_2001_03)
rm(chla_2001_04)
rm(chla_2001_05)
rm(chla_2001_06)
rm(chla_2001_07)
rm(chla_2001_08)
rm(chla_2001_09)
rm(chla_2001_10)
rm(chla_2001_11)
rm(chla_2002_01)
rm(chla_2002_02)
rm(chla_2002_03)
rm(chla_2002_04)
rm(chla_2002_05)
rm(chla_2002_06)
rm(chla_2002_07)
rm(chla_2002_08)
rm(chla_2002_09)
rm(chla_2002_10)
rm(chla_2002_11)
rm(chla_2003_01)
rm(chla_2003_02)
rm(chla_2003_03)
rm(chla_2003_04)
rm(chla_2003_05)
rm(chla_2003_06)
rm(chla_2003_07)
rm(chla_2003_08)
rm(chla_2003_09)
rm(chla_2003_10)
rm(chla_2003_11)
rm(chla_2004_01)
rm(chla_2004_02)
rm(chla_2004_03)
rm(chla_2004_04)
rm(chla_2004_05)
rm(chla_2004_06)
rm(chla_2004_07)
rm(chla_2004_08)
rm(chla_2004_09)
rm(chla_2004_10)
rm(chla_2004_11)
rm(chla_2005_01)
rm(chla_2005_02)
rm(chla_2005_03)
rm(chla_2005_04)
rm(chla_2005_05)
rm(chla_2005_06)
rm(chla_2005_07)
rm(chla_2005_08)
rm(chla_2005_09)
rm(chla_2005_10)
rm(chla_2005_11)
rm(chla_2006_01)
rm(chla_2006_02)
rm(chla_2006_03)
rm(chla_2006_04)
rm(chla_2006_05)
rm(chla_2006_06)
rm(chla_2006_07)
rm(chla_2006_08)
rm(chla_2006_09)
rm(chla_2006_10)
rm(chla_2006_11)
rm(chla_2007_01)
rm(chla_2007_02)
rm(chla_2007_03)
rm(chla_2007_04)
rm(chla_2007_05)
rm(chla_2007_06)
rm(chla_2007_07)
rm(chla_2007_08)
rm(chla_2007_09)
rm(chla_2007_10)
rm(chla_2007_11)
rm(chla_2008_01)
rm(chla_2008_02)
rm(chla_2008_03)
rm(chla_2008_04)
rm(chla_2008_05)
rm(chla_2008_06)
rm(chla_2008_07)
rm(chla_2008_08)
rm(chla_2008_09)
rm(chla_2008_10)
rm(chla_2008_11)
rm(chla_2009_01)
rm(chla_2009_02)
rm(chla_2009_03)
rm(chla_2009_04)
rm(chla_2009_05)
rm(chla_2009_06)
rm(chla_2009_07)
rm(chla_2009_08)
rm(chla_2009_09)
rm(chla_2009_10)
rm(chla_2009_11)
rm(chla_2010_01)
rm(chla_2010_02)
rm(chla_2010_03)
rm(chla_2010_04)
rm(chla_2010_05)
rm(chla_2010_06)
rm(chla_2010_07)
rm(chla_2010_08)
rm(chla_2010_09)
rm(chla_2010_10)
rm(chla_2010_11)
rm(chla_2011_01)
rm(chla_2011_02)
rm(chla_2011_03)
rm(chla_2011_04)
rm(chla_2011_05)
rm(chla_2011_06)
rm(chla_2011_07)
rm(chla_2011_08)
rm(chla_2011_09)
rm(chla_2011_10)
rm(chla_2011_11)
rm(chla_2012_01)
rm(chla_2012_02)
rm(chla_2012_03)
rm(chla_2012_04)
rm(chla_2012_05)
rm(chla_2012_06)
rm(chla_2012_07)
rm(chla_2012_08)
rm(chla_2012_09)
rm(chla_2012_10)
rm(chla_2012_11)
rm(chla_2013_01)
rm(chla_2013_02)
rm(chla_2013_03)
rm(chla_2013_04)
rm(chla_2013_05)
rm(chla_2013_06)
rm(chla_2013_07)
rm(chla_2013_08)
rm(chla_2013_09)
rm(chla_2013_10)
rm(chla_2013_11)
rm(chla_2014_01)
rm(chla_2014_02)
rm(chla_2014_03)
rm(chla_2014_04)
rm(chla_2014_05)
rm(chla_2014_06)
rm(chla_2014_07)
rm(chla_2014_08)
rm(chla_2014_09)
rm(chla_2014_10)
rm(chla_2014_11)
rm(chla_2015_01)
rm(chla_2015_02)
rm(chla_2015_03)
rm(chla_2015_04)
rm(chla_2015_05)
rm(chla_2015_06)
rm(chla_2015_07)
rm(chla_2015_08)
rm(chla_2015_09)
rm(chla_2015_10)
rm(chla_2015_11)
rm(chla_2016_01)
rm(chla_2016_02)
rm(chla_2016_03)
rm(chla_2016_04)
rm(chla_2016_05)
rm(chla_2016_06)
rm(chla_2016_07)
rm(chla_2016_08)
rm(chla_2016_09)
rm(chla_2016_10)
rm(chla_2016_11)
rm(chla_2017_01)
rm(chla_2017_02)
rm(chla_2017_03)
rm(chla_2017_04)
rm(chla_2017_05)
rm(chla_2017_06)
rm(chla_2017_07)
rm(chla_2017_08)
rm(chla_2017_09)
rm(chla_2017_10)
rm(chla_2017_11)
rm(chla_2018_01)
rm(chla_2018_02)
rm(chla_2018_03)
rm(chla_2018_04)
rm(chla_2018_05)
rm(chla_2018_06)
rm(chla_2018_07)
rm(chla_2018_08)
rm(chla_2018_09)
rm(chla_2018_10)
rm(chla_2018_11)
rm(chla_2019_01)
rm(chla_2019_02)
rm(chla_2019_03)
rm(chla_2019_04)
rm(chla_2019_05)
rm(chla_2019_06)
rm(chla_2019_07)
rm(chla_2019_08)
rm(chla_2019_09)
rm(chla_2019_10)
rm(chla_2019_11)
rm(chla_2020_01)
rm(chla_2020_02)
rm(chla_2020_03)
rm(chla_2020_04)
rm(chla_2020_05)
rm(chla_2020_06)
rm(chla_2020_07)
rm(chla_2020_08)
rm(chla_2020_09)
rm(chla_2020_10)
rm(chla_2020_11)
rm(chla_2021_01)
rm(chla_2021_02)
rm(chla_2021_03)
rm(chla_2021_04)
rm(chla_2021_05)
rm(chla_2021_06)
rm(chla_2021_07)
rm(chla_2021_08)
rm(chla_2021_09)
rm(chla_2021_10)
rm(chla_2021_11)
rm(chla_2022_01)
rm(chla_2022_02)
rm(chla_2022_03)
rm(chla_2022_04)
rm(chla_2022_05)
rm(chla_2022_06)
rm(chla_2022_07)
rm(chla_2022_08)
rm(chla_2022_09)
rm(chla_2022_10)
rm(chla_2022_11)
rm(chla_1997)
rm(chla_1998)
rm(chla_1999)
rm(chla_2000)
rm(chla_2001)
rm(chla_2002)
rm(chla_2003)
rm(chla_2004)
rm(chla_2005)
rm(chla_2006)
rm(chla_2007)
rm(chla_2008)
rm(chla_2009)
rm(chla_2010)
rm(chla_2011)
rm(chla_2012)
rm(chla_2013)
rm(chla_2014)
rm(chla_2015)
rm(chla_2016)
rm(chla_2017)
rm(chla_2018)
rm(chla_2019)
rm(chla_2020)
rm(chla_2021)
rm(chla_2022)
rm(chla_years)
rm(chla1)
rm(chla2)
rm(jan)
rm(jan.vec)
rm(jan.vec.2)
rm(feb)
rm(feb.vec)
rm(feb.vec.2)
rm(mar)
rm(mar.vec)
rm(mar.vec.2)
rm(apr)
rm(apr.vec)
rm(apr.vec.2)
rm(may)
rm(may.vec)
rm(may.vec.2)
rm(jun)
rm(jun.vec)
rm(jun.vec.2)
rm(jul)
rm(jul.vec)
rm(jul.vec.2)
rm(aug)
rm(aug.vec)
rm(aug.vec.2)
rm(sep)
rm(sep.vec)
rm(sep.vec.2)
rm(oct)
rm(oct.vec)
rm(oct.vec.2)
rm(nov)
rm(nov.vec)
rm(nov.vec.2)
rm(chla_col_head)
rm(chla_row_head)
rm(i)