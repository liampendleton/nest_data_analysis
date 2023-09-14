library(here)
library(tidyverse)

#####
#ChlA
chla1 <- read.csv(here("Data", "erdMH1chlamday_ff42_85c8_cd0c.csv"))
chla2 <- read.csv(here("Data", "erdSW2018chlamday_29f3_6396_25da.csv"))

chla1 <- chla1[-1,] #get rid of extra headers
chla2 <- chla2[-1,]

rownames(chla1) = seq(length=nrow(chla1)) #reset row numbers
rownames(chla2) = seq(length=nrow(chla2))

#1997
chla_1997_09 <- chla2 %>% filter(time == "1997-09-16T00:00:00Z" & chlorophyll != NaN) #Isolate data from specific year and month
chla_1997_09 <- chla_1997_09$chlorophyll #Isolate chlorophyll data
chla_1997_09 <- as.numeric(chla_1997_09) #Convert to numeric
chla_1997_09 <- mean(chla_1997_09) #Get monthly average

chla_1997_10 <- chla2 %>% filter(time == "1997-10-16T00:00:00Z" & chlorophyll != NaN)
chla_1997_10 <- chla_1997_10$chlorophyll
chla_1997_10 <- as.numeric(chla_1997_10)
chla_1997_10 <- mean(chla_1997_10)

chla_1997_11 <- chla2 %>% filter(time == "1997-11-16T00:00:00Z" & chlorophyll != NaN)
chla_1997_11 <- chla_1997_11$chlorophyll
chla_1997_11 <- as.numeric(chla_1997_11)
chla_1997_11 <- mean(chla_1997_11)

chla_1997_12 <- chla2 %>% filter(time == "1997-12-16T00:00:00Z" & chlorophyll != NaN)
chla_1997_12 <- chla_1997_12$chlorophyll
chla_1997_12 <- as.numeric(chla_1997_12)
chla_1997_12 <- mean(chla_1997_12)

chla_1997 <- c(rep(NA, 8), chla_1997_09, chla_1997_10, chla_1997_11, chla_1997_12) #Compile data; fill in leading empty slots

#1998
chla_1998_01 <- chla2 %>% filter(time == "1998-01-16T00:00:00Z" & chlorophyll != NaN)
chla_1998_01 <- chla_1998_01$chlorophyll
chla_1998_01 <- as.numeric(chla_1998_01)
chla_1998_01 <- mean(chla_1998_01)

chla_1998_02 <- chla2 %>% filter(time == "1998-02-16T00:00:00Z" & chlorophyll != NaN)
chla_1998_02 <- chla_1998_02$chlorophyll
chla_1998_02 <- as.numeric(chla_1998_02)
chla_1998_02 <- mean(chla_1998_02)

chla_1998_03 <- chla2 %>% filter(time == "1998-03-16T00:00:00Z" & chlorophyll != NaN)
chla_1998_03 <- chla_1998_03$chlorophyll
chla_1998_03 <- as.numeric(chla_1998_03)
chla_1998_03 <- mean(chla_1998_03)

chla_1998_04 <- chla2 %>% filter(time == "1998-04-16T00:00:00Z" & chlorophyll != NaN)
chla_1998_04 <- chla_1998_04$chlorophyll
chla_1998_04 <- as.numeric(chla_1998_04)
chla_1998_04 <- mean(chla_1998_04)

chla_1998_05 <- chla2 %>% filter(time == "1998-05-16T00:00:00Z" & chlorophyll != NaN)
chla_1998_05 <- chla_1998_05$chlorophyll
chla_1998_05 <- as.numeric(chla_1998_05)
chla_1998_05 <- mean(chla_1998_05)

chla_1998_06 <- chla2 %>% filter(time == "1998-06-16T00:00:00Z" & chlorophyll != NaN)
chla_1998_06 <- chla_1998_06$chlorophyll
chla_1998_06 <- as.numeric(chla_1998_06)
chla_1998_06 <- mean(chla_1998_06)

chla_1998_07 <- chla2 %>% filter(time == "1998-07-16T00:00:00Z" & chlorophyll != NaN)
chla_1998_07 <- chla_1998_07$chlorophyll
chla_1998_07 <- as.numeric(chla_1998_07)
chla_1998_07 <- mean(chla_1998_07)

chla_1998_08 <- chla2 %>% filter(time == "1998-08-16T00:00:00Z" & chlorophyll != NaN)
chla_1998_08 <- chla_1998_08$chlorophyll
chla_1998_08 <- as.numeric(chla_1998_08)
chla_1998_08 <- mean(chla_1998_08)

chla_1998_09 <- chla2 %>% filter(time == "1998-09-16T00:00:00Z" & chlorophyll != NaN)
chla_1998_09 <- chla_1998_09$chlorophyll
chla_1998_09 <- as.numeric(chla_1998_09)
chla_1998_09 <- mean(chla_1998_09)

chla_1998_10 <- chla2 %>% filter(time == "1998-10-16T00:00:00Z" & chlorophyll != NaN)
chla_1998_10 <- chla_1998_10$chlorophyll
chla_1998_10 <- as.numeric(chla_1998_10)
chla_1998_10 <- mean(chla_1998_10)

chla_1998_11 <- chla2 %>% filter(time == "1998-11-16T00:00:00Z" & chlorophyll != NaN)
chla_1998_11 <- chla_1998_11$chlorophyll
chla_1998_11 <- as.numeric(chla_1998_11)
chla_1998_11 <- mean(chla_1998_11)

chla_1998_12 <- chla2 %>% filter(time == "1998-12-16T00:00:00Z" & chlorophyll != NaN)
chla_1998_12 <- chla_1998_12$chlorophyll
chla_1998_12 <- as.numeric(chla_1998_12)
chla_1998_12 <- mean(chla_1998_12)

chla_1998 <- c(chla_1998_01, chla_1998_02, chla_1998_03, chla_1998_04, chla_1998_05, chla_1998_06, chla_1998_07, chla_1998_08, chla_1998_09, chla_1998_10, chla_1998_11, chla_1998_12)

#1999
chla_1999_01 <- chla2 %>% filter(time == "1999-01-16T00:00:00Z" & chlorophyll != NaN)
chla_1999_01 <- chla_1999_01$chlorophyll
chla_1999_01 <- as.numeric(chla_1999_01)
chla_1999_01 <- mean(chla_1999_01)

chla_1999_02 <- chla2 %>% filter(time == "1999-02-16T00:00:00Z" & chlorophyll != NaN)
chla_1999_02 <- chla_1999_02$chlorophyll
chla_1999_02 <- as.numeric(chla_1999_02)
chla_1999_02 <- mean(chla_1999_02)

chla_1999_03 <- chla2 %>% filter(time == "1999-03-16T00:00:00Z" & chlorophyll != NaN)
chla_1999_03 <- chla_1999_03$chlorophyll
chla_1999_03 <- as.numeric(chla_1999_03)
chla_1999_03 <- mean(chla_1999_03)

chla_1999_04 <- chla2 %>% filter(time == "1999-04-16T00:00:00Z" & chlorophyll != NaN)
chla_1999_04 <- chla_1999_04$chlorophyll
chla_1999_04 <- as.numeric(chla_1999_04)
chla_1999_04 <- mean(chla_1999_04)

chla_1999_05 <- chla2 %>% filter(time == "1999-05-16T00:00:00Z" & chlorophyll != NaN)
chla_1999_05 <- chla_1999_05$chlorophyll
chla_1999_05 <- as.numeric(chla_1999_05)
chla_1999_05 <- mean(chla_1999_05)

chla_1999_06 <- chla2 %>% filter(time == "1999-06-16T00:00:00Z" & chlorophyll != NaN)
chla_1999_06 <- chla_1999_06$chlorophyll
chla_1999_06 <- as.numeric(chla_1999_06)
chla_1999_06 <- mean(chla_1999_06)

chla_1999_07 <- chla2 %>% filter(time == "1999-07-16T00:00:00Z" & chlorophyll != NaN)
chla_1999_07 <- chla_1999_07$chlorophyll
chla_1999_07 <- as.numeric(chla_1999_07)
chla_1999_07 <- mean(chla_1999_07)

chla_1999_08 <- chla2 %>% filter(time == "1999-08-16T00:00:00Z" & chlorophyll != NaN)
chla_1999_08 <- chla_1999_08$chlorophyll
chla_1999_08 <- as.numeric(chla_1999_08)
chla_1999_08 <- mean(chla_1999_08)

chla_1999_09 <- chla2 %>% filter(time == "1999-09-16T00:00:00Z" & chlorophyll != NaN)
chla_1999_09 <- chla_1999_09$chlorophyll
chla_1999_09 <- as.numeric(chla_1999_09)
chla_1999_09 <- mean(chla_1999_09)

chla_1999_10 <- chla2 %>% filter(time == "1999-10-16T00:00:00Z" & chlorophyll != NaN)
chla_1999_10 <- chla_1999_10$chlorophyll
chla_1999_10 <- as.numeric(chla_1999_10)
chla_1999_10 <- mean(chla_1999_10)

chla_1999_11 <- chla2 %>% filter(time == "1999-11-16T00:00:00Z" & chlorophyll != NaN)
chla_1999_11 <- chla_1999_11$chlorophyll
chla_1999_11 <- as.numeric(chla_1999_11)
chla_1999_11 <- mean(chla_1999_11)

chla_1999_12 <- chla2 %>% filter(time == "1999-12-16T00:00:00Z" & chlorophyll != NaN)
chla_1999_12 <- chla_1999_12$chlorophyll
chla_1999_12 <- as.numeric(chla_1999_12)
chla_1999_12 <- mean(chla_1999_12)

chla_1999 <- c(chla_1999_01, chla_1999_02, chla_1999_03, chla_1999_04, chla_1999_05, chla_1999_06, chla_1999_07, chla_1999_08, chla_1999_09, chla_1999_10, chla_1999_11, chla_1999_12)

#2000
chla_2000_01 <- chla2 %>% filter(time == "2000-01-16T00:00:00Z" & chlorophyll != NaN)
chla_2000_01 <- chla_2000_01$chlorophyll
chla_2000_01 <- as.numeric(chla_2000_01)
chla_2000_01 <- mean(chla_2000_01)

chla_2000_02 <- chla2 %>% filter(time == "2000-02-16T00:00:00Z" & chlorophyll != NaN)
chla_2000_02 <- chla_2000_02$chlorophyll
chla_2000_02 <- as.numeric(chla_2000_02)
chla_2000_02 <- mean(chla_2000_02)

chla_2000_03 <- chla2 %>% filter(time == "2000-03-16T00:00:00Z" & chlorophyll != NaN)
chla_2000_03 <- chla_2000_03$chlorophyll
chla_2000_03 <- as.numeric(chla_2000_03)
chla_2000_03 <- mean(chla_2000_03)

chla_2000_04 <- chla2 %>% filter(time == "2000-04-16T00:00:00Z" & chlorophyll != NaN)
chla_2000_04 <- chla_2000_04$chlorophyll
chla_2000_04 <- as.numeric(chla_2000_04)
chla_2000_04 <- mean(chla_2000_04)

chla_2000_05 <- chla2 %>% filter(time == "2000-05-16T00:00:00Z" & chlorophyll != NaN)
chla_2000_05 <- chla_2000_05$chlorophyll
chla_2000_05 <- as.numeric(chla_2000_05)
chla_2000_05 <- mean(chla_2000_05)

chla_2000_06 <- chla2 %>% filter(time == "2000-06-16T00:00:00Z" & chlorophyll != NaN)
chla_2000_06 <- chla_2000_06$chlorophyll
chla_2000_06 <- as.numeric(chla_2000_06)
chla_2000_06 <- mean(chla_2000_06)

chla_2000_07 <- chla2 %>% filter(time == "2000-07-16T00:00:00Z" & chlorophyll != NaN)
chla_2000_07 <- chla_2000_07$chlorophyll
chla_2000_07 <- as.numeric(chla_2000_07)
chla_2000_07 <- mean(chla_2000_07)

chla_2000_08 <- chla2 %>% filter(time == "2000-08-16T00:00:00Z" & chlorophyll != NaN)
chla_2000_08 <- chla_2000_08$chlorophyll
chla_2000_08 <- as.numeric(chla_2000_08)
chla_2000_08 <- mean(chla_2000_08)

chla_2000_09 <- chla2 %>% filter(time == "2000-09-16T00:00:00Z" & chlorophyll != NaN)
chla_2000_09 <- chla_2000_09$chlorophyll
chla_2000_09 <- as.numeric(chla_2000_09)
chla_2000_09 <- mean(chla_2000_09)

chla_2000_10 <- chla2 %>% filter(time == "2000-10-16T00:00:00Z" & chlorophyll != NaN)
chla_2000_10 <- chla_2000_10$chlorophyll
chla_2000_10 <- as.numeric(chla_2000_10)
chla_2000_10 <- mean(chla_2000_10)

chla_2000_11 <- chla2 %>% filter(time == "2000-11-16T00:00:00Z" & chlorophyll != NaN)
chla_2000_11 <- chla_2000_11$chlorophyll
chla_2000_11 <- as.numeric(chla_2000_11)
chla_2000_11 <- mean(chla_2000_11)

chla_2000_12 <- chla2 %>% filter(time == "2000-12-16T00:00:00Z" & chlorophyll != NaN)
chla_2000_12 <- chla_2000_12$chlorophyll
chla_2000_12 <- as.numeric(chla_2000_12)
chla_2000_12 <- mean(chla_2000_12)

chla_2000 <- c(chla_2000_01, chla_2000_02, chla_2000_03, chla_2000_04, chla_2000_05, chla_2000_06, chla_2000_07, chla_2000_08, chla_2000_09, chla_2000_10, chla_2000_11, chla_2000_12)

#2001
chla_2001_01 <- chla2 %>% filter(time == "2001-01-16T00:00:00Z" & chlorophyll != NaN)
chla_2001_01 <- chla_2001_01$chlorophyll
chla_2001_01 <- as.numeric(chla_2001_01)
chla_2001_01 <- mean(chla_2001_01)

chla_2001_02 <- chla2 %>% filter(time == "2001-02-16T00:00:00Z" & chlorophyll != NaN)
chla_2001_02 <- chla_2001_02$chlorophyll
chla_2001_02 <- as.numeric(chla_2001_02)
chla_2001_02 <- mean(chla_2001_02)

chla_2001_03 <- chla2 %>% filter(time == "2001-03-16T00:00:00Z" & chlorophyll != NaN)
chla_2001_03 <- chla_2001_03$chlorophyll
chla_2001_03 <- as.numeric(chla_2001_03)
chla_2001_03 <- mean(chla_2001_03)

chla_2001_04 <- chla2 %>% filter(time == "2001-04-16T00:00:00Z" & chlorophyll != NaN)
chla_2001_04 <- chla_2001_04$chlorophyll
chla_2001_04 <- as.numeric(chla_2001_04)
chla_2001_04 <- mean(chla_2001_04)

chla_2001_05 <- chla2 %>% filter(time == "2001-05-16T00:00:00Z" & chlorophyll != NaN)
chla_2001_05 <- chla_2001_05$chlorophyll
chla_2001_05 <- as.numeric(chla_2001_05)
chla_2001_05 <- mean(chla_2001_05)

chla_2001_06 <- chla2 %>% filter(time == "2001-06-16T00:00:00Z" & chlorophyll != NaN)
chla_2001_06 <- chla_2001_06$chlorophyll
chla_2001_06 <- as.numeric(chla_2001_06)
chla_2001_06 <- mean(chla_2001_06)

chla_2001_07 <- chla2 %>% filter(time == "2001-07-16T00:00:00Z" & chlorophyll != NaN)
chla_2001_07 <- chla_2001_07$chlorophyll
chla_2001_07 <- as.numeric(chla_2001_07)
chla_2001_07 <- mean(chla_2001_07)

chla_2001_08 <- chla2 %>% filter(time == "2001-08-16T00:00:00Z" & chlorophyll != NaN)
chla_2001_08 <- chla_2001_08$chlorophyll
chla_2001_08 <- as.numeric(chla_2001_08)
chla_2001_08 <- mean(chla_2001_08)

chla_2001_09 <- chla2 %>% filter(time == "2001-09-16T00:00:00Z" & chlorophyll != NaN)
chla_2001_09 <- chla_2001_09$chlorophyll
chla_2001_09 <- as.numeric(chla_2001_09)
chla_2001_09 <- mean(chla_2001_09)

chla_2001_10 <- chla2 %>% filter(time == "2001-10-16T00:00:00Z" & chlorophyll != NaN)
chla_2001_10 <- chla_2001_10$chlorophyll
chla_2001_10 <- as.numeric(chla_2001_10)
chla_2001_10 <- mean(chla_2001_10)

chla_2001_11 <- chla2 %>% filter(time == "2001-11-16T00:00:00Z" & chlorophyll != NaN)
chla_2001_11 <- chla_2001_11$chlorophyll
chla_2001_11 <- as.numeric(chla_2001_11)
chla_2001_11 <- mean(chla_2001_11)

chla_2001_12 <- chla2 %>% filter(time == "2001-12-16T00:00:00Z" & chlorophyll != NaN)
chla_2001_12 <- chla_2001_12$chlorophyll
chla_2001_12 <- as.numeric(chla_2001_12)
chla_2001_12 <- mean(chla_2001_12)

chla_2001 <- c(chla_2001_01, chla_2001_02, chla_2001_03, chla_2001_04, chla_2001_05, chla_2001_06, chla_2001_07, chla_2001_08, chla_2001_09, chla_2001_10, chla_2001_11, chla_2001_12)

#2002
chla_2002_01 <- chla2 %>% filter(time == "2002-01-16T00:00:00Z" & chlorophyll != NaN)
chla_2002_01 <- chla_2002_01$chlorophyll
chla_2002_01 <- as.numeric(chla_2002_01)
chla_2002_01 <- mean(chla_2002_01)

chla_2002_02 <- chla2 %>% filter(time == "2002-02-16T00:00:00Z" & chlorophyll != NaN)
chla_2002_02 <- chla_2002_02$chlorophyll
chla_2002_02 <- as.numeric(chla_2002_02)
chla_2002_02 <- mean(chla_2002_02)

chla_2002_03 <- chla2 %>% filter(time == "2002-03-16T00:00:00Z" & chlorophyll != NaN)
chla_2002_03 <- chla_2002_03$chlorophyll
chla_2002_03 <- as.numeric(chla_2002_03)
chla_2002_03 <- mean(chla_2002_03)

chla_2002_04 <- chla2 %>% filter(time == "2002-04-16T00:00:00Z" & chlorophyll != NaN)
chla_2002_04 <- chla_2002_04$chlorophyll
chla_2002_04 <- as.numeric(chla_2002_04)
chla_2002_04 <- mean(chla_2002_04)

chla_2002_05 <- chla2 %>% filter(time == "2002-05-16T00:00:00Z" & chlorophyll != NaN)
chla_2002_05 <- chla_2002_05$chlorophyll
chla_2002_05 <- as.numeric(chla_2002_05)
chla_2002_05 <- mean(chla_2002_05)

chla_2002_06 <- chla2 %>% filter(time == "2002-06-16T00:00:00Z" & chlorophyll != NaN)
chla_2002_06 <- chla_2002_06$chlorophyll
chla_2002_06 <- as.numeric(chla_2002_06)
chla_2002_06 <- mean(chla_2002_06)

chla_2002_07 <- chla2 %>% filter(time == "2002-07-16T00:00:00Z" & chlorophyll != NaN)
chla_2002_07 <- chla_2002_07$chlorophyll
chla_2002_07 <- as.numeric(chla_2002_07)
chla_2002_07 <- mean(chla_2002_07)

chla_2002_08 <- chla2 %>% filter(time == "2002-08-16T00:00:00Z" & chlorophyll != NaN)
chla_2002_08 <- chla_2002_08$chlorophyll
chla_2002_08 <- as.numeric(chla_2002_08)
chla_2002_08 <- mean(chla_2002_08)

chla_2002_09 <- chla2 %>% filter(time == "2002-09-16T00:00:00Z" & chlorophyll != NaN)
chla_2002_09 <- chla_2002_09$chlorophyll
chla_2002_09 <- as.numeric(chla_2002_09)
chla_2002_09 <- mean(chla_2002_09)

chla_2002_10 <- chla2 %>% filter(time == "2002-10-16T00:00:00Z" & chlorophyll != NaN)
chla_2002_10 <- chla_2002_10$chlorophyll
chla_2002_10 <- as.numeric(chla_2002_10)
chla_2002_10 <- mean(chla_2002_10)

chla_2002_11 <- chla2 %>% filter(time == "2002-11-16T00:00:00Z" & chlorophyll != NaN)
chla_2002_11 <- chla_2002_11$chlorophyll
chla_2002_11 <- as.numeric(chla_2002_11)
chla_2002_11 <- mean(chla_2002_11)

chla_2002_12 <- chla2 %>% filter(time == "2002-12-16T00:00:00Z" & chlorophyll != NaN)
chla_2002_12 <- chla_2002_12$chlorophyll
chla_2002_12 <- as.numeric(chla_2002_12)
chla_2002_12 <- mean(chla_2002_12)

chla_2002 <- c(chla_2002_01, chla_2002_02, chla_2002_03, chla_2002_04, chla_2002_05, chla_2002_06, chla_2002_07, chla_2002_08, chla_2002_09, chla_2002_10, chla_2002_11, chla_2002_12)

#2003
chla_2003_01 <- chla2 %>% filter(time == "2003-01-16T00:00:00Z" & chlorophyll != NaN)
chla_2003_01 <- chla_2003_01$chlorophyll
chla_2003_01 <- as.numeric(chla_2003_01)
chla_2003_01 <- mean(chla_2003_01)

chla_2003_02 <- chla2 %>% filter(time == "2003-02-16T00:00:00Z" & chlorophyll != NaN)
chla_2003_02 <- chla_2003_02$chlorophyll
chla_2003_02 <- as.numeric(chla_2003_02)
chla_2003_02 <- mean(chla_2003_02)

chla_2003_03 <- chla2 %>% filter(time == "2003-03-16T00:00:00Z" & chlorophyll != NaN)
chla_2003_03 <- chla_2003_03$chlorophyll
chla_2003_03 <- as.numeric(chla_2003_03)
chla_2003_03 <- mean(chla_2003_03)

chla_2003_04 <- chla2 %>% filter(time == "2003-04-16T00:00:00Z" & chlorophyll != NaN)
chla_2003_04 <- chla_2003_04$chlorophyll
chla_2003_04 <- as.numeric(chla_2003_04)
chla_2003_04 <- mean(chla_2003_04)

chla_2003_05 <- chla2 %>% filter(time == "2003-05-16T00:00:00Z" & chlorophyll != NaN)
chla_2003_05 <- chla_2003_05$chlorophyll
chla_2003_05 <- as.numeric(chla_2003_05)
chla_2003_05 <- mean(chla_2003_05)

chla_2003_06 <- chla2 %>% filter(time == "2003-06-16T00:00:00Z" & chlorophyll != NaN)
chla_2003_06 <- chla_2003_06$chlorophyll
chla_2003_06 <- as.numeric(chla_2003_06)
chla_2003_06 <- mean(chla_2003_06)

chla_2003_07 <- chla2 %>% filter(time == "2003-07-16T00:00:00Z" & chlorophyll != NaN)
chla_2003_07 <- chla_2003_07$chlorophyll
chla_2003_07 <- as.numeric(chla_2003_07)
chla_2003_07 <- mean(chla_2003_07)

chla_2003_08 <- chla2 %>% filter(time == "2003-08-16T00:00:00Z" & chlorophyll != NaN)
chla_2003_08 <- chla_2003_08$chlorophyll
chla_2003_08 <- as.numeric(chla_2003_08)
chla_2003_08 <- mean(chla_2003_08)

chla_2003_09 <- chla2 %>% filter(time == "2003-09-16T00:00:00Z" & chlorophyll != NaN)
chla_2003_09 <- chla_2003_09$chlorophyll
chla_2003_09 <- as.numeric(chla_2003_09)
chla_2003_09 <- mean(chla_2003_09)

chla_2003_10 <- chla2 %>% filter(time == "2003-10-16T00:00:00Z" & chlorophyll != NaN)
chla_2003_10 <- chla_2003_10$chlorophyll
chla_2003_10 <- as.numeric(chla_2003_10)
chla_2003_10 <- mean(chla_2003_10)

chla_2003_11 <- chla2 %>% filter(time == "2003-11-16T00:00:00Z" & chlorophyll != NaN)
chla_2003_11 <- chla_2003_11$chlorophyll
chla_2003_11 <- as.numeric(chla_2003_11)
chla_2003_11 <- mean(chla_2003_11)

chla_2003_12 <- chla2 %>% filter(time == "2003-12-16T00:00:00Z" & chlorophyll != NaN)
chla_2003_12 <- chla_2003_12$chlorophyll
chla_2003_12 <- as.numeric(chla_2003_12)
chla_2003_12 <- mean(chla_2003_12)

chla_2003 <- c(chla_2003_01, chla_2003_02, chla_2003_03, chla_2003_04, chla_2003_05, chla_2003_06, chla_2003_07, chla_2003_08, chla_2003_09, chla_2003_10, chla_2003_11, chla_2003_12)

#2004
chla_2004_01 <- chla2 %>% filter(time == "2004-01-16T00:00:00Z" & chlorophyll != NaN)
chla_2004_01 <- chla_2004_01$chlorophyll
chla_2004_01 <- as.numeric(chla_2004_01)
chla_2004_01 <- mean(chla_2004_01)

chla_2004_02 <- chla2 %>% filter(time == "2004-02-16T00:00:00Z" & chlorophyll != NaN)
chla_2004_02 <- chla_2004_02$chlorophyll
chla_2004_02 <- as.numeric(chla_2004_02)
chla_2004_02 <- mean(chla_2004_02)

chla_2004_03 <- chla2 %>% filter(time == "2004-03-16T00:00:00Z" & chlorophyll != NaN)
chla_2004_03 <- chla_2004_03$chlorophyll
chla_2004_03 <- as.numeric(chla_2004_03)
chla_2004_03 <- mean(chla_2004_03)

chla_2004_04 <- chla2 %>% filter(time == "2004-04-16T00:00:00Z" & chlorophyll != NaN)
chla_2004_04 <- chla_2004_04$chlorophyll
chla_2004_04 <- as.numeric(chla_2004_04)
chla_2004_04 <- mean(chla_2004_04)

chla_2004_05 <- chla2 %>% filter(time == "2004-05-16T00:00:00Z" & chlorophyll != NaN)
chla_2004_05 <- chla_2004_05$chlorophyll
chla_2004_05 <- as.numeric(chla_2004_05)
chla_2004_05 <- mean(chla_2004_05)

chla_2004_06 <- chla2 %>% filter(time == "2004-06-16T00:00:00Z" & chlorophyll != NaN)
chla_2004_06 <- chla_2004_06$chlorophyll
chla_2004_06 <- as.numeric(chla_2004_06)
chla_2004_06 <- mean(chla_2004_06)

chla_2004_07 <- chla2 %>% filter(time == "2004-07-16T00:00:00Z" & chlorophyll != NaN)
chla_2004_07 <- chla_2004_07$chlorophyll
chla_2004_07 <- as.numeric(chla_2004_07)
chla_2004_07 <- mean(chla_2004_07)

chla_2004_08 <- chla2 %>% filter(time == "2004-08-16T00:00:00Z" & chlorophyll != NaN)
chla_2004_08 <- chla_2004_08$chlorophyll
chla_2004_08 <- as.numeric(chla_2004_08)
chla_2004_08 <- mean(chla_2004_08)

chla_2004_09 <- chla2 %>% filter(time == "2004-09-16T00:00:00Z" & chlorophyll != NaN)
chla_2004_09 <- chla_2004_09$chlorophyll
chla_2004_09 <- as.numeric(chla_2004_09)
chla_2004_09 <- mean(chla_2004_09)

chla_2004_10 <- chla2 %>% filter(time == "2004-10-16T00:00:00Z" & chlorophyll != NaN)
chla_2004_10 <- chla_2004_10$chlorophyll
chla_2004_10 <- as.numeric(chla_2004_10)
chla_2004_10 <- mean(chla_2004_10)

chla_2004_11 <- chla2 %>% filter(time == "2004-11-16T00:00:00Z" & chlorophyll != NaN)
chla_2004_11 <- chla_2004_11$chlorophyll
chla_2004_11 <- as.numeric(chla_2004_11)
chla_2004_11 <- mean(chla_2004_11)

chla_2004_12 <- chla2 %>% filter(time == "2004-12-16T00:00:00Z" & chlorophyll != NaN)
chla_2004_12 <- chla_2004_12$chlorophyll
chla_2004_12 <- as.numeric(chla_2004_12)
chla_2004_12 <- mean(chla_2004_12)

chla_2004 <- c(chla_2004_01, chla_2004_02, chla_2004_03, chla_2004_04, chla_2004_05, chla_2004_06, chla_2004_07, chla_2004_08, chla_2004_09, chla_2004_10, chla_2004_11, chla_2004_12)

#2005
chla_2005_01 <- chla2 %>% filter(time == "2005-01-16T00:00:00Z" & chlorophyll != NaN)
chla_2005_01 <- chla_2005_01$chlorophyll
chla_2005_01 <- as.numeric(chla_2005_01)
chla_2005_01 <- mean(chla_2005_01)

chla_2005_02 <- chla2 %>% filter(time == "2005-02-16T00:00:00Z" & chlorophyll != NaN)
chla_2005_02 <- chla_2005_02$chlorophyll
chla_2005_02 <- as.numeric(chla_2005_02)
chla_2005_02 <- mean(chla_2005_02)

chla_2005_03 <- chla2 %>% filter(time == "2005-03-16T00:00:00Z" & chlorophyll != NaN)
chla_2005_03 <- chla_2005_03$chlorophyll
chla_2005_03 <- as.numeric(chla_2005_03)
chla_2005_03 <- mean(chla_2005_03)

chla_2005_04 <- chla2 %>% filter(time == "2005-04-16T00:00:00Z" & chlorophyll != NaN)
chla_2005_04 <- chla_2005_04$chlorophyll
chla_2005_04 <- as.numeric(chla_2005_04)
chla_2005_04 <- mean(chla_2005_04)

chla_2005_05 <- chla2 %>% filter(time == "2005-05-16T00:00:00Z" & chlorophyll != NaN)
chla_2005_05 <- chla_2005_05$chlorophyll
chla_2005_05 <- as.numeric(chla_2005_05)
chla_2005_05 <- mean(chla_2005_05)

chla_2005_06 <- chla2 %>% filter(time == "2005-06-16T00:00:00Z" & chlorophyll != NaN)
chla_2005_06 <- chla_2005_06$chlorophyll
chla_2005_06 <- as.numeric(chla_2005_06)
chla_2005_06 <- mean(chla_2005_06)

chla_2005_07 <- chla2 %>% filter(time == "2005-07-16T00:00:00Z" & chlorophyll != NaN)
chla_2005_07 <- chla_2005_07$chlorophyll
chla_2005_07 <- as.numeric(chla_2005_07)
chla_2005_07 <- mean(chla_2005_07)

chla_2005_08 <- chla2 %>% filter(time == "2005-08-16T00:00:00Z" & chlorophyll != NaN)
chla_2005_08 <- chla_2005_08$chlorophyll
chla_2005_08 <- as.numeric(chla_2005_08)
chla_2005_08 <- mean(chla_2005_08)

chla_2005_09 <- chla2 %>% filter(time == "2005-09-16T00:00:00Z" & chlorophyll != NaN)
chla_2005_09 <- chla_2005_09$chlorophyll
chla_2005_09 <- as.numeric(chla_2005_09)
chla_2005_09 <- mean(chla_2005_09)

chla_2005_10 <- chla2 %>% filter(time == "2005-10-16T00:00:00Z" & chlorophyll != NaN)
chla_2005_10 <- chla_2005_10$chlorophyll
chla_2005_10 <- as.numeric(chla_2005_10)
chla_2005_10 <- mean(chla_2005_10)

chla_2005_11 <- chla2 %>% filter(time == "2005-11-16T00:00:00Z" & chlorophyll != NaN)
chla_2005_11 <- chla_2005_11$chlorophyll
chla_2005_11 <- as.numeric(chla_2005_11)
chla_2005_11 <- mean(chla_2005_11)

chla_2005_12 <- chla2 %>% filter(time == "2005-12-16T00:00:00Z" & chlorophyll != NaN)
chla_2005_12 <- chla_2005_12$chlorophyll
chla_2005_12 <- as.numeric(chla_2005_12)
chla_2005_12 <- mean(chla_2005_12)

chla_2005 <- c(chla_2005_01, chla_2005_02, chla_2005_03, chla_2005_04, chla_2005_05, chla_2005_06, chla_2005_07, chla_2005_08, chla_2005_09, chla_2005_10, chla_2005_11, chla_2005_12)

#2006
chla_2006_01 <- chla2 %>% filter(time == "2006-01-16T00:00:00Z" & chlorophyll != NaN)
chla_2006_01 <- chla_2006_01$chlorophyll
chla_2006_01 <- as.numeric(chla_2006_01)
chla_2006_01 <- mean(chla_2006_01)

chla_2006_02 <- chla2 %>% filter(time == "2006-02-16T00:00:00Z" & chlorophyll != NaN)
chla_2006_02 <- chla_2006_02$chlorophyll
chla_2006_02 <- as.numeric(chla_2006_02)
chla_2006_02 <- mean(chla_2006_02)

chla_2006_03 <- chla2 %>% filter(time == "2006-03-16T00:00:00Z" & chlorophyll != NaN)
chla_2006_03 <- chla_2006_03$chlorophyll
chla_2006_03 <- as.numeric(chla_2006_03)
chla_2006_03 <- mean(chla_2006_03)

chla_2006_04 <- chla2 %>% filter(time == "2006-04-16T00:00:00Z" & chlorophyll != NaN)
chla_2006_04 <- chla_2006_04$chlorophyll
chla_2006_04 <- as.numeric(chla_2006_04)
chla_2006_04 <- mean(chla_2006_04)

chla_2006_05 <- chla2 %>% filter(time == "2006-05-16T00:00:00Z" & chlorophyll != NaN)
chla_2006_05 <- chla_2006_05$chlorophyll
chla_2006_05 <- as.numeric(chla_2006_05)
chla_2006_05 <- mean(chla_2006_05)

chla_2006_06 <- chla2 %>% filter(time == "2006-06-16T00:00:00Z" & chlorophyll != NaN)
chla_2006_06 <- chla_2006_06$chlorophyll
chla_2006_06 <- as.numeric(chla_2006_06)
chla_2006_06 <- mean(chla_2006_06)

chla_2006_07 <- chla2 %>% filter(time == "2006-07-16T00:00:00Z" & chlorophyll != NaN)
chla_2006_07 <- chla_2006_07$chlorophyll
chla_2006_07 <- as.numeric(chla_2006_07)
chla_2006_07 <- mean(chla_2006_07)

chla_2006_08 <- chla2 %>% filter(time == "2006-08-16T00:00:00Z" & chlorophyll != NaN)
chla_2006_08 <- chla_2006_08$chlorophyll
chla_2006_08 <- as.numeric(chla_2006_08)
chla_2006_08 <- mean(chla_2006_08)

chla_2006_09 <- chla2 %>% filter(time == "2006-09-16T00:00:00Z" & chlorophyll != NaN)
chla_2006_09 <- chla_2006_09$chlorophyll
chla_2006_09 <- as.numeric(chla_2006_09)
chla_2006_09 <- mean(chla_2006_09)

chla_2006_10 <- chla2 %>% filter(time == "2006-10-16T00:00:00Z" & chlorophyll != NaN)
chla_2006_10 <- chla_2006_10$chlorophyll
chla_2006_10 <- as.numeric(chla_2006_10)
chla_2006_10 <- mean(chla_2006_10)

chla_2006_11 <- chla2 %>% filter(time == "2006-11-16T00:00:00Z" & chlorophyll != NaN)
chla_2006_11 <- chla_2006_11$chlorophyll
chla_2006_11 <- as.numeric(chla_2006_11)
chla_2006_11 <- mean(chla_2006_11)

chla_2006_12 <- chla2 %>% filter(time == "2006-12-16T00:00:00Z" & chlorophyll != NaN)
chla_2006_12 <- chla_2006_12$chlorophyll
chla_2006_12 <- as.numeric(chla_2006_12)
chla_2006_12 <- mean(chla_2006_12)

chla_2006 <- c(chla_2006_01, chla_2006_02, chla_2006_03, chla_2006_04, chla_2006_05, chla_2006_06, chla_2006_07, chla_2006_08, chla_2006_09, chla_2006_10, chla_2006_11, chla_2006_12)

#2007
chla_2007_01 <- chla2 %>% filter(time == "2007-01-16T00:00:00Z" & chlorophyll != NaN)
chla_2007_01 <- chla_2007_01$chlorophyll
chla_2007_01 <- as.numeric(chla_2007_01)
chla_2007_01 <- mean(chla_2007_01)

chla_2007_02 <- chla2 %>% filter(time == "2007-02-16T00:00:00Z" & chlorophyll != NaN)
chla_2007_02 <- chla_2007_02$chlorophyll
chla_2007_02 <- as.numeric(chla_2007_02)
chla_2007_02 <- mean(chla_2007_02)

chla_2007_03 <- chla2 %>% filter(time == "2007-03-16T00:00:00Z" & chlorophyll != NaN)
chla_2007_03 <- chla_2007_03$chlorophyll
chla_2007_03 <- as.numeric(chla_2007_03)
chla_2007_03 <- mean(chla_2007_03)

chla_2007_04 <- chla2 %>% filter(time == "2007-04-16T00:00:00Z" & chlorophyll != NaN)
chla_2007_04 <- chla_2007_04$chlorophyll
chla_2007_04 <- as.numeric(chla_2007_04)
chla_2007_04 <- mean(chla_2007_04)

chla_2007_05 <- chla2 %>% filter(time == "2007-05-16T00:00:00Z" & chlorophyll != NaN)
chla_2007_05 <- chla_2007_05$chlorophyll
chla_2007_05 <- as.numeric(chla_2007_05)
chla_2007_05 <- mean(chla_2007_05)

chla_2007_06 <- chla2 %>% filter(time == "2007-06-16T00:00:00Z" & chlorophyll != NaN)
chla_2007_06 <- chla_2007_06$chlorophyll
chla_2007_06 <- as.numeric(chla_2007_06)
chla_2007_06 <- mean(chla_2007_06)

chla_2007_07 <- chla2 %>% filter(time == "2007-07-16T00:00:00Z" & chlorophyll != NaN)
chla_2007_07 <- chla_2007_07$chlorophyll
chla_2007_07 <- as.numeric(chla_2007_07)
chla_2007_07 <- mean(chla_2007_07)

chla_2007_08 <- chla2 %>% filter(time == "2007-08-16T00:00:00Z" & chlorophyll != NaN)
chla_2007_08 <- chla_2007_08$chlorophyll
chla_2007_08 <- as.numeric(chla_2007_08)
chla_2007_08 <- mean(chla_2007_08)

chla_2007_09 <- chla2 %>% filter(time == "2007-09-16T00:00:00Z" & chlorophyll != NaN)
chla_2007_09 <- chla_2007_09$chlorophyll
chla_2007_09 <- as.numeric(chla_2007_09)
chla_2007_09 <- mean(chla_2007_09)

chla_2007_10 <- chla2 %>% filter(time == "2007-10-16T00:00:00Z" & chlorophyll != NaN)
chla_2007_10 <- chla_2007_10$chlorophyll
chla_2007_10 <- as.numeric(chla_2007_10)
chla_2007_10 <- mean(chla_2007_10)

chla_2007_11 <- chla2 %>% filter(time == "2007-11-16T00:00:00Z" & chlorophyll != NaN)
chla_2007_11 <- chla_2007_11$chlorophyll
chla_2007_11 <- as.numeric(chla_2007_11)
chla_2007_11 <- mean(chla_2007_11)

chla_2007_12 <- chla2 %>% filter(time == "2007-12-16T00:00:00Z" & chlorophyll != NaN)
chla_2007_12 <- chla_2007_12$chlorophyll
chla_2007_12 <- as.numeric(chla_2007_12)
chla_2007_12 <- mean(chla_2007_12)

chla_2007 <- c(chla_2007_01, chla_2007_02, chla_2007_03, chla_2007_04, chla_2007_05, chla_2007_06, chla_2007_07, chla_2007_08, chla_2007_09, chla_2007_10, chla_2007_11, chla_2007_12)

#2008
chla_2008_01 <- chla2 %>% filter(time == "2008-01-16T00:00:00Z" & chlorophyll != NaN)
chla_2008_01 <- chla_2008_01$chlorophyll
chla_2008_01 <- as.numeric(chla_2008_01)
chla_2008_01 <- mean(chla_2008_01)

chla_2008_02 <- chla2 %>% filter(time == "2008-02-16T00:00:00Z" & chlorophyll != NaN)
chla_2008_02 <- chla_2008_02$chlorophyll
chla_2008_02 <- as.numeric(chla_2008_02)
chla_2008_02 <- mean(chla_2008_02)

chla_2008_03 <- chla2 %>% filter(time == "2008-03-16T00:00:00Z" & chlorophyll != NaN)
chla_2008_03 <- chla_2008_03$chlorophyll
chla_2008_03 <- as.numeric(chla_2008_03)
chla_2008_03 <- mean(chla_2008_03)

chla_2008_04 <- chla2 %>% filter(time == "2008-04-16T00:00:00Z" & chlorophyll != NaN)
chla_2008_04 <- chla_2008_04$chlorophyll
chla_2008_04 <- as.numeric(chla_2008_04)
chla_2008_04 <- mean(chla_2008_04)

chla_2008_05 <- chla2 %>% filter(time == "2008-05-16T00:00:00Z" & chlorophyll != NaN)
chla_2008_05 <- chla_2008_05$chlorophyll
chla_2008_05 <- as.numeric(chla_2008_05)
chla_2008_05 <- mean(chla_2008_05)

chla_2008_06 <- chla2 %>% filter(time == "2008-06-16T00:00:00Z" & chlorophyll != NaN)
chla_2008_06 <- chla_2008_06$chlorophyll
chla_2008_06 <- as.numeric(chla_2008_06)
chla_2008_06 <- mean(chla_2008_06)

chla_2008_07 <- chla2 %>% filter(time == "2008-07-16T00:00:00Z" & chlorophyll != NaN)
chla_2008_07 <- chla_2008_07$chlorophyll
chla_2008_07 <- as.numeric(chla_2008_07)
chla_2008_07 <- mean(chla_2008_07)

chla_2008_08 <- chla2 %>% filter(time == "2008-08-16T00:00:00Z" & chlorophyll != NaN)
chla_2008_08 <- chla_2008_08$chlorophyll
chla_2008_08 <- as.numeric(chla_2008_08)
chla_2008_08 <- mean(chla_2008_08)

chla_2008_09 <- chla2 %>% filter(time == "2008-09-16T00:00:00Z" & chlorophyll != NaN)
chla_2008_09 <- chla_2008_09$chlorophyll
chla_2008_09 <- as.numeric(chla_2008_09)
chla_2008_09 <- mean(chla_2008_09)

chla_2008_10 <- chla2 %>% filter(time == "2008-10-16T00:00:00Z" & chlorophyll != NaN)
chla_2008_10 <- chla_2008_10$chlorophyll
chla_2008_10 <- as.numeric(chla_2008_10)
chla_2008_10 <- mean(chla_2008_10)

chla_2008_11 <- chla2 %>% filter(time == "2008-11-16T00:00:00Z" & chlorophyll != NaN)
chla_2008_11 <- chla_2008_11$chlorophyll
chla_2008_11 <- as.numeric(chla_2008_11)
chla_2008_11 <- mean(chla_2008_11)

chla_2008_12 <- chla2 %>% filter(time == "2008-12-16T00:00:00Z" & chlorophyll != NaN)
chla_2008_12 <- chla_2008_12$chlorophyll
chla_2008_12 <- as.numeric(chla_2008_12)
chla_2008_12 <- mean(chla_2008_12)

chla_2008 <- c(chla_2008_01, chla_2008_02, chla_2008_03, chla_2008_04, chla_2008_05, chla_2008_06, chla_2008_07, chla_2008_08, chla_2008_09, chla_2008_10, chla_2008_11, chla_2008_12)

#2009
chla_2009_01 <- chla2 %>% filter(time == "2009-01-16T00:00:00Z" & chlorophyll != NaN)
chla_2009_01 <- chla_2009_01$chlorophyll
chla_2009_01 <- as.numeric(chla_2009_01)
chla_2009_01 <- mean(chla_2009_01)

chla_2009_02 <- chla2 %>% filter(time == "2009-02-16T00:00:00Z" & chlorophyll != NaN)
chla_2009_02 <- chla_2009_02$chlorophyll
chla_2009_02 <- as.numeric(chla_2009_02)
chla_2009_02 <- mean(chla_2009_02)

chla_2009_03 <- chla2 %>% filter(time == "2009-03-16T00:00:00Z" & chlorophyll != NaN)
chla_2009_03 <- chla_2009_03$chlorophyll
chla_2009_03 <- as.numeric(chla_2009_03)
chla_2009_03 <- mean(chla_2009_03)

chla_2009_04 <- chla2 %>% filter(time == "2009-04-16T00:00:00Z" & chlorophyll != NaN)
chla_2009_04 <- chla_2009_04$chlorophyll
chla_2009_04 <- as.numeric(chla_2009_04)
chla_2009_04 <- mean(chla_2009_04)

chla_2009_05 <- chla2 %>% filter(time == "2009-05-16T00:00:00Z" & chlorophyll != NaN)
chla_2009_05 <- chla_2009_05$chlorophyll
chla_2009_05 <- as.numeric(chla_2009_05)
chla_2009_05 <- mean(chla_2009_05)

chla_2009_06 <- chla2 %>% filter(time == "2009-06-16T00:00:00Z" & chlorophyll != NaN)
chla_2009_06 <- chla_2009_06$chlorophyll
chla_2009_06 <- as.numeric(chla_2009_06)
chla_2009_06 <- mean(chla_2009_06)

chla_2009_07 <- chla2 %>% filter(time == "2009-07-16T00:00:00Z" & chlorophyll != NaN)
chla_2009_07 <- chla_2009_07$chlorophyll
chla_2009_07 <- as.numeric(chla_2009_07)
chla_2009_07 <- mean(chla_2009_07)

chla_2009_08 <- chla2 %>% filter(time == "2009-08-16T00:00:00Z" & chlorophyll != NaN)
chla_2009_08 <- chla_2009_08$chlorophyll
chla_2009_08 <- as.numeric(chla_2009_08)
chla_2009_08 <- mean(chla_2009_08)

chla_2009_09 <- chla2 %>% filter(time == "2009-09-16T00:00:00Z" & chlorophyll != NaN)
chla_2009_09 <- chla_2009_09$chlorophyll
chla_2009_09 <- as.numeric(chla_2009_09)
chla_2009_09 <- mean(chla_2009_09)

chla_2009_10 <- chla2 %>% filter(time == "2009-10-16T00:00:00Z" & chlorophyll != NaN)
chla_2009_10 <- chla_2009_10$chlorophyll
chla_2009_10 <- as.numeric(chla_2009_10)
chla_2009_10 <- mean(chla_2009_10)

chla_2009_11 <- chla2 %>% filter(time == "2009-11-16T00:00:00Z" & chlorophyll != NaN)
chla_2009_11 <- chla_2009_11$chlorophyll
chla_2009_11 <- as.numeric(chla_2009_11)
chla_2009_11 <- mean(chla_2009_11)

chla_2009_12 <- chla2 %>% filter(time == "2009-12-16T00:00:00Z" & chlorophyll != NaN)
chla_2009_12 <- chla_2009_12$chlorophyll
chla_2009_12 <- as.numeric(chla_2009_12)
chla_2009_12 <- mean(chla_2009_12)

chla_2009 <- c(chla_2009_01, chla_2009_02, chla_2009_03, chla_2009_04, chla_2009_05, chla_2009_06, chla_2009_07, chla_2009_08, chla_2009_09, chla_2009_10, chla_2009_11, chla_2009_12)

#2010
chla_2010_01 <- chla2 %>% filter(time == "2010-01-16T00:00:00Z" & chlorophyll != NaN)
chla_2010_01 <- chla_2010_01$chlorophyll
chla_2010_01 <- as.numeric(chla_2010_01)
chla_2010_01 <- mean(chla_2010_01)

chla_2010_02 <- chla2 %>% filter(time == "2010-02-16T00:00:00Z" & chlorophyll != NaN)
chla_2010_02 <- chla_2010_02$chlorophyll
chla_2010_02 <- as.numeric(chla_2010_02)
chla_2010_02 <- mean(chla_2010_02)

chla_2010_03 <- chla2 %>% filter(time == "2010-03-16T00:00:00Z" & chlorophyll != NaN)
chla_2010_03 <- chla_2010_03$chlorophyll
chla_2010_03 <- as.numeric(chla_2010_03)
chla_2010_03 <- mean(chla_2010_03)

chla_2010_04 <- chla2 %>% filter(time == "2010-04-16T00:00:00Z" & chlorophyll != NaN)
chla_2010_04 <- chla_2010_04$chlorophyll
chla_2010_04 <- as.numeric(chla_2010_04)
chla_2010_04 <- mean(chla_2010_04)

chla_2010_05 <- chla2 %>% filter(time == "2010-05-16T00:00:00Z" & chlorophyll != NaN)
chla_2010_05 <- chla_2010_05$chlorophyll
chla_2010_05 <- as.numeric(chla_2010_05)
chla_2010_05 <- mean(chla_2010_05)

chla_2010_06 <- chla2 %>% filter(time == "2010-06-16T00:00:00Z" & chlorophyll != NaN)
chla_2010_06 <- chla_2010_06$chlorophyll
chla_2010_06 <- as.numeric(chla_2010_06)
chla_2010_06 <- mean(chla_2010_06)

chla_2010_07 <- chla2 %>% filter(time == "2010-07-16T00:00:00Z" & chlorophyll != NaN)
chla_2010_07 <- chla_2010_07$chlorophyll
chla_2010_07 <- as.numeric(chla_2010_07)
chla_2010_07 <- mean(chla_2010_07)

chla_2010_08 <- chla2 %>% filter(time == "2010-08-16T00:00:00Z" & chlorophyll != NaN)
chla_2010_08 <- chla_2010_08$chlorophyll
chla_2010_08 <- as.numeric(chla_2010_08)
chla_2010_08 <- mean(chla_2010_08)

chla_2010_09 <- chla2 %>% filter(time == "2010-09-16T00:00:00Z" & chlorophyll != NaN)
chla_2010_09 <- chla_2010_09$chlorophyll
chla_2010_09 <- as.numeric(chla_2010_09)
chla_2010_09 <- mean(chla_2010_09)

chla_2010_10 <- chla2 %>% filter(time == "2010-10-16T00:00:00Z" & chlorophyll != NaN)
chla_2010_10 <- chla_2010_10$chlorophyll
chla_2010_10 <- as.numeric(chla_2010_10)
chla_2010_10 <- mean(chla_2010_10)

chla_2010_11 <- chla2 %>% filter(time == "2010-11-16T00:00:00Z" & chlorophyll != NaN)
chla_2010_11 <- chla_2010_11$chlorophyll
chla_2010_11 <- as.numeric(chla_2010_11)
chla_2010_11 <- mean(chla_2010_11)

chla_2010_12 <- chla2 %>% filter(time == "2010-12-16T00:00:00Z" & chlorophyll != NaN)
chla_2010_12 <- chla_2010_12$chlorophyll
chla_2010_12 <- as.numeric(chla_2010_12)
chla_2010_12 <- mean(chla_2010_12)

chla_2010 <- c(chla_2010_01, chla_2010_02, chla_2010_03, chla_2010_04, chla_2010_05, chla_2010_06, chla_2010_07, chla_2010_08, chla_2010_09, chla_2010_10, chla_2010_11, chla_2010_12)

#2011
chla_2011_01 <- chla1 %>% filter(time == "2011-01-16T00:00:00Z" & chlorophyll != NaN)
chla_2011_01 <- chla_2011_01$chlorophyll
as.numeric(chla_2011_01)

chla_2011_02 <- chla1 %>% filter(time == "2011-02-16T00:00:00Z" & chlorophyll != NaN)
chla_2011_02 <- chla_2011_02$chlorophyll
chla_2011_02 <- as.numeric(chla_2011_02)
chla_2011_02 <- mean(chla_2011_02)

chla_2011_03 <- chla1 %>% filter(time == "2011-03-16T00:00:00Z" & chlorophyll != NaN)
chla_2011_03 <- chla_2011_03$chlorophyll
chla_2011_03 <- as.numeric(chla_2011_03)
chla_2011_03 <- mean(chla_2011_03)

chla_2011_04 <- chla1 %>% filter(time == "2011-04-16T00:00:00Z" & chlorophyll != NaN)
chla_2011_04 <- chla_2011_04$chlorophyll
chla_2011_04 <- as.numeric(chla_2011_04)
chla_2011_04 <- mean(chla_2011_04)

chla_2011_05 <- chla1 %>% filter(time == "2011-05-16T00:00:00Z" & chlorophyll != NaN)
chla_2011_05 <- chla_2011_05$chlorophyll
chla_2011_05 <- as.numeric(chla_2011_05)
chla_2011_05 <- mean(chla_2011_05)

chla_2011_06 <- chla1 %>% filter(time == "2011-06-16T00:00:00Z" & chlorophyll != NaN)
chla_2011_06 <- chla_2011_06$chlorophyll
chla_2011_06 <- as.numeric(chla_2011_06)
chla_2011_06 <- mean(chla_2011_06)

chla_2011_07 <- chla1 %>% filter(time == "2011-07-16T00:00:00Z" & chlorophyll != NaN)
chla_2011_07 <- chla_2011_07$chlorophyll
chla_2011_07 <- as.numeric(chla_2011_07)
chla_2011_07 <- mean(chla_2011_07)

chla_2011_08 <- chla1 %>% filter(time == "2011-08-16T00:00:00Z" & chlorophyll != NaN)
chla_2011_08 <- chla_2011_08$chlorophyll
chla_2011_08 <- as.numeric(chla_2011_08)
chla_2011_08 <- mean(chla_2011_08)

chla_2011_09 <- chla1 %>% filter(time == "2011-09-16T00:00:00Z" & chlorophyll != NaN)
chla_2011_09 <- chla_2011_09$chlorophyll
chla_2011_09 <- as.numeric(chla_2011_09)
chla_2011_09 <- mean(chla_2011_09)

chla_2011_10 <- chla1 %>% filter(time == "2011-10-16T00:00:00Z" & chlorophyll != NaN)
chla_2011_10 <- chla_2011_10$chlorophyll
chla_2011_10 <- as.numeric(chla_2011_10)
chla_2011_10 <- mean(chla_2011_10)

chla_2011_11 <- chla1 %>% filter(time == "2011-11-16T00:00:00Z" & chlorophyll != NaN)
chla_2011_11 <- chla_2011_11$chlorophyll
chla_2011_11 <- as.numeric(chla_2011_11)
chla_2011_11 <- mean(chla_2011_11)

chla_2011_12 <- chla1 %>% filter(time == "2011-12-16T00:00:00Z" & chlorophyll != NaN)
chla_2011_12 <- chla_2011_12$chlorophyll
chla_2011_12 <- as.numeric(chla_2011_12)
chla_2011_12 <- mean(chla_2011_12)

chla_2011 <- c(chla_2011_01, chla_2011_02, chla_2011_03, chla_2011_04, chla_2011_05, chla_2011_06, chla_2011_07, chla_2011_08, chla_2011_09, chla_2011_10, chla_2011_11, chla_2011_12)

#2012
chla_2012_01 <- chla1 %>% filter(time == "2012-01-16T00:00:00Z" & chlorophyll != NaN)
chla_2012_01 <- chla_2012_01$chlorophyll
chla_2012_01 <- as.numeric(chla_2012_01)
chla_2012_01 <- mean(chla_2012_01)

chla_2012_02 <- chla1 %>% filter(time == "2012-02-16T00:00:00Z" & chlorophyll != NaN)
chla_2012_02 <- chla_2012_02$chlorophyll
chla_2012_02 <- as.numeric(chla_2012_02)
chla_2012_02 <- mean(chla_2012_02)

chla_2012_03 <- chla1 %>% filter(time == "2012-03-16T00:00:00Z" & chlorophyll != NaN)
chla_2012_03 <- chla_2012_03$chlorophyll
chla_2012_03 <- as.numeric(chla_2012_03)
chla_2012_03 <- mean(chla_2012_03)

chla_2012_04 <- chla1 %>% filter(time == "2012-04-16T00:00:00Z" & chlorophyll != NaN)
chla_2012_04 <- chla_2012_04$chlorophyll
chla_2012_04 <- as.numeric(chla_2012_04)
chla_2012_04 <- mean(chla_2012_04)

chla_2012_05 <- chla1 %>% filter(time == "2012-05-16T00:00:00Z" & chlorophyll != NaN)
chla_2012_05 <- chla_2012_05$chlorophyll
chla_2012_05 <- as.numeric(chla_2012_05)
chla_2012_05 <- mean(chla_2012_05)

chla_2012_06 <- chla1 %>% filter(time == "2012-06-16T00:00:00Z" & chlorophyll != NaN)
chla_2012_06 <- chla_2012_06$chlorophyll
chla_2012_06 <- as.numeric(chla_2012_06)
chla_2012_06 <- mean(chla_2012_06)

chla_2012_07 <- chla1 %>% filter(time == "2012-07-16T00:00:00Z" & chlorophyll != NaN)
chla_2012_07 <- chla_2012_07$chlorophyll
chla_2012_07 <- as.numeric(chla_2012_07)
chla_2012_07 <- mean(chla_2012_07)

chla_2012_08 <- chla1 %>% filter(time == "2012-08-16T00:00:00Z" & chlorophyll != NaN)
chla_2012_08 <- chla_2012_08$chlorophyll
chla_2012_08 <- as.numeric(chla_2012_08)
chla_2012_08 <- mean(chla_2012_08)

chla_2012_09 <- chla1 %>% filter(time == "2012-09-16T00:00:00Z" & chlorophyll != NaN)
chla_2012_09 <- chla_2012_09$chlorophyll
chla_2012_09 <- as.numeric(chla_2012_09)
chla_2012_09 <- mean(chla_2012_09)

chla_2012_10 <- chla1 %>% filter(time == "2012-10-16T00:00:00Z" & chlorophyll != NaN)
chla_2012_10 <- chla_2012_10$chlorophyll
chla_2012_10 <- as.numeric(chla_2012_10)
chla_2012_10 <- mean(chla_2012_10)

chla_2012_11 <- chla1 %>% filter(time == "2012-11-16T00:00:00Z" & chlorophyll != NaN)
chla_2012_11 <- chla_2012_11$chlorophyll
chla_2012_11 <- as.numeric(chla_2012_11)
chla_2012_11 <- mean(chla_2012_11)

chla_2012_12 <- chla1 %>% filter(time == "2012-12-16T00:00:00Z" & chlorophyll != NaN)
chla_2012_12 <- chla_2012_12$chlorophyll
chla_2012_12 <- as.numeric(chla_2012_12)
chla_2012_12 <- mean(chla_2012_12)

chla_2012 <- c(chla_2012_01, chla_2012_02, chla_2012_03, chla_2012_04, chla_2012_05, chla_2012_06, chla_2012_07, chla_2012_08, chla_2012_09, chla_2012_10, chla_2012_11, chla_2012_12)

#2013
chla_2013_01 <- chla1 %>% filter(time == "2013-01-16T00:00:00Z" & chlorophyll != NaN)
chla_2013_01 <- chla_2013_01$chlorophyll
chla_2013_01 <- as.numeric(chla_2013_01)
chla_2013_01 <- mean(chla_2013_01)

chla_2013_02 <- chla1 %>% filter(time == "2013-02-16T00:00:00Z" & chlorophyll != NaN)
chla_2013_02 <- chla_2013_02$chlorophyll
chla_2013_02 <- as.numeric(chla_2012_03)
chla_2013_02 <- mean(chla_2013_02)

chla_2013_03 <- chla1 %>% filter(time == "2013-03-16T00:00:00Z" & chlorophyll != NaN)
chla_2013_03 <- chla_2013_03$chlorophyll
chla_2013_03 <- as.numeric(chla_2013_03)
chla_2013_03 <- mean(chla_2013_03)

chla_2013_04 <- chla1 %>% filter(time == "2013-04-16T00:00:00Z" & chlorophyll != NaN)
chla_2013_04 <- chla_2013_04$chlorophyll
chla_2013_04 <- as.numeric(chla_2013_04)
chla_2013_04 <- mean(chla_2013_04)

chla_2013_05 <- chla1 %>% filter(time == "2013-05-16T00:00:00Z" & chlorophyll != NaN)
chla_2013_05 <- chla_2013_05$chlorophyll
chla_2013_05 <- as.numeric(chla_2013_05)
chla_2013_05 <- mean(chla_2013_05)

chla_2013_06 <- chla1 %>% filter(time == "2013-06-16T00:00:00Z" & chlorophyll != NaN)
chla_2013_06 <- chla_2013_06$chlorophyll
chla_2013_06 <- as.numeric(chla_2013_06)
chla_2013_06 <- mean(chla_2013_06)

chla_2013_07 <- chla1 %>% filter(time == "2013-07-16T00:00:00Z" & chlorophyll != NaN)
chla_2013_07 <- chla_2013_07$chlorophyll
chla_2013_07 <- as.numeric(chla_2013_07)
chla_2013_07 <- mean(chla_2013_07)

chla_2013_08 <- chla1 %>% filter(time == "2013-08-16T00:00:00Z" & chlorophyll != NaN)
chla_2013_08 <- chla_2013_08$chlorophyll
chla_2013_08 <- as.numeric(chla_2013_08)
chla_2013_08 <- mean(chla_2013_08)

chla_2013_09 <- chla1 %>% filter(time == "2013-09-16T00:00:00Z" & chlorophyll != NaN)
chla_2013_09 <- chla_2013_09$chlorophyll
chla_2013_09 <- as.numeric(chla_2013_09)
chla_2013_09 <- mean(chla_2013_09)

chla_2013_10 <- chla1 %>% filter(time == "2013-10-16T00:00:00Z" & chlorophyll != NaN)
chla_2013_10 <- chla_2013_10$chlorophyll
chla_2013_10 <- as.numeric(chla_2013_10)
chla_2013_10 <- mean(chla_2013_10)

chla_2013_11 <- chla1 %>% filter(time == "2013-11-16T00:00:00Z" & chlorophyll != NaN)
chla_2013_11 <- chla_2013_11$chlorophyll
chla_2013_11 <- as.numeric(chla_2013_11)
chla_2013_11 <- mean(chla_2013_11)

chla_2013_12 <- chla1 %>% filter(time == "2013-12-16T00:00:00Z" & chlorophyll != NaN)
chla_2013_12 <- chla_2013_12$chlorophyll
chla_2013_12 <- as.numeric(chla_2013_12)
chla_2013_12 <- mean(chla_2013_12)

chla_2013 <- c(chla_2013_01, chla_2013_02, chla_2013_03, chla_2013_04, chla_2013_05, chla_2013_06, chla_2013_07, chla_2013_08, chla_2013_09, chla_2013_10, chla_2013_11, chla_2013_12)

#2012
chla_2014_01 <- chla1 %>% filter(time == "2014-01-16T00:00:00Z" & chlorophyll != NaN)
chla_2014_01 <- chla_2014_01$chlorophyll
chla_2014_01 <- as.numeric(chla_2014_01)
chla_2014_01 <- mean(chla_2014_01)

chla_2014_02 <- chla1 %>% filter(time == "2014-02-16T00:00:00Z" & chlorophyll != NaN)
chla_2014_02 <- chla_2014_02$chlorophyll
chla_2014_02 <- as.numeric(chla_2014_02)
chla_2014_02 <- mean(chla_2014_02)

chla_2014_03 <- chla1 %>% filter(time == "2014-03-16T00:00:00Z" & chlorophyll != NaN)
chla_2014_03 <- chla_2014_03$chlorophyll
chla_2014_03 <- as.numeric(chla_2014_03)
chla_2014_03 <- mean(chla_2014_03)

chla_2014_04 <- chla1 %>% filter(time == "2014-04-16T00:00:00Z" & chlorophyll != NaN)
chla_2014_04 <- chla_2014_04$chlorophyll
chla_2014_04 <- as.numeric(chla_2014_04)
chla_2014_04 <- mean(chla_2014_04)

chla_2014_05 <- chla1 %>% filter(time == "2014-05-16T00:00:00Z" & chlorophyll != NaN)
chla_2014_05 <- chla_2014_05$chlorophyll
chla_2014_05 <- as.numeric(chla_2014_05)
chla_2014_05 <- mean(chla_2014_05)

chla_2014_06 <- chla1 %>% filter(time == "2014-06-16T00:00:00Z" & chlorophyll != NaN)
chla_2014_06 <- chla_2014_06$chlorophyll
chla_2014_06 <- as.numeric(chla_2014_06)
chla_2014_06 <- mean(chla_2014_06)

chla_2014_07 <- chla1 %>% filter(time == "2014-07-16T00:00:00Z" & chlorophyll != NaN)
chla_2014_07 <- chla_2014_07$chlorophyll
chla_2014_07 <- as.numeric(chla_2014_07)
chla_2014_07 <- mean(chla_2014_07)

chla_2014_08 <- chla1 %>% filter(time == "2014-08-16T00:00:00Z" & chlorophyll != NaN)
chla_2014_08 <- chla_2014_08$chlorophyll
chla_2014_08 <- as.numeric(chla_2014_08)
chla_2014_08 <- mean(chla_2014_08)

chla_2014_09 <- chla1 %>% filter(time == "2014-09-16T00:00:00Z" & chlorophyll != NaN)
chla_2014_09 <- chla_2014_09$chlorophyll
chla_2014_09 <- as.numeric(chla_2014_09)
chla_2014_09 <- mean(chla_2014_09)

chla_2014_10 <- chla1 %>% filter(time == "2014-10-16T00:00:00Z" & chlorophyll != NaN)
chla_2014_10 <- chla_2014_10$chlorophyll
chla_2014_10 <- as.numeric(chla_2014_10)
chla_2014_10 <- mean(chla_2014_10)

chla_2014_11 <- chla1 %>% filter(time == "2014-11-16T00:00:00Z" & chlorophyll != NaN)
chla_2014_11 <- chla_2014_11$chlorophyll
chla_2014_11 <- as.numeric(chla_2014_11)
chla_2014_11 <- mean(chla_2014_11)

chla_2014_12 <- chla1 %>% filter(time == "2014-12-16T00:00:00Z" & chlorophyll != NaN)
chla_2014_12 <- chla_2014_12$chlorophyll
chla_2014_12 <- as.numeric(chla_2014_12)
chla_2014_12 <- mean(chla_2014_12)

chla_2014 <- c(chla_2014_01, chla_2014_02, chla_2014_03, chla_2014_04, chla_2014_05, chla_2014_06, chla_2014_07, chla_2014_08, chla_2014_09, chla_2014_10, chla_2014_11, chla_2014_12)

#2015
chla_2015_01 <- chla1 %>% filter(time == "2015-01-16T00:00:00Z" & chlorophyll != NaN)
chla_2015_01 <- chla_2015_01$chlorophyll
chla_2015_01 <- as.numeric(chla_2015_01)
chla_2015_01 <- mean(chla_2015_01)

chla_2015_02 <- chla1 %>% filter(time == "2015-02-16T00:00:00Z" & chlorophyll != NaN)
chla_2015_02 <- chla_2015_02$chlorophyll
chla_2015_02 <- as.numeric(chla_2015_02)
chla_2015_02 <- mean(chla_2015_02)

chla_2015_03 <- chla1 %>% filter(time == "2015-03-16T00:00:00Z" & chlorophyll != NaN)
chla_2015_03 <- chla_2015_03$chlorophyll
chla_2015_03 <- as.numeric(chla_2015_03)
chla_2015_03 <- mean(chla_2015_03)

chla_2015_04 <- chla1 %>% filter(time == "2015-04-16T00:00:00Z" & chlorophyll != NaN)
chla_2015_04 <- chla_2015_04$chlorophyll
chla_2015_04 <- as.numeric(chla_2015_04)
chla_2015_04 <- mean(chla_2015_04)

chla_2015_05 <- chla1 %>% filter(time == "2015-05-16T00:00:00Z" & chlorophyll != NaN)
chla_2015_05 <- chla_2015_05$chlorophyll
chla_2015_05 <- as.numeric(chla_2015_05)
chla_2015_05 <- mean(chla_2015_05)

chla_2015_06 <- chla1 %>% filter(time == "2015-06-16T00:00:00Z" & chlorophyll != NaN)
chla_2015_06 <- chla_2015_06$chlorophyll
chla_2015_06 <- as.numeric(chla_2015_06)
chla_2015_06 <- mean(chla_2015_06)

chla_2015_07 <- chla1 %>% filter(time == "2015-07-16T00:00:00Z" & chlorophyll != NaN)
chla_2015_07 <- chla_2015_07$chlorophyll
chla_2015_07 <- as.numeric(chla_2015_07)
chla_2015_07 <- mean(chla_2015_07)

chla_2015_08 <- chla1 %>% filter(time == "2015-08-16T00:00:00Z" & chlorophyll != NaN)
chla_2015_08 <- chla_2015_08$chlorophyll
chla_2015_08 <- as.numeric(chla_2015_08)
chla_2015_08 <- mean(chla_2015_08)

chla_2015_09 <- chla1 %>% filter(time == "2015-09-16T00:00:00Z" & chlorophyll != NaN)
chla_2015_09 <- chla_2015_09$chlorophyll
chla_2015_09 <- as.numeric(chla_2015_09)
chla_2015_09 <- mean(chla_2015_09)

chla_2015_10 <- chla1 %>% filter(time == "2015-10-16T00:00:00Z" & chlorophyll != NaN)
chla_2015_10 <- chla_2015_10$chlorophyll
chla_2015_10 <- as.numeric(chla_2015_10)
chla_2015_10 <- mean(chla_2015_10)

chla_2015_11 <- chla1 %>% filter(time == "2015-11-16T00:00:00Z" & chlorophyll != NaN)
chla_2015_11 <- chla_2015_11$chlorophyll
chla_2015_11 <- as.numeric(chla_2015_11)
chla_2015_11 <- mean(chla_2015_11)

chla_2015_12 <- chla1 %>% filter(time == "2015-12-16T00:00:00Z" & chlorophyll != NaN)
chla_2015_12 <- chla_2015_12$chlorophyll
chla_2015_12 <- as.numeric(chla_2015_12)
chla_2015_12 <- mean(chla_2015_12)

chla_2015 <- c(chla_2015_01, chla_2015_02, chla_2015_03, chla_2015_04, chla_2015_05, chla_2015_06, chla_2015_07, chla_2015_08, chla_2015_09, chla_2015_10, chla_2015_11, chla_2015_12)

#2016
chla_2016_01 <- chla1 %>% filter(time == "2016-01-16T00:00:00Z" & chlorophyll != NaN)
chla_2016_01 <- chla_2016_01$chlorophyll
chla_2016_01 <- as.numeric(chla_2016_01)
chla_2016_01 <- mean(chla_2016_01)

chla_2016_02 <- chla1 %>% filter(time == "2016-02-16T00:00:00Z" & chlorophyll != NaN)
chla_2016_02 <- chla_2016_02$chlorophyll
chla_2016_02 <- as.numeric(chla_2016_02)
chla_2016_02 <- mean(chla_2016_02)

chla_2016_03 <- chla1 %>% filter(time == "2016-03-16T00:00:00Z" & chlorophyll != NaN)
chla_2016_03 <- chla_2016_03$chlorophyll
chla_2016_03 <- as.numeric(chla_2016_03)
chla_2016_03 <- mean(chla_2016_03)

chla_2016_04 <- chla1 %>% filter(time == "2016-04-16T00:00:00Z" & chlorophyll != NaN)
chla_2016_04 <- chla_2016_04$chlorophyll
chla_2016_04 <- as.numeric(chla_2016_04)
chla_2016_04 <- mean(chla_2016_04)

chla_2016_05 <- chla1 %>% filter(time == "2016-05-16T00:00:00Z" & chlorophyll != NaN)
chla_2016_05 <- chla_2016_05$chlorophyll
chla_2016_05 <- as.numeric(chla_2016_05)
chla_2016_05 <- mean(chla_2016_05)

chla_2016_06 <- chla1 %>% filter(time == "2016-06-16T00:00:00Z" & chlorophyll != NaN)
chla_2016_06 <- chla_2016_06$chlorophyll
chla_2016_06 <- as.numeric(chla_2016_06)
chla_2016_06 <- mean(chla_2016_06)

chla_2016_07 <- chla1 %>% filter(time == "2016-07-16T00:00:00Z" & chlorophyll != NaN)
chla_2016_07 <- chla_2016_07$chlorophyll
chla_2016_07 <- as.numeric(chla_2016_07)
chla_2016_07 <- mean(chla_2016_07)

chla_2016_08 <- chla1 %>% filter(time == "2016-08-16T00:00:00Z" & chlorophyll != NaN)
chla_2016_08 <- chla_2016_08$chlorophyll
chla_2016_08 <- as.numeric(chla_2016_08)
chla_2016_08 <- mean(chla_2016_08)

chla_2016_09 <- chla1 %>% filter(time == "2016-09-16T00:00:00Z" & chlorophyll != NaN)
chla_2016_09 <- chla_2016_09$chlorophyll
chla_2016_09 <- as.numeric(chla_2016_09)
chla_2016_09 <- mean(chla_2016_09)

chla_2016_10 <- chla1 %>% filter(time == "2016-10-16T00:00:00Z" & chlorophyll != NaN)
chla_2016_10 <- chla_2016_10$chlorophyll
chla_2016_10 <- as.numeric(chla_2016_10)
chla_2016_10 <- mean(chla_2016_10)

chla_2016_11 <- chla1 %>% filter(time == "2016-11-16T00:00:00Z" & chlorophyll != NaN)
chla_2016_11 <- chla_2016_11$chlorophyll
chla_2016_11 <- as.numeric(chla_2016_11)
chla_2016_11 <- mean(chla_2016_11)

chla_2016_12 <- chla1 %>% filter(time == "2016-12-16T00:00:00Z" & chlorophyll != NaN)
chla_2016_12 <- chla_2016_12$chlorophyll
chla_2016_12 <- as.numeric(chla_2016_12)
chla_2016_12 <- mean(chla_2016_12)

chla_2016 <- c(chla_2016_01, chla_2016_02, chla_2016_03, chla_2016_04, chla_2016_05, chla_2016_06, chla_2016_07, chla_2016_08, chla_2016_09, chla_2016_10, chla_2016_11, chla_2016_12)

#2017
chla_2017_01 <- chla1 %>% filter(time == "2017-01-16T00:00:00Z" & chlorophyll != NaN)
chla_2017_01 <- chla_2017_01$chlorophyll
chla_2017_01 <- as.numeric(chla_2017_01)
chla_2017_01 <- mean(chla_2017_01)

chla_2017_02 <- chla1 %>% filter(time == "2017-02-16T00:00:00Z" & chlorophyll != NaN)
chla_2017_02 <- chla_2017_02$chlorophyll
chla_2017_02 <- as.numeric(chla_2017_02)
chla_2017_02 <- mean(chla_2017_02)

chla_2017_03 <- chla1 %>% filter(time == "2017-03-16T00:00:00Z" & chlorophyll != NaN)
chla_2017_03 <- chla_2017_03$chlorophyll
chla_2017_03 <- as.numeric(chla_2017_03)
chla_2017_03 <- mean(chla_2017_03)

chla_2017_04 <- chla1 %>% filter(time == "2017-04-16T00:00:00Z" & chlorophyll != NaN)
chla_2017_04 <- chla_2017_04$chlorophyll
chla_2017_04 <- as.numeric(chla_2017_04)
chla_2017_04 <- mean(chla_2017_04)

chla_2017_05 <- chla1 %>% filter(time == "2017-05-16T00:00:00Z" & chlorophyll != NaN)
chla_2017_05 <- chla_2017_05$chlorophyll
chla_2017_05 <- as.numeric(chla_2017_05)
chla_2017_05 <- mean(chla_2017_05)

chla_2017_06 <- chla1 %>% filter(time == "2017-06-16T00:00:00Z" & chlorophyll != NaN)
chla_2017_06 <- chla_2017_06$chlorophyll
chla_2017_06 <- as.numeric(chla_2017_06)
chla_2017_06 <- mean(chla_2017_06)

chla_2017_07 <- chla1 %>% filter(time == "2017-07-16T00:00:00Z" & chlorophyll != NaN)
chla_2017_07 <- chla_2017_07$chlorophyll
chla_2017_07 <- as.numeric(chla_2017_07)
chla_2017_07 <- mean(chla_2017_07)

chla_2017_08 <- chla1 %>% filter(time == "2017-08-16T00:00:00Z" & chlorophyll != NaN)
chla_2017_08 <- chla_2017_08$chlorophyll
chla_2017_08 <- as.numeric(chla_2017_08)
chla_2017_08 <- mean(chla_2017_08)

chla_2017_09 <- chla1 %>% filter(time == "2017-09-16T00:00:00Z" & chlorophyll != NaN)
chla_2017_09 <- chla_2017_09$chlorophyll
chla_2017_09 <- as.numeric(chla_2017_09)
chla_2017_09 <- mean(chla_2017_09)

chla_2017_10 <- chla1 %>% filter(time == "2017-10-16T00:00:00Z" & chlorophyll != NaN)
chla_2017_10 <- chla_2017_10$chlorophyll
chla_2017_10 <- as.numeric(chla_2017_10)
chla_2017_10 <- mean(chla_2017_10)

chla_2017_11 <- chla1 %>% filter(time == "2017-11-16T00:00:00Z" & chlorophyll != NaN)
chla_2017_11 <- chla_2017_11$chlorophyll
chla_2017_11 <- as.numeric(chla_2017_11)
chla_2017_11 <- mean(chla_2017_11)

chla_2017_12 <- chla1 %>% filter(time == "2017-12-16T00:00:00Z" & chlorophyll != NaN)
chla_2017_12 <- chla_2017_12$chlorophyll
chla_2017_12 <- as.numeric(chla_2017_12)
chla_2017_12 <- mean(chla_2017_12)

chla_2017 <- c(chla_2017_01, chla_2017_02, chla_2017_03, chla_2017_04, chla_2017_05, chla_2017_06, chla_2017_07, chla_2017_08, chla_2017_09, chla_2017_10, chla_2017_11, chla_2017_12)

#2018
chla_2018_01 <- chla1 %>% filter(time == "2018-01-16T00:00:00Z" & chlorophyll != NaN)
chla_2018_01 <- chla_2018_01$chlorophyll
chla_2018_01 <- as.numeric(chla_2018_01)
chla_2018_01 <- mean(chla_2018_01)

chla_2018_02 <- chla1 %>% filter(time == "2018-02-16T00:00:00Z" & chlorophyll != NaN)
chla_2018_02 <- chla_2018_02$chlorophyll
chla_2018_02 <- as.numeric(chla_2018_02)
chla_2018_02 <- mean(chla_2018_02)

chla_2018_03 <- chla1 %>% filter(time == "2018-03-16T00:00:00Z" & chlorophyll != NaN)
chla_2018_03 <- chla_2018_03$chlorophyll
chla_2018_03 <- as.numeric(chla_2018_03)
chla_2018_03 <- mean(chla_2018_03)

chla_2018_04 <- chla1 %>% filter(time == "2018-04-16T00:00:00Z" & chlorophyll != NaN)
chla_2018_04 <- chla_2018_04$chlorophyll
chla_2018_04 <- as.numeric(chla_2018_04)
chla_2018_04 <- mean(chla_2018_04)

chla_2018_05 <- chla1 %>% filter(time == "2018-05-16T00:00:00Z" & chlorophyll != NaN)
chla_2018_05 <- chla_2018_05$chlorophyll
chla_2018_05 <- as.numeric(chla_2018_05)
chla_2018_05 <- mean(chla_2018_05)

chla_2018_06 <- chla1 %>% filter(time == "2018-06-16T00:00:00Z" & chlorophyll != NaN)
chla_2018_06 <- chla_2018_06$chlorophyll
chla_2018_06 <- as.numeric(chla_2018_06)
chla_2018_06 <- mean(chla_2018_06)

chla_2018_07 <- chla1 %>% filter(time == "2018-07-16T00:00:00Z" & chlorophyll != NaN)
chla_2018_07 <- chla_2018_07$chlorophyll
chla_2018_07 <- as.numeric(chla_2018_07)
chla_2018_07 <- mean(chla_2018_07)

chla_2018_08 <- chla1 %>% filter(time == "2018-08-16T00:00:00Z" & chlorophyll != NaN)
chla_2018_08 <- chla_2018_08$chlorophyll
chla_2018_08 <- as.numeric(chla_2018_08)
chla_2018_08 <- mean(chla_2018_08)

chla_2018_09 <- chla1 %>% filter(time == "2018-09-16T00:00:00Z" & chlorophyll != NaN)
chla_2018_09 <- chla_2018_09$chlorophyll
chla_2018_09 <- as.numeric(chla_2018_09)
chla_2018_09 <- mean(chla_2018_09)

chla_2018_10 <- chla1 %>% filter(time == "2018-10-16T00:00:00Z" & chlorophyll != NaN)
chla_2018_10 <- chla_2018_10$chlorophyll
chla_2018_10 <- as.numeric(chla_2018_10)
chla_2018_10 <- mean(chla_2018_10)

chla_2018_11 <- chla1 %>% filter(time == "2018-11-16T00:00:00Z" & chlorophyll != NaN)
chla_2018_11 <- chla_2018_11$chlorophyll
chla_2018_11 <- as.numeric(chla_2018_11)
chla_2018_11 <- mean(chla_2018_11)

chla_2018_12 <- chla1 %>% filter(time == "2018-12-16T00:00:00Z" & chlorophyll != NaN)
chla_2018_12 <- chla_2018_12$chlorophyll
chla_2018_12 <- as.numeric(chla_2018_12)
chla_2018_12 <- mean(chla_2018_12)

chla_2018 <- c(chla_2018_01, chla_2018_02, chla_2018_03, chla_2018_04, chla_2018_05, chla_2018_06, chla_2018_07, chla_2018_08, chla_2018_09, chla_2018_10, chla_2018_11, chla_2018_12)

#2019
chla_2019_01 <- chla1 %>% filter(time == "2019-01-16T00:00:00Z" & chlorophyll != NaN)
chla_2019_01 <- chla_2019_01$chlorophyll
chla_2019_01 <- as.numeric(chla_2019_01)
chla_2019_01 <- mean(chla_2019_01)

chla_2019_02 <- chla1 %>% filter(time == "2019-02-16T00:00:00Z" & chlorophyll != NaN)
chla_2019_02 <- chla_2019_02$chlorophyll
chla_2019_02 <- as.numeric(chla_2019_02)
chla_2019_02 <- mean(chla_2019_02)

chla_2019_03 <- chla1 %>% filter(time == "2019-03-16T00:00:00Z" & chlorophyll != NaN)
chla_2019_03 <- chla_2019_03$chlorophyll
chla_2019_03 <- as.numeric(chla_2019_03)
chla_2019_03 <- mean(chla_2019_03)

chla_2019_04 <- chla1 %>% filter(time == "2019-04-16T00:00:00Z" & chlorophyll != NaN)
chla_2019_04 <- chla_2019_04$chlorophyll
chla_2019_04 <- as.numeric(chla_2019_04)
chla_2019_04 <- mean(chla_2019_04)

chla_2019_05 <- chla1 %>% filter(time == "2019-05-16T00:00:00Z" & chlorophyll != NaN)
chla_2019_05 <- chla_2019_05$chlorophyll
chla_2019_05 <- as.numeric(chla_2019_05)
chla_2019_05 <- mean(chla_2019_05)

chla_2019_06 <- chla1 %>% filter(time == "2019-06-16T00:00:00Z" & chlorophyll != NaN)
chla_2019_06 <- chla_2019_06$chlorophyll
chla_2019_06 <- as.numeric(chla_2019_06)
chla_2019_06 <- mean(chla_2019_06)

chla_2019_07 <- chla1 %>% filter(time == "2019-07-16T00:00:00Z" & chlorophyll != NaN)
chla_2019_07 <- chla_2019_07$chlorophyll
chla_2019_07 <- as.numeric(chla_2019_07)
chla_2019_07 <- mean(chla_2019_07)

chla_2019_08 <- chla1 %>% filter(time == "2019-08-16T00:00:00Z" & chlorophyll != NaN)
chla_2019_08 <- chla_2019_08$chlorophyll
chla_2019_08 <- as.numeric(chla_2019_08)
chla_2019_08 <- mean(chla_2019_08)

chla_2019_09 <- chla1 %>% filter(time == "2019-09-16T00:00:00Z" & chlorophyll != NaN)
chla_2019_09 <- chla_2019_09$chlorophyll
chla_2019_09 <- as.numeric(chla_2019_09)
chla_2019_09 <- mean(chla_2019_09)

chla_2019_10 <- chla1 %>% filter(time == "2019-10-16T00:00:00Z" & chlorophyll != NaN)
chla_2019_10 <- chla_2019_10$chlorophyll
chla_2019_10 <- as.numeric(chla_2019_10)
chla_2019_10 <- mean(chla_2019_10)

chla_2019_11 <- chla1 %>% filter(time == "2019-11-16T00:00:00Z" & chlorophyll != NaN)
chla_2019_11 <- chla_2019_11$chlorophyll
chla_2019_11 <- as.numeric(chla_2019_11)
chla_2019_11 <- mean(chla_2019_11)

chla_2019_12 <- chla1 %>% filter(time == "2019-12-16T00:00:00Z" & chlorophyll != NaN)
chla_2019_12 <- chla_2019_12$chlorophyll
chla_2019_12 <- as.numeric(chla_2019_12)
chla_2019_12 <- mean(chla_2019_12)

chla_2019 <- c(chla_2019_01, chla_2019_02, chla_2019_03, chla_2019_04, chla_2019_05, chla_2019_06, chla_2019_07, chla_2019_08, chla_2019_09, chla_2019_10, chla_2019_11, chla_2019_12)

#2020
chla_2020_01 <- chla1 %>% filter(time == "2020-01-16T00:00:00Z" & chlorophyll != NaN)
chla_2020_01 <- chla_2020_01$chlorophyll
chla_2020_01 <- as.numeric(chla_2020_01)
chla_2020_01 <- mean(chla_2020_01)

chla_2020_02 <- chla1 %>% filter(time == "2020-02-16T00:00:00Z" & chlorophyll != NaN)
chla_2020_02 <- chla_2020_02$chlorophyll
chla_2020_02 <- as.numeric(chla_2020_02)
chla_2020_02 <- mean(chla_2020_02)

chla_2020_03 <- chla1 %>% filter(time == "2020-03-16T00:00:00Z" & chlorophyll != NaN)
chla_2020_03 <- chla_2020_03$chlorophyll
chla_2020_03 <- as.numeric(chla_2020_03)
chla_2020_03 <- mean(chla_2020_03)

chla_2020_04 <- chla1 %>% filter(time == "2020-04-16T00:00:00Z" & chlorophyll != NaN)
chla_2020_04 <- chla_2020_04$chlorophyll
chla_2020_04 <- as.numeric(chla_2020_04)
chla_2020_04 <- mean(chla_2020_04)

chla_2020_05 <- chla1 %>% filter(time == "2020-05-16T00:00:00Z" & chlorophyll != NaN)
chla_2020_05 <- chla_2020_05$chlorophyll
chla_2020_05 <- as.numeric(chla_2020_05)
chla_2020_05 <- mean(chla_2020_05)

chla_2020_06 <- chla1 %>% filter(time == "2020-06-16T00:00:00Z" & chlorophyll != NaN)
chla_2020_06 <- chla_2020_06$chlorophyll
chla_2020_06 <- as.numeric(chla_2020_06)
chla_2020_06 <- mean(chla_2020_06)

chla_2020_07 <- chla1 %>% filter(time == "2020-07-16T00:00:00Z" & chlorophyll != NaN)
chla_2020_07 <- chla_2020_07$chlorophyll
chla_2020_07 <- as.numeric(chla_2020_07)
chla_2020_07 <- mean(chla_2020_07)

chla_2020_08 <- chla1 %>% filter(time == "2020-08-16T00:00:00Z" & chlorophyll != NaN)
chla_2020_08 <- chla_2020_08$chlorophyll
chla_2020_08 <- as.numeric(chla_2020_08)
chla_2020_08 <- mean(chla_2020_08)

chla_2020_09 <- chla1 %>% filter(time == "2020-09-16T00:00:00Z" & chlorophyll != NaN)
chla_2020_09 <- chla_2020_09$chlorophyll
chla_2020_09 <- as.numeric(chla_2020_09)
chla_2020_09 <- mean(chla_2020_09)

chla_2020_10 <- chla1 %>% filter(time == "2020-10-16T00:00:00Z" & chlorophyll != NaN)
chla_2020_10 <- chla_2020_10$chlorophyll
chla_2020_10 <- as.numeric(chla_2020_10)
chla_2020_10 <- mean(chla_2020_10)

chla_2020_11 <- chla1 %>% filter(time == "2020-11-16T00:00:00Z" & chlorophyll != NaN)
chla_2020_11 <- chla_2020_11$chlorophyll
chla_2020_11 <- as.numeric(chla_2020_11)
chla_2020_11 <- mean(chla_2020_11)

chla_2020_12 <- chla1 %>% filter(time == "2020-12-16T00:00:00Z" & chlorophyll != NaN)
chla_2020_12 <- chla_2020_12$chlorophyll
chla_2020_12 <- as.numeric(chla_2020_12)
chla_2020_12 <- mean(chla_2020_12)

chla_2020 <- c(chla_2020_01, chla_2020_02, chla_2020_03, chla_2020_04, chla_2020_05, chla_2020_06, chla_2020_07, chla_2020_08, chla_2020_09, chla_2020_10, chla_2020_11, chla_2020_12)

#2021
chla_2021_01 <- chla1 %>% filter(time == "2021-01-16T00:00:00Z" & chlorophyll != NaN)
chla_2021_01 <- chla_2021_01$chlorophyll
chla_2021_01 <- as.numeric(chla_2021_01)
chla_2021_01 <- mean(chla_2021_01)

chla_2021_02 <- chla1 %>% filter(time == "2021-02-16T00:00:00Z" & chlorophyll != NaN)
chla_2021_02 <- chla_2021_02$chlorophyll
chla_2021_02 <- as.numeric(chla_2021_02)
chla_2021_02 <- mean(chla_2021_02)

chla_2021_03 <- chla1 %>% filter(time == "2021-03-16T00:00:00Z" & chlorophyll != NaN)
chla_2021_03 <- chla_2021_03$chlorophyll
chla_2021_03 <- as.numeric(chla_2021_03)
chla_2021_03 <- mean(chla_2021_03)

chla_2021_04 <- chla1 %>% filter(time == "2021-04-16T00:00:00Z" & chlorophyll != NaN)
chla_2021_04 <- chla_2021_04$chlorophyll
chla_2021_04 <- as.numeric(chla_2021_04)
chla_2021_04 <- mean(chla_2021_04)

chla_2021_05 <- chla1 %>% filter(time == "2021-05-16T00:00:00Z" & chlorophyll != NaN)
chla_2021_05 <- chla_2021_05$chlorophyll
chla_2021_05 <- as.numeric(chla_2021_05)
chla_2021_05 <- mean(chla_2021_05)

chla_2021_06 <- chla1 %>% filter(time == "2021-06-16T00:00:00Z" & chlorophyll != NaN)
chla_2021_06 <- chla_2021_06$chlorophyll
chla_2021_06 <- as.numeric(chla_2021_06)
chla_2021_06 <- mean(chla_2021_06)

chla_2021_07 <- chla1 %>% filter(time == "2021-07-16T00:00:00Z" & chlorophyll != NaN)
chla_2021_07 <- chla_2021_07$chlorophyll
chla_2021_07 <- as.numeric(chla_2021_07)
chla_2021_07 <- mean(chla_2021_07)

chla_2021_08 <- chla1 %>% filter(time == "2021-08-16T00:00:00Z" & chlorophyll != NaN)
chla_2021_08 <- chla_2021_08$chlorophyll
chla_2021_08 <- as.numeric(chla_2021_08)
chla_2021_08 <- mean(chla_2021_08)

chla_2021_09 <- chla1 %>% filter(time == "2021-09-16T00:00:00Z" & chlorophyll != NaN)
chla_2021_09 <- chla_2021_09$chlorophyll
chla_2021_09 <- as.numeric(chla_2021_09)
chla_2021_09 <- mean(chla_2021_09)

chla_2021_10 <- chla1 %>% filter(time == "2021-10-16T00:00:00Z" & chlorophyll != NaN)
chla_2021_10 <- chla_2021_10$chlorophyll
chla_2021_10 <- as.numeric(chla_2021_10)
chla_2021_10 <- mean(chla_2021_10)

chla_2021_11 <- chla1 %>% filter(time == "2021-11-16T00:00:00Z" & chlorophyll != NaN)
chla_2021_11 <- chla_2021_11$chlorophyll
chla_2021_11 <- as.numeric(chla_2021_11)
chla_2021_11 <- mean(chla_2021_11)

chla_2021_12 <- chla1 %>% filter(time == "2021-12-16T00:00:00Z" & chlorophyll != NaN)
chla_2021_12 <- chla_2021_12$chlorophyll
chla_2021_12 <- as.numeric(chla_2021_12)
chla_2021_12 <- mean(chla_2021_12)

chla_2021 <- c(chla_2021_01, chla_2021_02, chla_2021_03, chla_2021_04, chla_2021_05, chla_2021_06, chla_2021_07, chla_2021_08, chla_2021_09, chla_2021_10, chla_2021_11, chla_2021_12)

#2022
chla_2022_01 <- chla1 %>% filter(time == "2022-01-16T00:00:00Z" & chlorophyll != NaN)
chla_2022_01 <- chla_2022_01$chlorophyll
chla_2022_01 <- as.numeric(chla_2022_01)
chla_2022_01 <- mean(chla_2022_01)

chla_2022_02 <- chla1 %>% filter(time == "2022-02-16T00:00:00Z" & chlorophyll != NaN)
chla_2022_02 <- chla_2022_02$chlorophyll
chla_2022_02 <- as.numeric(chla_2022_02)
chla_2022_02 <- mean(chla_2022_02)

chla_2022_03 <- chla1 %>% filter(time == "2022-03-16T00:00:00Z" & chlorophyll != NaN)
chla_2022_03 <- chla_2022_03$chlorophyll
chla_2022_03 <- as.numeric(chla_2022_03)
chla_2022_03 <- mean(chla_2022_03)

chla_2022_04 <- chla1 %>% filter(time == "2022-04-16T00:00:00Z" & chlorophyll != NaN)
chla_2022_04 <- chla_2022_04$chlorophyll
chla_2022_04 <- as.numeric(chla_2022_04)
chla_2022_04 <- mean(chla_2022_04)

chla_2022_05 <- chla1 %>% filter(time == "2022-05-16T00:00:00Z" & chlorophyll != NaN)
chla_2022_05 <- chla_2022_05$chlorophyll
chla_2022_05 <- as.numeric(chla_2022_05)
chla_2022_05 <- mean(chla_2022_05)

chla_2022_06 <- chla1 %>% filter(time == "2022-06-16T00:00:00Z" & chlorophyll != NaN)
chla_2022_06 <- chla_2022_06$chlorophyll
chla_2022_06 <- as.numeric(chla_2022_06)
chla_2022_06 <- mean(chla_2022_06)

chla_2022_07 <- chla1 %>% filter(time == "2022-07-16T00:00:00Z" & chlorophyll != NaN)
chla_2022_07 <- chla_2022_07$chlorophyll
chla_2022_07 <- as.numeric(chla_2022_07)
chla_2022_07 <- mean(chla_2022_07)

chla_2022_08 <- chla1 %>% filter(time == "2022-08-16T00:00:00Z" & chlorophyll != NaN)
chla_2022_08 <- chla_2022_08$chlorophyll
chla_2022_08 <- as.numeric(chla_2022_08)
chla_2022_08 <- mean(chla_2022_08)

chla_2022_09 <- chla1 %>% filter(time == "2022-09-16T00:00:00Z" & chlorophyll != NaN)
chla_2022_09 <- chla_2022_09$chlorophyll
chla_2022_09 <- as.numeric(chla_2022_09)
chla_2022_09 <- mean(chla_2022_09)

chla_2022_10 <- chla1 %>% filter(time == "2022-10-16T00:00:00Z" & chlorophyll != NaN)
chla_2022_10 <- chla_2022_10$chlorophyll
chla_2022_10 <- as.numeric(chla_2022_10)
chla_2022_10 <- mean(chla_2022_10)

chla_2022_11 <- chla1 %>% filter(time == "2022-11-16T00:00:00Z" & chlorophyll != NaN)
chla_2022_11 <- chla_2022_11$chlorophyll
chla_2022_11 <- as.numeric(chla_2022_11)
chla_2022_11 <- mean(chla_2022_11)

chla_2022_12 <- chla1 %>% filter(time == "2022-12-16T00:00:00Z" & chlorophyll != NaN)
chla_2022_12 <- chla_2022_12$chlorophyll
chla_2022_12 <- as.numeric(chla_2022_12)
chla_2022_12 <- mean(chla_2022_12)

chla_2022 <- c(chla_2022_01, chla_2022_02, chla_2022_03, chla_2022_04, chla_2022_05, chla_2022_06, chla_2022_07, chla_2022_08, chla_2022_09, chla_2022_10, chla_2022_11, chla_2022_12)

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

#Clean up NaN
chla[chla == NaN] <- NA
pi_full_chla <- chla
pi_pre_chla <- pi_full_chla[,1:6] 
