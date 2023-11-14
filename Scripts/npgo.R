library(here)
library(tidyverse)

npgo <- read.csv(here("Data", "NPGO.csv"))                

#####
#NPGO setup
pi_full_npgo <- npgo[which(npgo$YEAR >= 1995),]

#Need to make NPGO same dimensions as other covariates (29x13)
npgo_col_head <- c("Year", "Jan", "Feb", "Mar", "Apr", "May", "June", "Jul", "Aug", "Sept", "Oct", "Nov", "Dec")
npgo_row_head <- seq(1995, 2023, 1)

npgo_95 <- pi_full_npgo[1:12,3]
npgo_96 <- pi_full_npgo[13:24,3]
npgo_97 <- pi_full_npgo[25:36,3]
npgo_98 <- pi_full_npgo[37:48,3]
npgo_99 <- pi_full_npgo[49:60,3]
npgo_00 <- pi_full_npgo[61:72,3]
npgo_01 <- pi_full_npgo[73:84,3]
npgo_02 <- pi_full_npgo[85:96,3]
npgo_03 <- pi_full_npgo[97:108,3]
npgo_04 <- pi_full_npgo[109:120,3]
npgo_05 <- pi_full_npgo[121:132,3]
npgo_06 <- pi_full_npgo[133:144,3]
npgo_07 <- pi_full_npgo[145:156,3]
npgo_08 <- pi_full_npgo[157:168,3]
npgo_09 <- pi_full_npgo[169:180,3]
npgo_10 <- pi_full_npgo[181:192,3]
npgo_11 <- pi_full_npgo[193:204,3]
npgo_12 <- pi_full_npgo[205:216,3]
npgo_13 <- pi_full_npgo[217:228,3]
npgo_14 <- pi_full_npgo[229:240,3]
npgo_15 <- pi_full_npgo[241:252,3]
npgo_16 <- pi_full_npgo[253:264,3]
npgo_17 <- pi_full_npgo[265:276,3]
npgo_18 <- pi_full_npgo[277:288,3]
npgo_19 <- pi_full_npgo[289:300,3]
npgo_20 <- pi_full_npgo[301:312,3]
npgo_21 <- pi_full_npgo[313:324,3]
npgo_22 <- pi_full_npgo[325:336,3]
npgo_23 <- pi_full_npgo[337:348,3]

pi_full_npgo <- rbind(npgo_95, npgo_96, npgo_97, npgo_98, npgo_99, npgo_00, npgo_01, npgo_02, npgo_03, npgo_04, npgo_05, npgo_06, npgo_07, npgo_08, npgo_09, npgo_10, npgo_11, npgo_12, npgo_13, npgo_14, npgo_15, npgo_16, npgo_17, npgo_18, npgo_19, npgo_20, npgo_21, npgo_22, npgo_23)
rownames(pi_full_npgo) <- seq(1, 29, 1)
pi_full_npgo <- cbind(npgo_row_head, pi_full_npgo)
colnames(pi_full_npgo) <- npgo_col_head

####
#NPGO full-year; Jan-Dec
npgo_holder <- rep(NA, nrow(pi_full_npgo))

pi_full_npgo <- cbind(pi_full_npgo, npgo_holder)
pi_full_npgo <- as.data.frame(pi_full_npgo)
pi_full_npgo$npgo_holder <- rowMeans(pi_full_npgo[,2:13], na.rm=TRUE)
year_1_npgo <- pi_full_npgo[,c(1, 14)]
colnames(year_1_npgo) <- c("Year", "NPGO")

####
#NPGO full-year; May(t-1)-April(t)
full96 <- cbind(pi_full_npgo[1,6:13], pi_full_npgo[2,2:5])
full97 <- cbind(pi_full_npgo[2,6:13], pi_full_npgo[3,2:5])
full98 <- cbind(pi_full_npgo[3,6:13], pi_full_npgo[4,2:5])
full99 <- cbind(pi_full_npgo[4,6:13], pi_full_npgo[5,2:5])
full00 <- cbind(pi_full_npgo[5,6:13], pi_full_npgo[6,2:5])
full01 <- cbind(pi_full_npgo[6,6:13], pi_full_npgo[7,2:5])
full02 <- cbind(pi_full_npgo[7,6:13], pi_full_npgo[8,2:5])
full03 <- cbind(pi_full_npgo[8,6:13], pi_full_npgo[9,2:5])
full04 <- cbind(pi_full_npgo[9,6:13], pi_full_npgo[10,2:5])
full05 <- cbind(pi_full_npgo[10,6:13], pi_full_npgo[11,2:5])
full06 <- cbind(pi_full_npgo[11,6:13], pi_full_npgo[12,2:5])
full07 <- cbind(pi_full_npgo[12,6:13], pi_full_npgo[13,2:5])
full08 <- cbind(pi_full_npgo[13,6:13], pi_full_npgo[14,2:5])
full09 <- cbind(pi_full_npgo[14,6:13], pi_full_npgo[15,2:5])
full10 <- cbind(pi_full_npgo[15,6:13], pi_full_npgo[16,2:5])
full11 <- cbind(pi_full_npgo[16,6:13], pi_full_npgo[17,2:5])
full12 <- cbind(pi_full_npgo[17,6:13], pi_full_npgo[18,2:5])
full13 <- cbind(pi_full_npgo[18,6:13], pi_full_npgo[19,2:5])
full14 <- cbind(pi_full_npgo[19,6:13], pi_full_npgo[20,2:5])
full15 <- cbind(pi_full_npgo[20,6:13], pi_full_npgo[21,2:5])
full16 <- cbind(pi_full_npgo[21,6:13], pi_full_npgo[22,2:5])
full17 <- cbind(pi_full_npgo[22,6:13], pi_full_npgo[23,2:5])
full18 <- cbind(pi_full_npgo[23,6:13], pi_full_npgo[24,2:5])
full19 <- cbind(pi_full_npgo[24,6:13], pi_full_npgo[25,2:5])
full20 <- cbind(pi_full_npgo[25,6:13], pi_full_npgo[26,2:5])
full21 <- cbind(pi_full_npgo[26,6:13], pi_full_npgo[27,2:5])
full22 <- cbind(pi_full_npgo[27,6:13], pi_full_npgo[28,2:5])
full23 <- cbind(pi_full_npgo[28,6:13], pi_full_npgo[29,2:5])

pi_year_rnames <- seq(1996, 2023, 1)
year_1_npgo <- rbind(full96, full97, full98, full99, full00, full01, full02, full03, full04, full05, full06, full07, full08, full09, full10, full11, full12, full13, full14, full15, full16, full17, full18, full19, full20, full21, full22, full23)
year_1_npgo <- cbind(pi_year_rnames, year_1_npgo)
year_1_npgo <- cbind(year_1_npgo, npgo_holder[1:28])

year_1_npgo$`npgo_holder[1:28]` <- rowMeans(year_1_npgo[,2:13], na.rm=TRUE)
year_1_npgo <- year_1_npgo[,c(1,14)]
colnames(year_1_npgo) <- c("Year", "NPGO")

#NPGO full-year; Oct(t-1)-Sept(t)
full.2.96 <- cbind(pi_full_npgo[1,11:13], pi_full_npgo[2,2:10])
full.2.97 <- cbind(pi_full_npgo[2,11:13], pi_full_npgo[3,2:10])
full.2.98 <- cbind(pi_full_npgo[3,11:13], pi_full_npgo[4,2:10])
full.2.99 <- cbind(pi_full_npgo[4,11:13], pi_full_npgo[5,2:10])
full.2.00 <- cbind(pi_full_npgo[5,11:13], pi_full_npgo[6,2:10])
full.2.01 <- cbind(pi_full_npgo[6,11:13], pi_full_npgo[7,2:10])
full.2.02 <- cbind(pi_full_npgo[7,11:13], pi_full_npgo[8,2:10])
full.2.03 <- cbind(pi_full_npgo[8,11:13], pi_full_npgo[9,2:10])
full.2.04 <- cbind(pi_full_npgo[9,11:13], pi_full_npgo[10,2:10])
full.2.05 <- cbind(pi_full_npgo[10,11:13], pi_full_npgo[11,2:10])
full.2.06 <- cbind(pi_full_npgo[11,11:13], pi_full_npgo[12,2:10])
full.2.07 <- cbind(pi_full_npgo[12,11:13], pi_full_npgo[13,2:10])
full.2.08 <- cbind(pi_full_npgo[13,11:13], pi_full_npgo[14,2:10])
full.2.09 <- cbind(pi_full_npgo[14,11:13], pi_full_npgo[15,2:10])
full.2.10 <- cbind(pi_full_npgo[15,11:13], pi_full_npgo[16,2:10])
full.2.11 <- cbind(pi_full_npgo[16,11:13], pi_full_npgo[17,2:10])
full.2.12 <- cbind(pi_full_npgo[17,11:13], pi_full_npgo[18,2:10])
full.2.13 <- cbind(pi_full_npgo[18,11:13], pi_full_npgo[19,2:10])
full.2.14 <- cbind(pi_full_npgo[19,11:13], pi_full_npgo[20,2:10])
full.2.15 <- cbind(pi_full_npgo[20,11:13], pi_full_npgo[21,2:10])
full.2.16 <- cbind(pi_full_npgo[21,11:13], pi_full_npgo[22,2:10])
full.2.17 <- cbind(pi_full_npgo[22,11:13], pi_full_npgo[23,2:10])
full.2.18 <- cbind(pi_full_npgo[23,11:13], pi_full_npgo[24,2:10])
full.2.19 <- cbind(pi_full_npgo[24,11:13], pi_full_npgo[25,2:10])
full.2.20 <- cbind(pi_full_npgo[25,11:13], pi_full_npgo[26,2:10])
full.2.21 <- cbind(pi_full_npgo[26,11:13], pi_full_npgo[27,2:10])
full.2.22 <- cbind(pi_full_npgo[27,11:13], pi_full_npgo[28,2:10])
full.2.23 <- cbind(pi_full_npgo[28,11:13], pi_full_npgo[29,2:10])

year_2_npgo <- rbind(full.2.96, full.2.97, full.2.98, full.2.99, full.2.00, full.2.01, full.2.02, full.2.03, full.2.04, full.2.05, full.2.06, full.2.07, full.2.08, full.2.09, full.2.10, full.2.11, full.2.12, full.2.13, full.2.14, full.2.15, full.2.16, full.2.17, full.2.18, full.2.19, full.2.20, full.2.21, full.2.22, full.2.23)
year_2_npgo <- cbind(pi_year_rnames, year_2_npgo)
year_2_npgo <- cbind(year_2_npgo, npgo_holder[1:28])

year_2_npgo$`npgo_holder[1:28]` <- rowMeans(year_2_npgo[,2:13], na.rm=TRUE)
year_2_npgo <- year_2_npgo[,c(1,14)]
colnames(year_2_npgo) <- c("Year", "NPGO")

####
#NPGO pre-breeding season; Jan-Apr
pre_npgo <- cbind(pi_full_npgo[,1:5], npgo_holder)
pre_npgo$npgo_holder <- rowMeans(pre_npgo[,2:5], na.rm=TRUE)
pre_npgo <- pre_npgo[,c(1,6)]
colnames(pre_npgo) <- c("Year", "NPGO")

####
#NPGO breeding season; May - Sept
breed_npgo <- cbind(pi_full_npgo[,c(1,6:10)], npgo_holder)
breed_npgo$npgo_holder <- rowMeans(breed_npgo[,2:6], na.rm=TRUE)
breed_npgo <- breed_npgo[,c(1,7)]
colnames(breed_npgo) <- c("Year", "NPGO")

####
#winter; Oct-Mar
winter96 <- cbind(pi_full_npgo[1,11:13], pi_full_npgo[2,2:4])
winter97 <- cbind(pi_full_npgo[2,11:13], pi_full_npgo[3,2:4])
winter98 <- cbind(pi_full_npgo[3,11:13], pi_full_npgo[4,2:4])
winter99 <- cbind(pi_full_npgo[4,11:13], pi_full_npgo[5,2:4])
winter00 <- cbind(pi_full_npgo[5,11:13], pi_full_npgo[6,2:4])
winter01 <- cbind(pi_full_npgo[6,11:13], pi_full_npgo[7,2:4])
winter02 <- cbind(pi_full_npgo[7,11:13], pi_full_npgo[8,2:4])
winter03 <- cbind(pi_full_npgo[8,11:13], pi_full_npgo[9,2:4])
winter04 <- cbind(pi_full_npgo[9,11:13], pi_full_npgo[10,2:4])
winter05 <- cbind(pi_full_npgo[10,11:13], pi_full_npgo[11,2:4])
winter06 <- cbind(pi_full_npgo[11,11:13], pi_full_npgo[12,2:4])
winter07 <- cbind(pi_full_npgo[12,11:13], pi_full_npgo[13,2:4])
winter08 <- cbind(pi_full_npgo[13,11:13], pi_full_npgo[14,2:4])
winter09 <- cbind(pi_full_npgo[14,11:13], pi_full_npgo[15,2:4])
winter10 <- cbind(pi_full_npgo[15,11:13], pi_full_npgo[16,2:4])
winter11 <- cbind(pi_full_npgo[16,11:13], pi_full_npgo[17,2:4])
winter12 <- cbind(pi_full_npgo[17,11:13], pi_full_npgo[18,2:4])
winter13 <- cbind(pi_full_npgo[18,11:13], pi_full_npgo[19,2:4])
winter14 <- cbind(pi_full_npgo[19,11:13], pi_full_npgo[20,2:4])
winter15 <- cbind(pi_full_npgo[20,11:13], pi_full_npgo[21,2:4])
winter16 <- cbind(pi_full_npgo[21,11:13], pi_full_npgo[22,2:4])
winter17 <- cbind(pi_full_npgo[22,11:13], pi_full_npgo[23,2:4])
winter18 <- cbind(pi_full_npgo[23,11:13], pi_full_npgo[24,2:4])
winter19 <- cbind(pi_full_npgo[24,11:13], pi_full_npgo[25,2:4])
winter20 <- cbind(pi_full_npgo[25,11:13], pi_full_npgo[26,2:4])
winter21 <- cbind(pi_full_npgo[26,11:13], pi_full_npgo[27,2:4])
winter22 <- cbind(pi_full_npgo[27,11:13], pi_full_npgo[28,2:4])
winter23 <- cbind(pi_full_npgo[28,11:13], pi_full_npgo[29,2:4])

pi_winter_rnames <- seq(1996, 2023, 1)
winter_npgo <- rbind(winter96, winter97, winter98, winter99, winter00, winter01, winter02, winter03, winter04, winter05, winter06, winter07, winter08, winter09, winter10, winter11, winter12, winter13, winter14, winter15, winter16, winter17, winter18, winter19, winter20, winter21, winter22, winter23)
winter_npgo <- cbind(pi_winter_rnames, winter_npgo)
winter_npgo <- cbind(winter_npgo, npgo_holder[1:28])

winter_npgo$`npgo_holder[1:28]` <- rowMeans(winter_npgo[,2:7], na.rm=TRUE)
winter_npgo <- winter_npgo[,c(1,8)]
colnames(winter_npgo) <- c("Year", "NPGO")

####
#clean things up in environment
rm(npgo)
rm(pi_full_npgo)
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
rm(npgo_holder)
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
rm(npgo_95)
rm(npgo_96)
rm(npgo_97)
rm(npgo_98)
rm(npgo_99)
rm(npgo_00)
rm(npgo_01)
rm(npgo_02)
rm(npgo_03)
rm(npgo_04)
rm(npgo_05)
rm(npgo_06)
rm(npgo_07)
rm(npgo_08)
rm(npgo_09)
rm(npgo_10)
rm(npgo_11)
rm(npgo_12)
rm(npgo_13)
rm(npgo_14)
rm(npgo_15)
rm(npgo_16)
rm(npgo_17)
rm(npgo_18)
rm(npgo_19)
rm(npgo_20)
rm(npgo_21)
rm(npgo_22)
rm(npgo_23)
rm(npgo_col_head)
rm(npgo_row_head)
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
