install.packages("devtools")
install.packages("ggpubr")
install.packages("ggcorrplot")
install.packages("satin")
library(ggpubr)
library(here)
library(ggcorrplot)
library(tidyverse)
library(satin)

sst <- read.csv(here("Data", "racerocks_mSST.csv"))
npgo <- read.csv(here("Data", "NPGO.csv"))                
pdo <- read.csv(here("Data", "PDO.csv"))

####
#SST
sst[sst == 999.99] <- NA #wouldn't want a value like that guiding our analyses
pi_full_sst <- sst[which(sst$YEAR >= 1995),] #isolate years of interest
pi_pre_sst <- pi_full_sst[,1:6] #pre-breeding season defined as Jan. 1 - May 31

####
#PDO
pi_full_pdo <- pdo[which(pdo$Year >= 1995),]
pi_pre_pdo <- pi_full_pdo[,1:6]

#####
#NPGO
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

pi_pre_npgo <- pi_full_npgo[,1:6]

#####
#ChlA



###############################################
#pi_sst and pi_pdo have the same dimensions, allowing me to check them like this. 

cor_sst_pdo <- cor(x = pi_full_sst, y = pi_full_pdo,
                   use = "complete.obs", method = "pearson") #using complete.obs uses casewise deletion in the event of missing values/NAs

cor_sst_npgo <- cor(x = pi_full_sst, y = pi_full_npgo,
                    use = "complete.obs", method = "pearson")

cor_npgo_pdo <- cor(x = pi_full_npgo, y = pi_full_pdo,
                    use = "complete.obs", method = "pearson")






ggcorrplot(cor_sst_pdo)
ggcorrplot(cor3) 
