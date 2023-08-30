install.packages("devtools")
install.packages("ggpubr")
install.packages("ggcorrplot")
library("ggpubr")
library(here)
library(ggcorrplot)

sst <- read.csv(here("Data", "racerocks_mSST.csv"))
npgo <- read.csv(here("Data", "NPGO.csv"))                
pdo <- read.csv(here("Data", "PDO.csv"))

head(sst)
sst[sst == 999.99] <- NA #wouldn't want a value like that guiding our analyses
pi_sst <- sst[which(sst$YEAR >= 1995),] #isolate years of interest

pi_npgo <- npgo[which(npgo$YEAR >= 1995),]

pi_pdo <- pdo[which(pdo$Year >= 1995),]

data <- array()
#############################################
#pi_sst and pi_pdo have the same dimensions, allowing me to check them like this. 
#pi_npgo does not have the same dimensions, so i'm running into trouble using this command to check for correlation.

cor1<- cor(x = pi_sst, y = pi_pdo,
    use = "complete.obs", method = "pearson") #using complete.obs uses casewise deletion in the event of missing values/NAs

cor2 <- cor(x = pi_sst, y = pi_pdo,
    use = "na.or.complete", method = "pearson") #same output as above; it is erring on side of complete obs 

cor3 <- cor(x = pi_sst, y = pi_pdo,
             use = "pairwise.complete.obs", method = "pearson") #corr calculated based on all compelete pairs... not sure what is fundamentally different, but output differs.


ggcorrplot(cor1)
ggcorrplot(cor3)
