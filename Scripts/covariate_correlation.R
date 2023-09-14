install.packages("ggcorrplot")
library(here)
library(ggcorrplot)
library(tidyverse)

###
#Ensure same dimensions
chla_dim <- pi_full_chla
sst_dim <- pi_full_sst[3:28,]
npgo_dim <- pi_full_npgo[3:28,]
pdo_dim <- pi_full_pdo[3:28,]

###############################################
#SST vs others

cor_sst_pdo <- cor(x = sst_dim, y = pdo_dim,
                   use = "complete.obs", method = "pearson") #using complete.obs uses casewise deletion in the event of missing values/NAs

cor_sst_npgo <- cor(x = sst_dim, y = npgo_dim,
                    use = "complete.obs", method = "pearson")

cor_sst_chla <- cor(x = sst_dim, y = chla_dim,
                    use = "complete.obs", method = "pearson")

#PDO vs others

cor_pdo_npgo <- cor(x = pdo_dim, y = npgo_dim,
                    use = "complete.obs", method = "pearson")

cor_pdo_chla <- cor(x = pdo_dim, y = chla_dim,
                    use = "complete.obs", method = "pearson")

#Chla vs others

cor_chla_npgo <- cor(x = chla_dim, y = npgo_dim,
                     use = "complete.obs", method = "pearson")

###############################################




ggcorrplot(cor_sst_pdo)
ggcorrplot(cor3) 
