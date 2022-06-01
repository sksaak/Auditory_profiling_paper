##################################################
#
#  Clustering
#
#
##################################################

# clear console, objects and plots 
cat("\014")  
rm(list = ls())
dev.off()

# libraries
if (!require(mice)) install.packages('mice')
if (!require(mclust)) install.packages('mclust')
if(!require(tidyverse)) install.packages('tidyverse')
library(mice)
library(mclust)
library(tidyverse)

set.seed(22)

#functions
source("./code/functions/order_bisgaard_by_PTA.R")
source("./code/functions/minMaxScale.R")
source("./code/functions/do_mice.R")
source("./code/functions/load_data_for_clust.R")


set.seed(22)
#---------------------------------

dat_with_missing <- load_data_for_clust()
load("./data/bootstrap_results.Rdata")

# IMPUTATION -------------------------------------------------------------------
# impute bootstrapped data
mi.res <- do_mice(dat_with_missing, savenumber = "final_check")

# CLUSTERING -------------------------------------------------------------------
classification = c() 
uncertainty =  c()
for (imp in 1:20){
  
  data <- mice::complete(mi.res, action = imp)
  dat <- minMaxScale(data)
  
  set.seed(22)
  mc <- Mclust(dat, G =final_k, modelNames = final_cov)
  tmp <- summary(mc)
  
  uncertainty <- cbind(uncertainty, mc$uncertainty)
  
  
  classification <- cbind(classification, mc$classification)
  
  save(data, mc, file = paste0("./data/clustering/mc_", imp, ".Rdata"))
  
}

# check certainties of classifications
x = round(rowMeans(uncertainty), digits = 3)
uncertain = x > 0.2
classification = cbind(classification,uncertain)
classification = cbind(classification,x)
cert_class = classification[!uncertain,]
sim = get_profile_similarity(cert_class[,1:20], mc, impsets = 1:20)
sim2 = get_profile_similarity(classification[,1:20], mc, impsets = 1:20)








