##################################################
#
#  Bootstrap
#
#
##################################################

# clear console, objects and plots 
cat("\014")  
rm(list = ls())
dev.off()

if (!require(mice)) install.packages('mice')
if (!require(mclust)) install.packages('mclust')
if(!require(tidyverse)) install.packages('tidyverse')
library(mice)
library(mclust)
library(tidyverse)

# functions
source("./code/functions/order_bisgaard_by_PTA.R")
source("./code/functions/load_data_for_clust.R")
source("./code/functions/minMaxScale.R")
source("./code/functions/do_mice.R")
source("./code/functions/bootstrap.R")
source("./code/functions/get_profile_similarity.R")
source("./code/functions/row_cf.R")

set.seed(22)

# set up folders and directories
dir.create(file.path("data/clustering"), recursive = TRUE)  
dir.create(file.path("data/bootstrap_result"), recursive = TRUE)  

path_save <- "data/clustering"
path_plot <- "plots/clustering"

#functions
get_best_model <- function(BICs, imp_max){
  #
  # Get best model plus idx with all corresponding imps 
  #
  # get cluster N prevalence, for first, second and third best BIC----------------
  mod1 <- c()
  mod2 <- c() 
  mod3 <- c()
  
  for (imp in 1:imp_max){
    mod1 <- rbind(mod1,names(BICs[[imp]][1]) )
    mod2 <- rbind(mod2,names(BICs[[imp]][2]) )
    mod3 <- rbind(mod3,names(BICs[[imp]][3]) )
    
  }
  all_models <- rbind(mod1,mod2,mod3)
  table(all_models)
  
  model_k <- as.data.frame(table(all_models))
  
  # get best model
  mod <- as.character(model_k[model_k$Freq %in% max(model_k$Freq),1])
  mod_ <- strsplit(mod, ",")
  cov = as.character(unlist(lapply(mod_, "[[", 1)))
  num_k = as.numeric(unlist(lapply(mod_, "[[", 2)))
  
  # get maximum k, if several are equal
  if (length(num_k) > 1){
  id = num_k %in% max(num_k)
  num_k <- num_k[id]
  cov <- cov[id]
  mod <- mod[id]
  } # if multiple have equal, select first index
  if (length(num_k) >1){
    num_k = num_k[1]
    cov = cov[id]
    mod = mod[id]
  }
  
  model <- data.frame(cov = cov, num_k = num_k, mod = mod)
  
  imps <- rep(1:imp_max, times = 3) # 3 best models
  clusters <- sort(c(imps[all_models == mod]), decreasing = FALSE)
  
  out <- list(impsets = clusters,
              model = model)
  return(out)
}

#----------------------------
text_size = 17

set.seed(22)
BICs <- list()
all_bics_EEI <- c()
all_bics_VEI <- c()
#---------------------------------

dat_with_missing <- load_data_for_clust()

if (file.exists("data/seeds.Rdata")){
  load("data/seeds.Rdata")
} else {
  # set reproducible seeds
  seeds <- sample(1:10000, 1000, replace = FALSE)
  save(seeds, file= "data/seeds.Rdata")
}

#---BOOTSTRAP ------------------------------------------------------------------
for (b in 1:1000){
  
  set.seed(seeds[b])
  df <- bootstrap(dat_with_missing, "subsample")
  
  # IMPUTATION -------------------------------------------------------------------
  # impute bootstrapped data
  mi.res <- do_mice(df, b)
  
  # GET OPTIMAL K ACROSS COMPLETED DATASETS --------------------------------------
  # initialize vars
  BICs_EEI <- matrix(data = NA, ncol = 20, nrow = 29)
  BICs_VEI <- matrix(data = NA, ncol = 20, nrow = 29)
  BICs <- c()
  for (imp in 1:20){
    
    data <- mice::complete(mi.res, action = imp)
    dat <- minMaxScale(data)
    
    set.seed(22)
    BIC <- mclustBIC(dat, G = 2:30, modelNames = c("EII", "VII", "EEI", "VEI"))
    print(summary(BIC))
    
    # store k vars
    tmp <- summary(BIC) 
    BICs[[imp]] <- tmp[1:3]  
    BICs_EEI[,imp] <- BIC[,3]
    BICs_VEI[,imp] <- BIC[,4]
    
  }
  
  mod <- get_best_model(BICs, imp_max = 20)
  
  # GENERATE PROFILES WITH OPTIMAL K & COV ---------------------------------------
  
  classification = c() 
  
  for (i in mod$impsets){
    
    dat <- mice::complete(mi.res, action = i)
    c = mod$model$num_k
    set.seed(22)
    mc <- Mclust(dat, G =c, modelNames = as.character(mod$model$cov))
    tmp <- summary(mc)
    
    classification <- cbind(classification, mc$classification)
    
    save(mc, file= paste0("data/clustering/mc_bootstrap_",b,"_imp_", i,".Rdata"))
    
  }
  
  # GET PROFILE SIMILARITY -------------------------------------------------------
  sim <- get_profile_similarity(classification, mc, mod$impsets, overall_similarity=FALSE)
  
  result <- data.frame(k = mod$model$num_k, 
                       similarity = sim)
  
  save(result, mod, mi.res, BICs_EEI, BICs_VEI, classification, file = paste0("data/bootstrap_result/results_", b, ".Rdata"))
  
  print(paste("Boot:", b, " K:", result$k, " Sim:", sim))
}



