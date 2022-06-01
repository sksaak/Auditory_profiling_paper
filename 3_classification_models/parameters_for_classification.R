
set_parameters_for_classification = function(){

# libraries
if (!require(rlist)) install.packages('rlist')
if (!require(randomForest)) install.packages('randomForest')
if(!require(caret)) install.packages('caret')
if(!require(caTools)) install.packages('caTools')
library(rlist)
library(randomForest)
library(caret)
library(caTools)

#
source("code/functions/classification_functions.R")

# load data ----------------
load("data/final_impset.Rdata")
load(paste0("data/clustering/mc_", selected_imp, ".Rdata"))

# SEEDS for repeated 10-fold CV
set.seed(22)
seeds <- vector(mode = "list", length = 101)
for(i in 1:100) seeds[[i]] <- sample.int(1000, 12) 
## For the last model:
seeds[[101]] <- sample.int(1000, 1)


# FITCONTROLS -----------------------------------------------------------------
fitControls = list()

# AUPRC 
fitControls[[1]] <- trainControl(method = "repeatedcv",
                                 number = 10,
                                 repeats = 10,
                                 verboseIter = TRUE,
                                 seeds = seeds,
                                 summaryFunction = auprcSummary,
                                 classProbs = TRUE)

#kappa
fitControls[[2]] <- trainControl(method = "repeatedcv",
                                 number = 10,
                                 repeats = 10,
                                 verboseIter = TRUE,
                                 seeds = seeds
)
#balanced_accuracy
fitControls[[3]] <- trainControl(method = "repeatedcv",
                                 number = 10,
                                 repeats = 10,
                                 verboseIter = TRUE,
                                 seeds = seeds,
                                 summaryFunction = balanced_accuracy
)
#F1
fitControls[[4]]  <- trainControl(method = "repeatedcv",
                                  number = 10,
                                  repeats = 10,
                                  verboseIter = TRUE,
                                  seeds = seeds,
                                  summaryFunction = f1,
)
# AUPRC 
fitControls[[5]] <- trainControl(method = "LOOCV",
                                 verboseIter = TRUE,
                                 summaryFunction = auprcSummary,
                                 classProbs = TRUE)
#kappa
fitControls[[6]] <- trainControl(method = "LOOCV",
                                 verboseIter = FALSE
)

#balanced_accuracy
fitControls[[7]] <- trainControl(method = "LOOCV",
                                 verboseIter = TRUE,
                                 summaryFunction = balanced_accuracy
)
#F1
fitControls[[8]]  <- trainControl(method = "LOOCV",
                                  verboseIter = TRUE,
                                  summaryFunction = f1,
)
trainMet = rep(c("AUPRC","Kappa", "balanced_accuracy", "F1"), times = 2)

# make feature groups ----------------------------------------------------------
data$profile <- as.factor(mc$classification)

vhc <-  data.frame(profile = data$profile, 
                   goesa_slope=data$goesa_slope, goesa_srt=data$goesa_srt,
                   ag_ac_pta = data$ag_ac_pta, ag_asym = data$ag_asym, bisgaard=data$bisgaard,
                   acalos_4_L15=data$acalos_4_L15,
                   acalos_4_diff=data$acalos_diff_4,
                   acalos_4_L35=data$acalos_4_L35,
                   acalos_1_5_L15=data$acalos_1_5_L15,
                   acalos_1_5_diff=data$acalos_diff_1_5,
                   acalos_1_5_L35=data$acalos_1_5_L35,
                   age = data$age)

vhc <- vhc[mc$uncertainty < 0.4,]
unc <- vhc[mc$uncertainty >= 0.4,]

# train and test set
set.seed(22)
tt_set <- sample.split(vhc$profile, SplitRatio = 0.75)

train_data <- vhc[tt_set,]
test_data <- vhc[!tt_set,]

table(train_data$profile)
table(test_data$profile)

min_n = max(table(train_data$profile))


save(tt_set,seeds, file = "data/tt_set.Rdata")
save(fitControls, trainMet, train_data, test_data, min_n,
     file ="data/train_params.Rdata")

}

