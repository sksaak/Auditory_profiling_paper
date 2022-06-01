#
# Build classification models with Random forest
#
# for Multiclass, OVA, OVO, OVAOVO
# (Kappa, AUPRC, Balanced accuracy, F1-Score)
# (LOOCV, RepCV)
#
####################################################

# clear console, objects and plots 
cat("\014")  
rm(list = ls())
dev.off()

# run scripts

source("./code/3_classification_models/parameters_for_classification.R")
source("./code/3_classification_models/multiclass_train.R")
source("./code/3_classification_models/ova_train.R")
source("./code/3_classification_models/ovo_train.R")

# set parameters for classification 
set_parameters_for_classification()

# run models 
multiclass_train()
ova_train()
ovo_train()

# finished model run
print("ALL MODEL TRAINING FINISHED")



