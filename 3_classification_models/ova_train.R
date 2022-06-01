ova_train = function(){
  
# libraries
if (!require(randomForest)) install.packages('randomForest')
if(!require(caret)) install.packages('caret')
library(randomForest)
library(caret)

set.seed(22)

# functions
source("code/functions/classification_functions.R")

# load data ----------------
load("data/train_params.Rdata")

# general vars
i_mat <- logmat(2)

rfGrid = mtryGrid(train_data)

# LOOP ACROSS PROFILES -------------------------------------------------------
for (ap in 1:13){
  
  # AP1
  ap_dat <- train_data
  ap_dat$profile <- as.numeric(ap_dat$profile)
  ap_dat$profile[ap_dat$profile != ap] = "neg"
  ap_dat$profile[ap_dat$profile == ap] = "pos"
  ap_dat$profile <- factor(ap_dat$profile)
  
  new_data = balance(ap_dat, method = "upsample_with_noise", levels = c("pos","neg"), min_n = min_n)

  ap_dat = rbind(ap_dat, new_data)

  # shuffle rows
  ap_dat= ap_dat[sample(1:nrow(ap_dat)), ]

  model_weights <- ifelse(ap_dat$profile == "neg",
                          (1/table(ap_dat$profile)[1]) * 0.5,
                          (1/table(ap_dat$profile)[2]) * 0.5)

# Loop across different evaluation metrics
for (m in c(1,2,3,4,5,6,7,8)){ 
  
  if (m %in% c(1,2,3,4)){cv = "repeatedCV"
  }else if (m %in% c(5,6,7,8)){cv = "LOOCV"}

 
    # random forest model 
    model<- train(profile ~ ., data = ap_dat, 
                            method = "rf", 
                            trControl = fitControls[[m]],
                            ntree = c(500),
                            weights = model_weights,
                            tuneGrid = rfGrid,
                            metric = trainMet[m], 
                            verbose = TRUE
    )


    # train prediction ---------------------------------------------- 
    conf <- model$finalModel$confusion
    miss <- conf[i_mat]
    train_metric = get_metric(conf)
    
    # test prediction ----------------------------------------------
    test_data_ap = test_data
    test_data_ap$profile <- as.numeric(test_data_ap$profile)
    test_data_ap$profile[test_data_ap$profile != ap] = "neg"
    test_data_ap$profile[test_data_ap$profile == ap] = "pos"
    test_data_ap$profile <- factor(test_data_ap$profile)
    
    pred <- predict(model, newdata=test_data_ap)
    tmp <- caret::confusionMatrix(pred, reference = test_data_ap$profile)
    conf = tmp$table
    test_metric = get_metric(conf)

    metrics <- rbind(train_metric, test_metric)
    metrics$cond = c("train","test")
    
    
    print(paste("AP:", ap, "    CV:", cv, "    Metric:", trainMet[m] ))
    print(conf)
    
    # set up folders and directories
    dir.create(file.path("data/classification/ova/"), recursive = TRUE)  
    
    save(model, file = paste0("data/classification/ova/Model_",
                              cv,"_", trainMet[m],"_",ap, "_", cond_name ,".Rdata"))
    
    save(metrics, file = paste0("data/classification/ova/Metrics_",
                                cv,"_",trainMet[m],"_",ap,"_",cond_name ,".Rdata"))
    # delete for storage
    rm(model, metrics)
    }
 
} 

}