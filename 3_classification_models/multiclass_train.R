multiclass_train = function(){

# libraries
if (!require(randomForest)) install.packages('randomForest')
if(!require(caret)) install.packages('caret')
library(randomForest)
library(caret)

set.seed(22)
# functions
source("code/functions/classification_functions.R")

multiMetrics_fromConfMatrix <- function(conf){
  
  classes = c(1:nrow(conf))
  
  metrics = matrix(data = NA, nrow = nrow(conf), ncol = 5)
  
  for (idx in classes){
    
    TP = conf[idx,idx]
    FP = sum(conf[classes[classes != idx],idx])
    TN = sum(conf[classes[classes != idx], classes[classes != idx]])
    FN = sum(conf[idx, classes[classes != idx]])
    
    sens = TP / (TP + FN)
    spec = TN / (TN + FN)
    prec = TP / (TP + FP)
    f1 = 2*TP / (2*TP + FP + FN)
    
    met = c(idx, sens, spec, prec, f1)
    
    metrics[idx,] = met
  }
  
  metrics = as.data.frame(metrics)
  colnames(metrics) = c("ap", "sens", "spec", "prec", "f1")
  
  return(metrics)
}

# load parameters
load("data/train_params.Rdata")


text_size = 17
i_mat <- logmat(13)


fitControls[[1]] <- trainControl(method = "repeatedcv",
                                 number = 10,
                                 repeats = 10,
                                 verboseIter = TRUE,
                                 seeds = fitControls[[1]]$seeds,
                                 summaryFunction = AUPRC_multi,
                                 classProbs = TRUE)

#----------------------------------------------------------

# generate data with upsampling and added noise
class_to_upsample <-as.character(which(table(train_data$profile) < min_n))
new_data <- upsample_with_gaussian_noise(train_data, train_data$profile, min_n = min_n)

# bind new to old
compare_df <- rbind(train_data, new_data)
compare_df$idx <- c(rep("original", times = nrow(train_data)), rep("new", times = nrow(new_data)))
compare_df$age <- round(compare_df$age, 0)

# shuffle rows
compare_df= compare_df[sample(1:nrow(compare_df)), ]

# # tuneGrids--------------------------------------------------
rfGrid <- mtryGrid(train_data)

train_data$profile <- as.factor(paste("class_", train_data$profile, sep=""))
test_data$profile = as.factor(paste("class_", test_data$profile, sep=""))

for (m in c(1,2,3,4,5,6,7,8)){
  if (m %in% c(1, 2, 3, 4)) {cv = "repeatedCV"
  } else if (m %in% c(5, 6, 7, 8)) {cv = "LOOCV" }
  
 compare_df$profile <- as.factor(paste("class_", compare_df$profile, sep=""))


  model <- train(profile ~ .,data = compare_df[, 1:(ncol(compare_df) - 1)],
      method = "rf",
      trControl = fitControls[[m]],
      ntree = c(500),
      tuneGrid = rfGrid,
      metric = trainMet[m],
      verbose = TRUE
      )

  # train prediction ----------------------------------------------
  pred = model$finalModel$predicted
  
  conf = model$finalModel$confusion[,1:13]
  # rearrange conf order to match increasing profile number 
  ctmp = conf[c(-2,-3,-4,-5),2:5]
  rtmp = conf[2:5,]
  rtmp = cbind(rtmp[,c(1,6:13)], rtmp[,2:5])
  conf = conf[c(-2,-3,-4,-5),c(-2,-3,-4,-5)]
  conf  = cbind(conf, ctmp)
  conf = rbind(conf, rtmp)
  
  train_metric = multiMetrics_fromConfMatrix(conf)
  
  # test prediction ----------------------------------------------
  

  pred <- predict(model, newdata = test_data)
  tmp <- caret::confusionMatrix(pred, reference = test_data$profile)
  conf = tmp$table
  tmp <- tmp[["byClass"]]
  
  test_metric = data.frame(ap = c(1:13),
                            sens = tmp[,1],
                            spec = tmp[,2],
                            prec = tmp[,5],
                            f1   = tmp[,7])
  
  
  metrics <- rbind(train_metric, test_metric)
  metrics$cond = rep(c("train", "test"), each = 13)
  print(paste("CV:", cv, "    Metric:", trainMet[m] ))
  print(conf)
  

  # set up folders and directories
  dir.create(file.path("data/classification/multiclass/"), recursive = TRUE)  
  
  # save
  save(model,file = paste0("data/classification/multiclass/Model_",
      cv,"_",trainMet[m],"_",cond_name,".Rdata"))
  
  save(metrics, file = paste0("data/classification/multiclass/Metric_",
      cv,"_",trainMet[m],"_",cond_name,".Rdata"))
  
  # delete for storage
  rm(model, metrics)
}
print("Multiclass done")
}
