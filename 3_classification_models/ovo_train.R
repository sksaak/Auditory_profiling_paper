ovo_train = function(){

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

# make model grid ---------------------------------------------------------
a = c(rep(1, times = 12),
      rep(2, times = 11),
      rep(3, times = 10),
      rep(4, times = 9),
      rep(5, times = 8),
      rep(6, times = 7),
      rep(7, times = 6),
      rep(8, times = 5),
      rep(9, times = 4),
      rep(10, times = 3),
      rep(11, times = 2),
      rep(12, times = 1)
)
b = c(2:13,3:13,4:13,5:13,6:13,7:13,8:13,9:13,10:13,11:13,12:13,13)

apGrid = data.frame(a = a, b = b)      

rfGrid = mtryGrid(train_data)

# ONE-vs. ONE classification --------------------------------------------------

# LOOP ACROSS comparison models -------------------------------------------------------
for (r in 1:nrow(apGrid)){
  
  ap1 = apGrid[r,1]
  ap2 = apGrid[r,2]
  
  dat = rbind(train_data[train_data$profile == ap1,], train_data[train_data$profile == ap2,])
  
  dat$profile <- as.numeric(dat$profile)  
  dat$profile[dat$profile == ap1] = paste0("class_",ap1)
  dat$profile[dat$profile == ap2] = paste0("class_",ap2)
  
  
  new_data = balance(dat, method = "upsample_with_noise", levels = c("pos","neg"), min_n = 60)

  ap_dat = rbind(dat, new_data)

  # shuffle rows
  ap_dat= ap_dat[sample(1:nrow(ap_dat)), ]

  # LOOP across different model parameterization
  for (m in c(1,2,3,4,5,6,7,8)){
    
    if (m %in% c(1,2,3,4)){cv = "repeatedCV"
    }else if (m %in% c(5,6,7,8)){cv = "LOOCV"}
    
    model<- train(profile ~ ., data = dat, 
                  method = "rf", 
                  trControl = fitControls[[m]],
                  ntree = 500,
                  tuneGrid = rfGrid,
                  metric = trainMet[m], 
                  verbose = FALSE
    )
    
    
    # train prediction ---------------------------------------------- 
    conf <- model$finalModel$confusion[1:2,1:2]
    miss <- conf[i_mat]
    train_metric = get_metric(conf)
    
    
    # test prediction ----------------------------------------------
    dat = rbind(test_data[test_data$profile == ap1,], test_data[test_data$profile == ap2,])
    
    dat$profile <- as.numeric(dat$profile)  
    dat$profile[dat$profile == ap1] = paste0("class_",ap1)
    dat$profile[dat$profile == ap2] = paste0("class_",ap2)
    dat$profile = as.factor(dat$profile)
    
    pred <- predict(model, newdata=dat)
    
    tmp <- caret::confusionMatrix(pred, reference = dat$profile)
    conf = tmp$table
    tmp <- tmp[["byClass"]]
    
    test_metric = data.frame(ap = paste(ap1,ap2),
                             sens = tmp[1],
                             spec = tmp[2],
                             prec = tmp[5],
                             f1   = tmp[7])
    
    
    metrics <- rbind(train_metric, test_metric)
    metrics$cond = c("train","train","test")
    print(paste("AP:", ap1, "vs.",ap2, "    CV:", cv, "    Metric:", trainMet[m] ))
    print(conf)
    
    # set up folders and directories
    dir.create(file.path("data/classification/ovo/"), recursive = TRUE)  
    
    save(model, file = paste0("data/classification/ovo/Model_",
                              cv,"_", trainMet[m],"_",ap1, "_", ap2,"_",cond_name ,".Rdata"))
    save(metrics, file = paste0("data/classification/ovo/Metric_",
                                cv,"_", trainMet[m],"_",ap1, "_", ap2,"_",cond_name ,".Rdata"))
    # delete for storage
    rm(model, metrics)
  }
}
}
