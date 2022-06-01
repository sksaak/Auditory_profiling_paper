merge_lists <- function(cluster){
  # rbind all dataframes in lists to one dataframe 
  
  dat <- data.frame()
  for (c in 1:length(cluster)){
    
    profile = rep(c, times = nrow(cluster[[c]]))
    dat <- rbind(dat, cbind(cluster[[c]], profile))
  }
  return(dat)
}
get_metric <- function(conf){
  
  ap = rownames(conf)

  TP = conf[2,2] 
  TN = conf[1,1]
  FP = conf[2,1]
  FN = conf[1,2]
  
  sens = TP/(TP + FN) 
  spec = TN/(TN+FP)
  prec = TP/(TP+FP)
  f1 = 2*TP/(2*TP + FP + FN)
  
  train_metric = data.frame(ap = ap,
                            sens = sens, 
                            spec = spec, 
                            prec = prec, 
                            f1 = f1)

  return(train_metric)
}

# evaluation metrics
f1 <- function(data, lev = NULL, model = NULL) {
  # using the F1 score as tuning metric 
  conf = caret::confusionMatrix(data=as.factor(data$pred),
                                reference = as.factor(data$obs)) 
  
  if (is.null(nrow(conf[["byClass"]]))){
    f1 = conf[["byClass"]][7]
  }else { f1 = conf[["byClass"]][,7]}
  
  c(F1 = mean(f1))
  
}
balanced_accuracy <- function(data, lev = NULL, model = NULL){
  
  conf = caret::confusionMatrix(data=as.factor(data$pred),
                                reference = as.factor(data$obs)) #,
                                #positive = lev[1])
  if (is.null(nrow(conf[["byClass"]]))){
    b_acc = conf[["byClass"]][11]
  }else { b_acc = conf[["byClass"]][,11]}
  
  c(balanced_accuracy = mean(b_acc))
}
auprcSummary <- function(data, lev = NULL, model = NULL){
  
  var = names(table(data$obs))
  index_class2 <- data$obs == var[2]
  index_class1 <- data$obs == var[1]
  
  the_curve <- PRROC::pr.curve(data$pred[index_class2], data$pred[index_class1], curve = FALSE)
  out <- the_curve$auc.integral
 
  c(AUPRC = out)
  
}
AUPRC_multi <- function(data, lev = NULL, model = NULL){
  #Check data
  if (!all(levels(data[, "pred"]) == levels(data[, "obs"])))
    stop("levels of observed and predicted data do not match")
  
  has_class_probs <- all(lev %in% colnames(data))
  
  if(has_class_probs) {
    ## Overall multinomial loss
    lloss <- mnLogLoss(data = data, lev = lev, model = model)
    
    require("ModelMetrics")
    require("MLmetrics")
    
    #Calculate custom one-vs-all ROC curves for each class
    prob_stats <-
      lapply(levels(data[, "pred"]),
             function(x){
               #Grab one-vs-all data for the class
               obs  <- ifelse(data[,"obs"] == x, 1, 0)
               prob <- data[,x]
               roc_auc <- try(ModelMetrics::auc(obs, data[,x]), silent = TRUE)
               # browser()
               pr_auc <- try(MLmetrics::PRAUC(y_pred = data[,x], 
                                              y_true = obs),
                             silent = TRUE)
               if(inherits(pr_auc, "try-error"))
                 pr_auc <- NA
               res <- c(ROC = roc_auc, AUC = pr_auc)
               return(res)
             })
    prob_stats <- do.call("rbind", prob_stats)
    prob_stats <- colMeans(prob_stats, na.rm = TRUE)
  }
  
  #Calculate confusion matrix-based statistics
  CM <- caret::confusionMatrix(data[, "pred"], data[, "obs"])
  
  #Aggregate and average class-wise stats
  #Todo: add weights
  # RES: support two classes here as well
  if (length(levels(data[, "pred"])) == 2) {
    class_stats <- CM$byClass
  } else {
    class_stats <- colMeans(CM$byClass)
    names(class_stats) <- paste("Mean", names(class_stats))
  }
  
  # Aggregate overall stats
  overall_stats <-
    if (has_class_probs)
      c(CM$overall,
        logLoss = as.numeric(lloss),
        AUC = unname(prob_stats["ROC"]),
        prAUC = unname(prob_stats["AUC"]))
  else
    CM$overall
  
  
  # Combine overall with class-wise stats and remove some stats we don't want
  stats <- c(overall_stats, class_stats)
  stats <- stats[! names(stats) %in% c('AccuracyNull', "AccuracyLower", "AccuracyUpper",
                                       "AccuracyPValue", "McnemarPValue",
                                       'Mean Prevalence', 'Mean Detection Prevalence')]
  
  # Clean names
  names(stats) <- gsub('[[:blank:]]+', '_', names(stats))
  
  # Change name ordering to place most useful first
  # May want to remove some of these eventually
  stat_list <- c("Accuracy", "Kappa", "Mean_F1", 
                 "Mean_Sensitivity", "Mean_Specificity",
                 "Mean_Pos_Pred_Value", "Mean_Neg_Pred_Value", 
                 "Mean_Precision", "Mean_Recall",
                 "Mean_Detection_Rate",
                 "Mean_Balanced_Accuracy")
  if(has_class_probs) stat_list <- c("logLoss", "AUC", "prAUC", stat_list)
  if (length(levels(data[, "pred"])) == 2) stat_list <- gsub("^Mean_", "", stat_list)
  
  stats <- stats[c(stat_list)]
  
  AUPRC = stats["prAUC"] 
  
  return(AUPRC)
}

# logical matrix to extract all misslabels
logmat <- function(n) {diag(n)==0}

# UPSAMPLING
balance <- function(df, method, levels = c(1,0), min_n = 60){
  # balances the dataset of minority/majority cases with respect to min_n, i.e., 
  # the minority data is upsampled to min_n
  
  
  minority = df[df$profile == levels[1],]
  majority = df[df$profile == levels[2],]
  
  if (method == "upsample"){
    idx = sample(1:nrow(minority), size = nrow(majority), replace = TRUE)
    
    new_cases = minority[idx, ]
    
    balanced = rbind(majority, new_cases)
    
  } else if(method == "upsample_with_noise"){
    
    # new_n = nrow(majority)
    
    new_data <- upsample_with_gaussian_noise(df, df$profile, min_n = min_n)
    
    balanced = new_data
    
  } else{print("error")}
  
  return(balanced)
}
upsample_with_gaussian_noise <- function(df, class, min_n){
  # upsampling of classes
  
  class_to_upsample <- names(which(table(class) < min_n))
  
  new_data = NULL
  
  
  for (c in class_to_upsample){
    
    selected_data <- df[which(class == c),]
    n_more <- min_n-nrow(selected_data)
    
    tmp_sample <- selected_data[sample(nrow(selected_data), n_more, replace = TRUE), ]
    tmp_new_data = tmp_sample
    
    
    for (cols in 1:ncol(tmp_sample)){
      
      if(class(tmp_sample[,cols]) == "numeric"){
        col_sd <- sd(selected_data[,cols])
        sd_range <- seq(from = -col_sd, to = col_sd, by = (2*col_sd)/100 )
        noise <- sample(x = sd_range, size = nrow(tmp_sample), replace = TRUE)
        tmp_new_data[,cols] <- tmp_sample[,cols] + noise
        
      } 
    }
    
    new_data <- rbind(new_data, tmp_new_data)
    
  }
  
  
  return(new_data)
  
}

# tuneGrids
mtryGrid = function(train_data){
rfGrid =  expand.grid(mtry = c(1:(ncol(train_data)-1)))
return(rfGrid)
}

