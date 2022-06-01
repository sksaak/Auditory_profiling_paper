#
#
# Evaluate models
#
#
###############################################################


# clear console, objects and plots 
cat("\014")  
rm(list = ls())
dev.off()

# libraries
if (!require(ggplot2)) install.packages('ggplot2')
if (!require(gridExtra)) install.packages('gridExtra')
if(!require(caret)) install.packages('caret')
if(!require(RColorBrewer)) install.packages('RColorBrewer')
library(ggplot2)
library(gridExtra)
library(caret)
library(RColorBrewer)

set.seed(22)
#pred functions
get_train_ova = function(cv){
  
  mets = c()
  trainMet = c("AUPRC", "balanced_accuracy", "Kappa", "F1")
  for (m in trainMet){
  # get train metric
  met = c()
  for (ap in 1:13){
    load(paste0("data/classification/ova/Metrics_",cv, "_", m, "_", ap,"_vhc.Rdata"))
    
    metric = metrics[1,]
    metric$metric = m
    met = rbind(met, metric)
   }
  
  mets = rbind(mets, met)
  
  }
  
  return(mets)
}
predict_ova = function(data, m, mode = "ova", cv){

  preds = c()
  out_probs = list()
  for (ap in 1:13){
    load(paste0("data/classification/ova/Model_", cv, "_", m, "_", ap,"_vhc.Rdata"))
    
    out = predict(model, data)
    out_prob = predict(model, data, "prob")
    out = as.character(out)
    out[out == "neg"] = 0
    out[out == "pos"] = ap
    
    preds = cbind(preds, as.numeric(out))
    out_probs[[ap]] = out_prob
    
  }
  colnames(preds) = c(1:13) 
  
  # loop over each row, and store indices with more than one prediction-------
  res_mat = matrix(data = NA, nrow = nrow(preds), ncol = 1)
  idx = c()
  aps = list()
  for (r in 1:nrow(preds)){
    out = table(preds[r,])
    if(length(out) == 2){
      res_mat[r] = getmin(out)
    }else if (length(out) > 2){
      idx = c(idx,r)
      outn = names(out)
      aps[[r]] = as.numeric(outn[names(out) != "0"])
    }
  }
  
  out = list(pred = res_mat, 
             idx = idx, 
             aps = aps)
  
  
  if (mode == "ovaovo"){
      return(out)
  } else if (mode == "ova"){
    
    # get prediction with highest probability-------
    if (is.null(out$idx)){
    } else if ( !is.null(out$idx)){
      new_pred = c()
      for (r in 1:length(out$idx)){
        
      potentialAPS = aps[[out$idx[r]]]
      
      probs = c()
      for (p in potentialAPS){
      prob = out_probs[[p]][idx[r],2]  
      probs = c(prob, probs)
      }
      
      new_ap = potentialAPS[which(probs == max(probs))]
      if (length(new_ap > 1 )){new_ap =  sample(new_ap, size = 1)}
      
      new_pred = c(new_pred, new_ap )
      
        # if (max(cut_prob_a) > max(cut_prob_b)){
        #   new_pred = c(new_pred,as.numeric(out$aps[r,1]))
        # } else {new_pred = c(new_pred,out$aps[r,2]) }
      }
      out[["new_pred"]] = new_pred
      
      res_mat[out$idx] = out$new_pred
      
    }
    
    # make highest predictions for 0 cases (i.e, no model detected it as its class)----
    if (any(is.na(res_mat))){
      NA_idx = which(is.na(res_mat))
      
      new_nas = c()
      for (ni in NA_idx){
        tmp = c()
        for (ap in 1:length(out_probs)){
          tmp =  c(tmp,out_probs[[ap]][ni,2])
        }
        set.seed(22)
        
        new_na = which(tmp == max(tmp))
        if (length(new_na) > 1){new_na =  sample(new_na, size = 1)}
        
        new_nas = c(new_nas, new_na  )
      }  
      
      res_mat[NA_idx] = new_na
      
    }
    
    res_mat = factor(res_mat, levels = c(1:13))
    
    AUPRC = caret::confusionMatrix(res_mat, data$profile)
    
    
    res = list(metric =AUPRC$byClass[,c(1,2,5,7)],
               pred = res_mat)
    
    return(res)
  }
}
get_train_multiclass = function(cv){
  
  mets = c()
  trainMet = c("AUPRC", "balanced_accuracy", "Kappa", "F1")
  
  for (m in trainMet){
    # get train metric
    load(paste0("data/classification/multiclass/Metric_", cv, "_", m,"_vhc.Rdata"))
    
    metrics$metric = m
    mets = rbind(mets, metrics)
  }
  train = mets[mets$cond == "train",]
  
  return(train)
}
predict_multi = function(data,m, cv){
  load(paste0("data/classification/multiclass/Model_", cv, "_", m,"_vhc.Rdata"))
  
  out =predict(model, newdata = data[,2:13])
  out = gsub("class_class_class_", "", out)
  out = factor(out, levels = c(1:13))
  
  tmp = caret::confusionMatrix(out,data$profile)
  
  met = tmp$byClass[,c(1,2,5,7)]
  
  res = list(metric = met, pred =  out)
  
  return(res)
}
get_train_ovo = function(cv){
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
  
  trainMet = c("AUPRC", "balanced_accuracy", "Kappa", "F1")

  all_mets = c()
  for (m in trainMet){
    mets = c()
    for (r in 1:78){
      load(paste0("data/classification/ovo/Metric_", cv, "_",m, "_",apGrid[r,1],"_",apGrid[r,2],"_vhc.Rdata"))
      
      metrics$metric = m
      mets <- rbind(mets, metrics[1,])
    }
    all_mets = rbind(mets,all_mets)
  }
  return(all_mets)
}
predict_ovo = function(data,m, cv, type = "ovo"){
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
  
  
  
    metric_pred = list()
    all_pred = c()
    for (r in 1:78){
      load(paste0("data/classification/ovo/Model_", cv, "_",m, "_",apGrid[r,1],"_",apGrid[r,2],"_vhc.Rdata"))
      
      pred =  predict(model, newdata = data[,2:13])   
      pred = as.character(pred)
      pred = as.numeric(gsub("class_", "", x = pred))
      
      all_pred = cbind(all_pred,pred)
      
    }
    metric_pred[[m]] = all_pred
    
    metric = c()
    result = c()
    for (r in 1:nrow(data)){
      result = c(result,getmode(as.numeric(metric_pred[[m]][r,])))
    }
    if (type == "ovaovo"){
      return(result)
    }
    
    tmp <- caret::confusionMatrix(as.factor(result), reference = data$profile)
    conf = tmp$table
    
    metrics <- as.data.frame(tmp[["byClass"]])
    
    met = metrics[,c(1,2,5,7)]
    
    met$metric = m 
    ap = c(1:13)
    met = cbind(ap, met)
    
    metric = rbind(metric,met)
    
    names(metric) = c("ap","sens","spec","prec","f1","metric")
    
    return(metric)
}
get_train_ovaovo = function(train_data, cv){
  
  mets = c("Kappa" ,"AUPRC", "balanced_accuracy", "F1")
  
  df = data.frame()
  for (m in mets){
    
    ovaovo_best = predict_ova(train_data, m, "ovaovo", cv)
    ovaovo_best = predict_ovaOvo(train_data, ovaovo_best, m, cv)
    
    ovaovo_best = predict_ova(train_data, m, "ovaovo", cv)
    ovaovo_best = predict_ovaOvo(train_data, ovaovo_best, m, cv)
    ovaovo_best = as.data.frame(ovaovo_best$conf$byClass[,c(1,2,5,7)])
    colnames(ovaovo_best) = c("sens","spec","prec","f1")
    ovaovo_best = cbind(ap = c(1:13),ovaovo_best, cond = "train", metric = m)
    ovaovo_best$model = "ovaovo"
    
    df = rbind(df,ovaovo_best )
  }
  return(df)
}
predict_ovaOvo = function(data, res,m, cv){
  #
  # model = the generated model used to predict cases,
  # data = the data (train or test)
  # res = list of predictions from ova with indices for unclear cases (from predict_ova)
  #  
  # make predictions for unclear cases 
  
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
  
  
  # get ovo models
  models = list()
  for (r in 1:78){
    load(paste0("data/classification/ovo/Model_", cv, "_",m, "_",apGrid[r,1],"_",apGrid[r,2],"_vhc.Rdata"))
    models[[r]] = model
  }
  
  # check if uncertainty is present
  if (is.null(res$idx)){
    print("No uncertainty")
    results = res$pred
  } else {
    # predict cases with ovo 
    all_ovo_res = c()
    for (i in res$idx){
      
      comp = res$aps[[i]]
      comp= sort(comp) # sort as apGrid goes from low to high
      ovo_idx =  which(apGrid$a == comp[1] & apGrid$b == comp[2])
      
      patient = data[i,]
      
      ovo_res = predict(models[[ovo_idx]], newdata=patient[,2:13] )
      
      ovo_res = unlist(ovo_res)
      
      ovo_res = as.numeric(gsub("class_", "", ovo_res))
      
      ovo_res = getmode(ovo_res)
      
      all_ovo_res[i] = ovo_res
      
    }
    
    results = res$pred
    results[res$idx] = all_ovo_res[res$idx]
    
    
    # make ovo predictions for 0 cases (i.e, no model detected it as its class)
    if (any(is.na(results))){
      NA_idx = which(is.na(results))
      NA_patients = data[NA_idx,]
      ovo_res = predict_ovo(NA_patients, m, cv, type = "ovaovo" )
      
      results[NA_idx] = ovo_res
    }
  }
  results = factor(results, levels = c(1:13))
  conf = confusionMatrix(results, data$profile)
  
  res = list(predictions = results,
             conf = conf)
  return(res)
  
}  

#plot functions
train_plot_metrics_line = function(metric, ymin = 0.2){
  cols = c("orange", "darkgreen", "darkred", "darkblue")
  train = metric
  p = c(1:3,5:13)
  sens_plot = ggplot(data = train, aes(x = as.factor(ap), group = metric, color = metric))+
    geom_line(aes(y = sens), size= 1.1)+
    geom_hline(yintercept = mean(train$sens[train$metric == "AUPRC"][p]), color = cols[1], linetype = "dashed")+
    geom_hline(yintercept = mean(train$sens[train$metric == "balanced_accuracy"][p]), color = cols[2], linetype = "dashed")+
    geom_hline(yintercept = mean(train$sens[train$metric == "F1"][p]), color = cols[3], linetype = "dashed")+
    geom_hline(yintercept = mean(train$sens[train$metric == "Kappa"][p]), color = cols[4], linetype = "dashed")+
   # scale_x_continuous(name = "AP", breaks = c(1:13))+
    scale_color_manual(values = cols)+
    scale_fill_manual(values = cols)+
    theme_bw()+
    theme(legend.position = "bottom")+
    ylab("Sensitivity")+
    ylim(ymin,1)
  
  spec_plot =  ggplot(data = train, aes(x = ap, group = metric, color = metric))+
    geom_line(aes(y = spec), size= 1.1)+
    geom_hline(yintercept = mean(train$spec[train$metric == "AUPRC"]), color = cols[1], linetype = "dashed")+
    geom_hline(yintercept = mean(train$spec[train$metric == "balanced_accuracy"]), color = cols[2], linetype = "dashed")+
    geom_hline(yintercept = mean(train$spec[train$metric == "F1"]), color = cols[3], linetype = "dashed")+
    geom_hline(yintercept = mean(train$spec[train$metric == "Kappa"]), color = cols[4], linetype = "dashed")+
    scale_x_continuous(name = "AP", breaks = c(1:13))+
    scale_color_manual(values = cols)+
    theme_bw()+
    theme(legend.position = "bottom")+
    ylab("Specificity")+
    ylim(ymin,1)
  
  prec_plot =ggplot(data = train, aes(x = ap, group = metric, color = metric))+
    geom_line(aes(y = prec), size= 1.1)+
    geom_hline(yintercept = mean(train$prec[train$metric == "AUPRC"]), color = cols[1], linetype = "dashed")+
    geom_hline(yintercept = mean(train$prec[train$metric == "balanced_accuracy"]), color = cols[2], linetype = "dashed")+
    geom_hline(yintercept = mean(train$prec[train$metric == "F1"]), color = cols[3], linetype = "dashed")+
    geom_hline(yintercept = mean(train$prec[train$metric == "Kappa"]), color = cols[4], linetype = "dashed")+
    scale_x_continuous(name = "AP", breaks = c(1:13))+
    scale_color_manual(values = cols)+
    theme_bw()+
    theme(legend.position = "bottom")+
    ylab("Precision")+
    ylim(ymin,1)
  
  f1_plot = ggplot(data = train, aes(x = ap, group = metric, color = metric))+
    geom_line(aes(y = f1), size= 1.1)+
    geom_hline(yintercept = mean(train$f1[train$metric == "AUPRC"]), color = cols[1], linetype = "dashed")+
    geom_hline(yintercept = mean(train$f1[train$metric == "balanced_accuracy"]), color = cols[2], linetype = "dashed")+
    geom_hline(yintercept = mean(train$f1[train$metric == "F1"]), color = cols[3], linetype = "dashed")+
    geom_hline(yintercept = mean(train$f1[train$metric == "Kappa"]), color = cols[4], linetype = "dashed")+
    scale_x_continuous(name = "AP", breaks = c(1:13))+
    scale_color_manual(values = cols)+
    theme_bw()+
    theme(legend.position = "bottom")+
    ylab("F1")+
    ylim(ymin,1)
  
  
  metric_plot = grid.arrange(sens_plot, spec_plot, 
                             prec_plot, f1_plot, 
                             nrow = 2, ncol = 2)
  
  return(metric_plot)
  
}
train_plot_metrics_point = function(metric, ymin = 0.2){
  cols = c("orange", "darkgreen", "darkred", "darkblue")
  train = metric
  p = c(1:3,5:13)
  sens_plot = ggplot(data = train, aes(x = as.factor(ap), group = metric, color = metric))+
    geom_point(aes(y = sens, shape = metric), size = 3, alpha = 0.8)+
    geom_hline(yintercept = mean(train$sens[train$metric == "AUPRC"][p]), color = cols[1], linetype = "dashed")+
    geom_hline(yintercept = mean(train$sens[train$metric == "balanced_accuracy"][p]), color = cols[2], linetype = "dashed")+
    geom_hline(yintercept = mean(train$sens[train$metric == "F1"][p]), color = cols[3], linetype = "dashed")+
    geom_hline(yintercept = mean(train$sens[train$metric == "Kappa"][p]), color = cols[4], linetype = "dashed")+
    # scale_x_continuous(name = "AP", breaks = c(1:13))+
    scale_color_manual(values = cols)+
    scale_fill_manual(values = cols)+
    theme_bw()+
    theme(legend.position = "bottom")+
    ylab("Sensitivity")+
    xlab("Auditory Profile")+
    ylim(ymin,1)
  
  spec_plot =  ggplot(data = train, aes(x = ap, group = metric, color = metric))+
    geom_point(aes(y = spec, shape = metric), size = 3, alpha = 0.8)+
    geom_hline(yintercept = mean(train$spec[train$metric == "AUPRC"]), color = cols[1], linetype = "dashed")+
    geom_hline(yintercept = mean(train$spec[train$metric == "balanced_accuracy"]), color = cols[2], linetype = "dashed")+
    geom_hline(yintercept = mean(train$spec[train$metric == "F1"]), color = cols[3], linetype = "dashed")+
    geom_hline(yintercept = mean(train$spec[train$metric == "Kappa"]), color = cols[4], linetype = "dashed")+
    scale_x_continuous(name = "AP", breaks = c(1:13))+
    scale_color_manual(values = cols)+
    theme_bw()+
    theme(legend.position = "bottom")+
    ylab("Specificity")+
    xlab("Auditory Profile")+
    ylim(ymin,1)
  
  prec_plot =ggplot(data = train, aes(x = ap, group = metric, color = metric))+
    geom_line(aes(y = prec), size= 1.1)+
    geom_hline(yintercept = mean(train$prec[train$metric == "AUPRC"]), color = cols[1], linetype = "dashed")+
    geom_hline(yintercept = mean(train$prec[train$metric == "balanced_accuracy"]), color = cols[2], linetype = "dashed")+
    geom_hline(yintercept = mean(train$prec[train$metric == "F1"]), color = cols[3], linetype = "dashed")+
    geom_hline(yintercept = mean(train$prec[train$metric == "Kappa"]), color = cols[4], linetype = "dashed")+
    scale_x_continuous(name = "AP", breaks = c(1:13))+
    scale_color_manual(values = cols)+
    theme_bw()+
    theme(legend.position = "bottom")+
    ylab("Precision")+
    ylim(ymin,1)
  
  f1_plot = ggplot(data = train, aes(x = ap, group = metric, color = metric))+
    geom_line(aes(y = f1), size= 1.1)+
    geom_hline(yintercept = mean(train$f1[train$metric == "AUPRC"]), color = cols[1], linetype = "dashed")+
    geom_hline(yintercept = mean(train$f1[train$metric == "balanced_accuracy"]), color = cols[2], linetype = "dashed")+
    geom_hline(yintercept = mean(train$f1[train$metric == "F1"]), color = cols[3], linetype = "dashed")+
    geom_hline(yintercept = mean(train$f1[train$metric == "Kappa"]), color = cols[4], linetype = "dashed")+
    scale_x_continuous(name = "AP", breaks = c(1:13))+
    scale_color_manual(values = cols)+
    theme_bw()+
    theme(legend.position = "bottom")+
    ylab("F1")+
    ylim(ymin,1)
  
  
  metric_plot = grid.arrange(sens_plot, spec_plot, 
                             prec_plot, f1_plot, 
                             nrow = 2, ncol = 2)
  
  return(metric_plot)
  
}
plot_train_test = function(best_train,test){
  
  # make format same
  test = cbind(c(1:13), test)
  test = as.data.frame(test)
  test$cond = "test"
  names(test) = c("ap", "sens","spec","prec","f1","cond")
  best_train$metric = NULL
  
  df = rbind(best_train, test)
  
  cols = c("darkred", "darkblue")
  p = c(1:3,5:13)
  sens_plot = ggplot(data = df, aes(x = ap, group = cond, color = cond))+
    geom_point(aes(y = sens, shape = cond), size= 3) +
    geom_hline(yintercept = mean(df$sens[df$cond == "test"][p]), color = cols[1], linetype = "dashed")+
    geom_hline(yintercept = mean(df$sens[df$cond == "train"][p]), color = cols[2], linetype = "dashed")+
    scale_x_continuous(name = "Auditory Profile", breaks = c(1:13))+
    scale_color_manual(values = cols)+
    theme_bw()+
    theme(legend.position = "bottom")+
    ylab("Sensitivity")+
    ylim(0,1)
  
  spec_plot = ggplot(data = df, aes(x = ap, group = cond, color = cond))+
    geom_point(aes(y = spec, shape = cond), size= 3) +
    geom_hline(yintercept = mean(df$spec[df$cond == "test"][p]), color = cols[1], linetype = "dashed")+
    geom_hline(yintercept = mean(df$spec[df$cond == "train"][p]), color = cols[2], linetype = "dashed")+
    scale_x_continuous(name = "Auditory Profile", breaks = c(1:13))+
    scale_color_manual(values = cols)+
    theme_bw()+
    theme(legend.position = "bottom")+
    ylab("Specificity")+
    ylim(0,1)
  
  prec_plot = ggplot(data = df, aes(x = ap, group = cond, color = cond))+
    geom_point(aes(y = prec, shape = cond), size= 3) +
    geom_hline(yintercept = mean(df$prec[df$cond == "test"][p]), color = cols[1], linetype = "dashed")+
    geom_hline(yintercept = mean(df$prec[df$cond == "train"][p]), color = cols[2], linetype = "dashed")+
    scale_x_continuous(name = "Auditory Profile", breaks = c(1:13))+
    scale_color_manual(values = cols)+
    theme_bw()+
    theme(legend.position = "bottom")+
    ylab("Precision")+
    ylim(0,1)
  
  f1_plot = ggplot(data = df, aes(x = ap, group = cond, color = cond))+
    geom_point(aes(y = f1, shape = cond), size= 3) +
    geom_hline(yintercept = mean(df$f1[df$cond == "test"][p]), color = cols[1], linetype = "dashed")+
    geom_hline(yintercept = mean(df$f1[df$cond == "train"][p]), color = cols[2], linetype = "dashed")+
    scale_x_continuous(name = "Auditory Profile", breaks = c(1:13))+
    scale_color_manual(values = cols)+
    theme_bw()+
    theme(legend.position = "bottom")+
    ylab("F1 Score")+
    ylim(0,1)
  
  plotlist = list(sens_plot, spec_plot, prec_plot, f1_plot)
  
  return(plotlist)
}
plot_test_ovo = function(metric, m, ymin = 0.2){
  cols = c("darkred")
  train = metric
  p = c(1:3,5:13)
  sens_plot = ggplot(data = train, aes(x = as.factor(ap), group = metric, color = metric))+
    geom_point(aes(y = sens), size= 3) +
    #geom_line(aes(y = sens), size= 1.1)+
    geom_hline(yintercept = mean(train$sens[train$metric == m][p]), color = cols[1], linetype = "dashed")+
    scale_color_manual(values = cols)+
    scale_fill_manual(values = cols)+
    theme_bw()+
    theme(legend.position = "bottom")+
    ylab("Sensitivity")+
    ylim(ymin,1)
  
  spec_plot =  ggplot(data = train, aes(x = ap, group = metric, color = metric))+
    geom_point(aes(y = spec), size= 3) +
    #geom_line(aes(y = spec), size= 1.1)+
    geom_hline(yintercept = mean(train$sens[train$metric == m][p]), color = cols[1], linetype = "dashed")+
    scale_x_continuous(name = "AP", breaks = c(1:13))+
    scale_color_manual(values = cols)+
    theme_bw()+
    theme(legend.position = "bottom")+
    ylab("Specificity")+
    ylim(ymin,1)
  
  prec_plot =ggplot(data = train, aes(x = ap, group = metric, color = metric))+
    geom_point(aes(y = prec), size= 3) +
    #geom_line(aes(y = prec), size= 1.1)+
    geom_hline(yintercept = mean(train$sens[train$metric == m][p]), color = cols[1], linetype = "dashed")+
    scale_x_continuous(name = "AP", breaks = c(1:13))+
    scale_color_manual(values = cols)+
    theme_bw()+
    theme(legend.position = "bottom")+
    ylab("Precision")+
    ylim(ymin,1)
  
  f1_plot = ggplot(data = train, aes(x = ap, group = metric, color = metric))+
    geom_point(aes(y = f1), size= 3) +
    #geom_line(aes(y = f1), size= 1.1)+
    geom_hline(yintercept = mean(train$sens[train$metric == m][p]), color = cols[1], linetype = "dashed")+
    scale_x_continuous(name = "AP", breaks = c(1:13))+
    scale_color_manual(values = cols)+
    theme_bw()+
    theme(legend.position = "bottom")+
    ylab("F1")+
    ylim(ymin,1)
  
  
  plotlist = list(sens_plot, spec_plot, prec_plot, f1_plot)
  
  return(plotlist)
  
}
plot_comparison <- function(all_best, cv, cond){
  
  p = c(1:3,5:13)
  cols = c("orange", "darkgreen", "darkred", "darkblue")
  
  sens = ggplot(data = all_best, aes(x = ap, y = sens, group = model, color = model, shape = model))+
    geom_point(size = 2)+
    geom_hline(yintercept = mean(all_best$sens[all_best$model == "ova"][p]), color = cols[1], linetype = "dashed")+
    geom_hline(yintercept = mean(all_best$sens[all_best$model == "multi"][p]), color = cols[2], linetype = "dashed")+
    geom_hline(yintercept = mean(all_best$sens[all_best$model == "ovo"][p]), color = cols[3], linetype = "dashed")+
    geom_hline(yintercept = mean(all_best$sens[all_best$model == "ovaovo"][p]), color = cols[4], linetype = "dashed")+
    scale_x_continuous(name = "AP", breaks = c(1:13))+
    scale_color_manual(values = cols)+
    theme_bw()+
    ylab("Sensitivity")+
    ggtitle(cv[c])+
    ylim(0,1)
  
  prec = ggplot(data = all_best, aes(x = ap, y = prec, group = model, color = model, shape = model))+
    geom_point(size = 2)+
    geom_hline(yintercept = mean(all_best$prec[all_best$model == "ova"][p]), color = cols[1], linetype = "dashed")+
    geom_hline(yintercept = mean(all_best$prec[all_best$model == "multi"][p]), color = cols[2], linetype = "dashed")+
    geom_hline(yintercept = mean(all_best$prec[all_best$model == "ovo"][p]), color = cols[3], linetype = "dashed")+
    geom_hline(yintercept = mean(all_best$prec[all_best$model == "ovaovo"][p]), color = cols[4], linetype = "dashed")+
    scale_x_continuous(name = "AP", breaks = c(1:13))+
    scale_color_manual(values = cols)+
    theme_bw()+
    ylab("Precision")+
    ggtitle(cv[c])+
    ylim(0,1)
  
  p = grid.arrange(sens, prec, ncol = 1, nrow = 2)
  
  return(p)
}


#general functions
get_best_metric= function(metrics){
  # provide all metrics in dataframe containing [sens, spec, prec, f1, metric]
  mets = c()
  for (m in unique(metrics$metric)){
    
    cnames = c("sens", "spec", "prec","f1")
    ridx = metrics$metric == m
    mets = rbind(mets, colMeans(metrics[ridx,cnames]))
    
  }
  mets = as.data.frame(mets)
  mets$metric = unique(metrics$metric)
  
  
  best = mets$metric[which(mets[,1] == max(mets[,1]))]
  if ("f1" %in% best ){best[best == "f1"] = "F1"}
  best = metrics[metrics$metric %in% best,]
  
  return(best)
}
getmin = function(x){
  
  out = names(x[which.min(x)])
  out = as.numeric(out)
  out
  
}
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

load("data/train_params.Rdata")

cv = c("repeatedCV", "LOOCV")

results = list()

for (c in 1:2){

# get training ova---------------------
ova_mets = get_train_ova(cv = cv[c])
train_plot_metrics_line(ova_mets)
ova_best = get_best_metric(ova_mets)
print(ova_best$metric[1])

# get test ova----------------------------------
ova_test = predict_ova(test_data, ova_best$metric[1], mode = "ova", cv = cv[c])
ova_plots = plot_train_test(ova_best, ova_test$metric)
grid.arrange(grobs = ova_plots)

results[[cv[c]]][["ova"]]$ova_mets = ova_mets
results[[cv[c]]][["ova"]]$ova_best = ova_best
results[[cv[c]]][["ova"]]$ova_test = ova_test

# get training multiclass-----------------------------
multi_mets = get_train_multiclass(cv[c])
train_plot_metrics_line(multi_mets)

multi_best = get_best_metric(multi_mets)
multi_best = multi_best[multi_best$metric == multi_best$metric[1],]

# get test multiclass----------------------------------------
multi_test = predict_multi(test_data, multi_best$metric[1], cv[c])
multi_plots = plot_train_test(multi_best, multi_test$metric)
grid.arrange(grobs = multi_plots)

results[[cv[c]]][["multi"]]$multi_mets = multi_mets
results[[cv[c]]][["multi"]]$multi_best = multi_best
results[[cv[c]]][["multi"]]$multi_test = multi_test

# get training ovo ------------------------------------------
ovo_mets = get_train_ovo(cv[c])
ovo_mets = rbind(predict_ovo(train_data, "Kappa",cv[c]),
                 predict_ovo(train_data, "AUPRC",cv[c]),
                 predict_ovo(train_data, "balanced_accuracy",cv[c]),
                 predict_ovo(train_data, "F1",cv[c]))

ovo_best = get_best_metric(ovo_mets)
train_plot_metrics_line(ovo_mets)

# get test ovo ------------------------------------------------
ovo_test = predict_ovo(test_data, ovo_best$metric[1], cv[c])
ovo_plots = plot_test_ovo(ovo_test, ovo_best$metric[1], ymin = 0)
grid.arrange(grobs = ovo_plots)

results[[cv[c]]][["ovo"]]$ovo_mets = ovo_mets
results[[cv[c]]][["ovo"]]$ovo_best = ovo_best
results[[cv[c]]][["ovo"]]$ovo_test = ovo_test

# get train ovo + ova -----------------------------------------------
ovaovo_mets = get_train_ovaovo(train_data, cv[c])
train_plot_metrics_line(ovaovo_mets)
ovaovo_best = get_best_metric(ovaovo_mets)


# get test ovo + ova -----------------------------------------------
ovaovo_test = predict_ova(test_data, ova_best$metric[1], "ovaovo", cv[c])
ovaovo_test = predict_ovaOvo(test_data, ovaovo_test, ova_best$metric[1], cv[c])
ovaovo_test_conf = ovaovo_test$conf$table
save(ovaovo_test_conf, file = "data/classification/ovaovo_test_conf.Rdata")

ovaovo_test = as.data.frame(ovaovo_test$conf$byClass[,c(1,2,5,7)])
colnames(ovaovo_test) = c("sens","spec","prec","f1")
ovaovo_test = cbind(ap = c(1:13),ovaovo_test, cond = "test", metric = ova_best$metric[1])
ovaovo_test$model = "ovaovo"

ovo_plots = plot_test_ovo(ovaovo_best, ova_best$metric[1], ymin = 0)
grid.arrange(grobs = ovo_plots)

results[[cv[c]]][["ovaovo"]]$ovaovo_mets = ovaovo_mets
results[[cv[c]]][["ovaovo"]]$ovaovo_best = ovaovo_best
results[[cv[c]]][["ovaovo"]]$ovaovo_test = ovaovo_test

# plot all combined----------------------------------
# TRAIN 
ova_train = as.data.frame(ova_best)
ova_train = cbind(ova_train, model = "ova" )

multi_train = as.data.frame(multi_best)
multi_train = cbind(multi_train, model = "multi" )

ovo_train = cbind(ovo_best[,1:5],cond = "train",metric = ovo_test$metric, model = "ovo" )

ovaovo_train = ovaovo_best

all_train = rbind(ova_train, multi_train, ovo_train, ovaovo_train)


# TEST
ova_test = as.data.frame(ova_test$metric)
colnames(ova_test) = c("sens","spec","prec","f1")
ova_test = cbind(ap = c(1:13),ova_test, cond = "test", metric= "kappa", model = "ova" )

multi_test = as.data.frame(multi_test$metric)
colnames(multi_test) = c("sens","spec","prec","f1")
multi_test = cbind(ap = c(1:13),multi_test, cond = "test", metric= "kappa", model = "multi" )

ovo_test = cbind(ovo_test[,c(1:5)],cond = "test",metric = ovo_test$metric, model = "ovo" )

ovaovo_test = ovaovo_test

all_test = rbind(ova_test, multi_test, ovo_test, ovaovo_test)


results[[cv[c]]][["all_train"]] = all_train
results[[cv[c]]][["all_test"]] = all_test

###################

plot_comparison(all_train, cv[c], cond = "train")
plot_comparison(all_test, cv[c], cond = "test")

}

save(results, file = "data/classification/trainTest_results.Rdata")
