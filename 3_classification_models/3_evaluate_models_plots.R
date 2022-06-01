#
#
# Model plots and feature  importance
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
if(!require(ggsci)) install.packages('ggsci')
if(!require(RColorBrewer)) install.packages('RColorBrewer')
library(ggplot2)
library(gridExtra)
library(ggsci)
library(RColorBrewer)

# functions
get_mean_mets = function(metrics, model, cv){
  
  mets = c("Kappa" ,"AUPRC", "balanced_accuracy", "F1")
  
  df = data.frame()
  for (m in mets){
    
    out = metrics
    out = out[out$metric == m,]
    sens = mean(out$sens[c(1:3,5:13)])
    prec = mean(out$prec[c(1:3,5:13)])
    f1 = mean(out$f1[c(1:3,5:13)])
    
    tmp = data.frame(sens = sens, prec = prec, f1 = f1, m = m, model = model, cv = cv)
    
    df = rbind(df, tmp)
    
  }
  return(df)
}

# load data
load("data/classification/trainTest_results.Rdata")
load("data/classification/ovaovo_test_conf.Rdata")

# Select final model on train set (Metric, CV, model) -------------------
ova = get_mean_mets(results$repeatedCV$ova$ova_mets, "ova", "repCV")
ova = rbind(ova, get_mean_mets(results$LOOCV$ova$ova_mets, "ova", "LOOCV"))
ovo = get_mean_mets(results$repeatedCV$ovo$ovo_mets, "ovo", "repCV")
ovo = rbind(ovo,get_mean_mets(results$LOOCV$ovo$ovo_mets, "ovo", "LOOCV"))
multi = get_mean_mets(results$repeatedCV$multi$multi_mets, "multi", "repCV")
multi = rbind(multi, get_mean_mets(results$LOOCV$multi$multi_mets, "multi", "LOOCV"))
ovaovo = get_mean_mets(results$repeatedCV$ovaovo$ovaovo_mets, "ovaovo", "repCV")
ovaovo = rbind(ovaovo, get_mean_mets(results$LOOCV$ovaovo$ovaovo_mets, "ovaovo", "LOOCV"))

data = rbind(ova, ovo, multi, ovaovo)


shapesize = 2
textsize = 9

# PLOTS -----------------------------------------

mean_f1_plot = ggplot()+
  geom_point(data = data[data$cv == "repCV",],aes(x = model, y = f1, group = m, color = m, shape = cv),
             size = shapesize, stat = "identity", position_dodge(width = 1) )+
  geom_point(data = data[data$cv == "LOOCV",],aes(x = model, y = f1, group = m, color = m, shape = cv),
             size = shapesize, stat = "identity", position_dodge(width = 1) )+
  theme_bw()+
  theme(text = element_text(size = textsize))+
  scale_color_viridis_d(name = "Metric", label= c("Kappa", "AUPRC", "BA", "F1-Score"))+
 # scale_color_manual(name = "Metric", label= c("Kappa", "AUPRC", "BA", "F1-Score"), values =pal_jco()(4))+
  scale_shape_discrete(name = "Cross-Validation")+
  #ylim(0.9,1)+
  ylab("Mean F1-Score")+
  xlab("Model")

mean_f1_plot

# Plot Train and Test results across profile for selected model ----------------

res = data[data$f1 == max(data$f1),]

model = results[[res$cv]][[res$model]]

train = model[[1]][model[[1]]$metric == res$m,]
test = model[[3]]

traintest = rbind(train,test)

cols = pal_jco()(4)
cols = c("#440154FF","#3CBB75FF")

sens = ggplot(data = traintest, aes(x = as.factor(ap), y =sens, group = cond))+
  geom_point(aes(color = cond, shape = cond), size = shapesize)+
  geom_hline(yintercept = mean(train$sens[c(1:3,5:13)]), colour = cols[1], linetype = "dashed")+
  geom_hline(yintercept = mean(test$sens[c(1:3,5:13)]), colour = cols[2], linetype = "dashed")+
  theme_bw()+
  theme(text = element_text(size = textsize), 
        legend.position = "bottom")+
  scale_color_manual(name = "", values = cols, 
                     breaks=c("train", "test"),
                     labels =c("Train", "Test") )+
  scale_shape_discrete(name = "",
                       breaks=c("train", "test"),
                       labels =c("Train", "Test"))+
  xlab("Profile") +
  ylab("Sensitivity")+
  ylim(0,1)

legend = get_legend(sens)
sens = sens + theme(legend.position = "none")

prec = ggplot(data = traintest, aes(x = as.factor(ap), y =prec, group = cond))+
  geom_point(aes(color = cond, shape = cond), size = shapesize)+
  geom_hline(yintercept = mean(train$prec[c(1:3,5:13)]), colour = cols[1], linetype = "dashed")+
  geom_hline(yintercept = mean(test$prec[c(1:3,5:13)]), colour = cols[2], linetype = "dashed")+
  theme_bw()+
  theme(text = element_text(size = textsize), 
        legend.position = "none")+
  scale_color_manual(name = "", values = cols, 
                     breaks=c("train", "test"),
                     labels =c("Train", "Test") )+
  scale_shape_discrete(name = "",
                       breaks=c("train", "test"),
                       labels =c("Train", "Test"))+
  xlab("Profile") +
  ylab("Precision")+
  ylim(0,1)

# Train test (Sensitivity, Precision)
grid.arrange(arrangeGrob(sens, prec, nrow=1),
            legend, nrow=2,heights=c(10, 1))

