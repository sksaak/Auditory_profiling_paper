##################################################
#
#  Merging bootstrap clusters
#
#
##################################################

# clear console, objects and plots 
cat("\014")  
rm(list = ls())
dev.off()

# libraries
if (!require(ggplot2)) install.packages('ggplot2')
if (!require(tiff)) install.packages('tiff')
library(ggplot2)
library(tiff)

# functions
source("./code/functions/getmode.R")

set.seed(22)

k = c()
cov = c()
sim = c()

for (imp in 1:1000){ # 1000 bootstrap iterations

load(paste0("./data/bootstrap_result/results_", imp, ".Rdata"))

  k = rbind(k, mod$model$num_k)
  cov = rbind(cov, as.character(mod$model$cov))
  
  sim = rbind(sim, result$similarity)
  
}

df <- data.frame(k = as.factor(k), cov = cov, sim = sim)

# Numer of profiles
ggplot(data = df, aes(x = k) )+
  geom_bar(stat = "count", fill="#2D708EFF")+
  theme_bw()+
  theme(text = element_text(size = 9))+
  xlab("Number of Profiles")+
  ylab("Frequency")

# Covariance parameterization
ggplot(data = df, aes(x = cov))+
  geom_bar(stat = "count")+
  theme_bw()+
  theme(text = element_text(size = 17))+
  xlab("Covariance")+
  ylab("Frequency")

# Similarity across completed data sets
ggplot(data = df, aes(y = sim, x = 1))+
  geom_violin(draw_quantiles = c(0.25,0.5,0.75), fill = "lightblue")+
  theme_bw()+
  theme(text = element_text(size = 17), 
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank())+
  xlab("")+
  ylab("Similarity [%]")+
  ggtitle("Similarity across completed datasets")


final_cov = getmode(cov)
final_k = getmode(k)

# save results
save(final_cov, final_k, file= "./data/bootstrap_results.Rdata")
  

