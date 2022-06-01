##################################################
#
#  Calculate classification agreement
#
#
##################################################

# clear console, objects and plots 
cat("\014")  
rm(list = ls())
dev.off()

# library
if (!require(psych)) install.packages('psych')
library(psych)

# functions
source("./code/functions/get_profile_similarity.R")
source("./code/functions/row_cf.R")

text_size = 17

#load("data/clustering/selected_imps.Rdata")

set.seed(22)

classifications <- c()

for (imp in 1:20){
  
  load(paste0("data/clustering/mc_",imp, ".Rdata"))
  
  classifications <- cbind(classifications, mc$classification)
  
}
  
# compare row similarity of given class, (with all imps as starting value)
mean_similarity_imp = get_profile_similarity(classifications,mc, impsets =  1:20, overall_similarity = FALSE)

# stats
mean(mean_similarity_imp$mean)
psych::SD(mean_similarity_imp$mean)
max(mean_similarity_imp$mean)


#png(filename = "plots/clusters/Profile_imps_ID_overlap.png", width = 600, height = 600)
ggplot(mean_similarity_imp, aes(x=mean)) + 
  geom_boxplot(fill = "lightblue")+
  theme_bw()+
  theme(text = element_text(size = text_size))+
  scale_x_continuous(limits = c(0.5,1), breaks = c(0.5,0.6,0.7,0.8,0.9,1))+
  ylab("Density")+
  xlab("Mean similarity of imputation sets")+
  ggtitle("Average similarity of profiles across imputation sets")
#dev.off()


# select the impset based on the most agreement with the largest overlap across impsets
selected_imp = which(mean_similarity_imp == max(mean_similarity_imp))

# store selected impset
save(selected_imp, file =  "data/final_impset.Rdata")




