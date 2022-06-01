get_profile_similarity <- function(classification, mc, impsets, overall_similarity = TRUE){
  # compare row similarity of given class, (with all imps as starting value)
  #
  # overall_similarity: TRUE returns the mean across all imps, FALSE returns the means for each imp 
  #
  #----------------------------------
  
  
  if (!require(tidyverse)) install.packages('tidyverse')
  library(tidyverse)
  
  p_similarity = c()
  imp_similarity = c()
  for (imp in 1:length(impsets)){
    for (p in 1:mc$G){
      
      clas <- classification[order(classification[,imp]),]
      
      tmp <- clas[clas[,imp] == p,]
      
      res <- expand.grid(1:nrow(tmp), 1:nrow(tmp)) %>% 
        rename(row_1 = Var1, row_2 = Var2) %>% 
        rowwise() %>% 
        mutate(similarity = row_cf(row_1, row_2, tmp))
      
      p_similarity <- rbind(p_similarity, mean(res$similarity))
      
    }
    imp_similarity <- cbind(imp_similarity, p_similarity) 
    
    p_similarity <- c()
  }
  
  
  mean_similarity_imp = data.frame(mean = colMeans(imp_similarity))
  
  if (overall_similarity == "TRUE"){mean_sim <- mean(mean_similarity_imp$mean)}
  else if(overall_similarity == "FALSE"){mean_sim = mean_similarity_imp}
  else {warning("incorrect value for overall similarity. (TRUE/FALSE) needed")}
  
  return(mean_sim)
}