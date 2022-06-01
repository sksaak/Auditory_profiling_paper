get_clusters <- function(mc, data){
  #
  # gets data for each cluster
  #--------------------------------------
  
  cluster = c()
  for (c in 1:max(mc$classification)){
    
    dat <- data[(mc$classification ==  c & mc$uncertainty < 0.4),]
    cluster[[c]] <- dat #dat[c(1:4,10:61)]
  }
  return(cluster)
}