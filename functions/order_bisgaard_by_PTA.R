order_bisgaard_by_PTA <- function(bisgaard, pta){

  bis_N = 10

  bisgaard_raw <- matrix(data = NA, nrow = 10, ncol = 1)

  for (i in 1:bis_N){

    bisgaard_raw[i] <- mean(pta[bisgaard == i])
  }

  bisgaard_raw <- as.data.frame(bisgaard_raw)
  bisgaard_raw$bis <-c(1:10)

  bisgaard_raw <- bisgaard_raw[order(bisgaard_raw$V1, decreasing = FALSE),]
  bisgaard_raw$order = c(101:110)

  #  bis_minMax <- as.data.frame(bisgaard_raw)

  bisgaard_scaled<- c()
  vec <- bisgaard_raw$bis

  for (row in 1:length(bisgaard)){

    for (bis in 1:bis_N){
      if(bisgaard[row] == bisgaard_raw$bis[bis]){
        bisgaard_scaled[row] = bis
        next
      }
    }
  }


  return(bisgaard_scaled)
}