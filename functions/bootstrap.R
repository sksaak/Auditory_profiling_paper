
bootstrap <- function(df, method ="resample"){
  
  df <- as.data.frame(df)
  
  if (method == "resample"){ 
    ids = sort(sample(1:nrow(df),size = nrow(df), replace = TRUE))
  }else if (method == "subsample"){
    ids = sort(sample(1:nrow(df),size = round(nrow(df)*0.9), replace = FALSE))
  }
  
  new_dat <- c()
  for (r in ids){
    new_dat <- rbind(new_dat, df[r,])
  }
  
  return(new_dat)
}
