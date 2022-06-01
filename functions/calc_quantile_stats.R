calc_quantile_stats <- function(df, q = c(0,0.1,0.9,1)){
  
  if (!require(stats)) install.packages('stats')
  library(stats)
  
  quants <- list()
  q <- q
  quants$q1 = stats::quantile(df, probs = q[1], na.rm = TRUE)
  quants$q2= stats::quantile(df, probs = q[2], na.rm = TRUE)
  quants$q3 = stats::quantile(df, probs = q[3], na.rm = TRUE)
  quants$q4= stats::quantile(df, probs = q[4], na.rm = TRUE)
  quants$median <- Rfast::colMedians(df, na.rm = TRUE)
  quants$sd <- Rfast::colVars(df, std = TRUE, na.rm = TRUE)
  quants$data <- df
  
  return (quants) 
}
