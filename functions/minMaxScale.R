minMaxScale <- function(df){

  df <- data.matrix(df)

  for (c in 1:ncol(df)){

    # get column min/max
    xmin = min(df[,c], na.rm = TRUE)
    xmax = max(df[,c], na.rm = TRUE)

    # iterate across each rox and transform

    for (r in 1:nrow(df)){

      if (is.na(df[r,c])){
        df[r,c] <- NA
      } else{
        xnew = (df[r,c] - xmin) / (xmax - xmin)

        df[r,c] <- xnew
      }
    }
  }
  return(df)
}