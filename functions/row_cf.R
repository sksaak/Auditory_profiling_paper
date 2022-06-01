row_cf <- function(x, y, df){
  sum(df[x,] == df[y,])/ncol(df)
}