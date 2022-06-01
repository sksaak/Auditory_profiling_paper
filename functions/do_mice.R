do_mice <- function(df, savenumber){
  #
  # df = dataframe to be imputed
  # savenumber = identifier on how to save imp
  #--------------------------------------
  
  
  # initialize predictor matrix 
  pred.Matrix <- 1 - diag( 1, ncol(df) )
  rownames( pred.Matrix ) <- colnames( pred.Matrix) <- colnames(df )
  
  pred.Matrix <- quickpred(df, 
                           exclude=NULL,
                           mincor = 0.1)
  
  #-----------------------
  #************************************************************
  # MICE imputation with method randomForest
  
  cl = unlist(lapply((sapply(df, class)), "[[",1))

  meth = as.character(ifelse(cl == "numeric", "rf", "cart"))
 
  
  mi.res <- mice(df, predictorMatrix = pred.Matrix,
                 seed = 22, 
                 method = meth,
                 maxit = 20,
                 m = 20)
  
  dir.create(file.path("data/MICE/"), recursive = TRUE)  
  save(mi.res, file = paste0("./data/MICE_", savenumber, ".Rdata"))
  
  return(mi.res)
}