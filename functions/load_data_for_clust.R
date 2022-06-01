load_data_for_clust <- function(){
  # load data
  load(paste0("data/data_reduc.Rdata"))
  load(paste0("data/bisgaard_idx.Rdata"))
  load(paste0("data/acalos_params.Rdata"))
  
  data$bisgaard <- order_bisgaard_by_PTA(bisgaard, data$ag_ac_pta)
  
  data <- cbind(data, acalos)
  
  data[,which(colnames(data)=="ag_ac_125"):which(colnames(data)=="ag_ucl_4000")] <- NULL
  data[,which(colnames(data)=="acalos_1_5_Lcut"):which(colnames(data)=="acalos_4_L50")] <- NULL
  
  dat <- as.data.frame(data)
  dat$bisgaard <- as.ordered(dat$bisgaard)
  dat$tinnitus <- as.ordered(dat$tinnitus)
  
  dat$acalos_1_5_L15[is.nan(dat$acalos_1_5_L15)] = NA
  dat$acalos_1_5_L35[is.nan(dat$acalos_1_5_L35)] = NA
  dat$acalos_4_L15[is.nan(dat$acalos_4_L15)] = NA
  dat$acalos_4_L35[is.nan(dat$acalos_4_L35)] = NA
  
  dat$acalos_diff_1_5 <- dat$acalos_1_5_L35 - dat$acalos_1_5_L15
  dat$acalos_diff_4 <- dat$acalos_4_L35 - dat$acalos_4_L15
  
  # remove data for HP (quiet/noise) & ACALOS L25
  dat$hp_noise = NULL
  dat$hp_quiet = NULL
  dat$acalos_1_5_L25 = NULL
  dat$acalos_4_L25 = NULL
  
  return(dat)
}