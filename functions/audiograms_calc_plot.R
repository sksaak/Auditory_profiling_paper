######### FUNCTIONS RELATED TO THE PLOTTING THE AUDIOGRAM ############

av_audiogram <- function(df, ac = TRUE, bc = TRUE, ucl = TRUE, asym = TRUE, abg = TRUE, q = c(0,0.25,0.75,1)) {
  # function to calculate average scores for the audiogram audiogram
  # 
  # if data for ac, bc, ucl, asym does not exist: set to false
  # 
  # df = data frame containing the audiogram
  # ac = air-conduction
  # bc = bone conduction
  # ucl = uncomfortable loudness scaling
  # asym = pta asymmetry score
  # q (quantiles) = defaults to [0,0.25,0.75,1], change if different quantile stats are required
  #
  #-----------------------------------------------------------------------------
  audiogram <- list()
  q <- q
  # ag_ac-----------------------------------------------------------------------  
  if (ac == TRUE){
    ac <-  tidyselect::vars_select(names(df), starts_with('ag_ac', ignore.case = TRUE)) 
    # if pta exists
    if (sum(ac == "ag_ac_pta") != 0){
      pta <- as.numeric(which(ac == "ag_ac_pta"))
      ag_ac_pta <- as.matrix(df[,ac[pta]])
      audiogram$ag_ac_pta$q_first = quantile(ag_ac_pta, probs = q[1], na.rm = TRUE)
      audiogram$ag_ac_pta$q_second= quantile(ag_ac_pta, probs = q[2], na.rm = TRUE)
      audiogram$ag_ac_pta$q_third = quantile(ag_ac_pta, probs = q[3], na.rm = TRUE)
      audiogram$ag_ac_pta$q_fourth= quantile(ag_ac_pta, probs = q[4], na.rm = TRUE)
      audiogram$ag_ac_pta$all <- ag_ac_pta
      audiogram$ag_ac_pta$median <- Rfast::colMedians(ag_ac_pta, na.rm = TRUE)
      audiogram$ag_ac_pta$sd <- Rfast::colVars(ag_ac_pta, std = TRUE, na.rm = TRUE)
      
      ac <- ac[-pta]
    }
    ag_ac<- as.matrix(df[,ac])
    
    for (freq in 1:ncol(ag_ac)){
      audiogram$ag_ac$q_first[freq] = quantile(ag_ac[,freq], probs = q[1], na.rm = TRUE)
      audiogram$ag_ac$q_second[freq] = quantile(ag_ac[,freq], probs = q[2], na.rm = TRUE)
      audiogram$ag_ac$q_third[freq] = quantile(ag_ac[,freq], probs = q[3], na.rm = TRUE)
      audiogram$ag_ac$q_fourth[freq] = quantile(ag_ac[,freq], probs = q[4], na.rm = TRUE)
    }
    audiogram$ag_ac$all <- ag_ac
    audiogram$ag_ac$median <- Rfast::colMedians(ag_ac, na.rm = TRUE)
    audiogram$ag_ac$sd <- Rfast::colVars(ag_ac, std = TRUE, na.rm = TRUE)
  }
  
  # ag_bc ----------------------------------------------------------------------
  if (bc == TRUE){
    bc <-  tidyselect::vars_select(names(df), starts_with('ag_bc', ignore.case = TRUE)) 
    # remove pta if exists from bc measures
    if (sum(bc == "ag_bc_pta") != 0){
      pta <- as.numeric(which(bc == "ag_bc_pta"))
      ag_bc_pta <- as.matrix(df[,bc[pta]])
      audiogram$ag_bc_pta$q_first = quantile(ag_bc_pta, probs = q[1], na.rm = TRUE)
      audiogram$ag_bc_pta$q_second= quantile(ag_bc_pta, probs = q[2], na.rm = TRUE)
      audiogram$ag_bc_pta$q_third = quantile(ag_bc_pta, probs = q[3], na.rm = TRUE)
      audiogram$ag_bc_pta$q_fourth= quantile(ag_bc_pta, probs = q[4], na.rm = TRUE)
     
      audiogram$ag_bc_pta$all <- ag_bc_pta
      audiogram$ag_bc_pta$median <- Rfast::colMedians(ag_bc_pta, na.rm = TRUE)
      audiogram$ag_bc_pta$sd <- Rfast::colVars(ag_bc_pta, std = TRUE, na.rm = TRUE)
   
      bc <- bc[-pta]
    }  
  ag_bc<- as.matrix(df[,bc])
  
  
  q_ag = matrix(data = NA, nrow = ncol(ag_bc), ncol = 4)
  
  for (freq in 1:ncol(ag_bc)){
    audiogram$ag_bc$q_first[freq] = quantile(ag_bc[,freq], probs = q[1], na.rm = TRUE)
    audiogram$ag_bc$q_second[freq] = quantile(ag_bc[,freq], probs = q[2], na.rm = TRUE)
    audiogram$ag_bc$q_third[freq] = quantile(ag_bc[,freq], probs = q[3], na.rm = TRUE)
    audiogram$ag_bc$q_fourth[freq] = quantile(ag_bc[,freq], probs = q[4], na.rm = TRUE)
  }
  audiogram$ag_bc$all <- ag_bc
  audiogram$ag_bc$median <- Rfast::colMedians(ag_bc, na.rm = TRUE)
  audiogram$ag_bc$sd <- Rfast::colVars(ag_bc, std = TRUE, na.rm = TRUE)

  }
  
  # ag_ucl --------------------------------------------------------------------- 
  if (ucl == TRUE){
    ucl <-  tidyselect::vars_select(names(df), starts_with('ag_ucl', ignore.case = TRUE)) 
    ag_ucl<-as.matrix(df[,ucl])
   
    q_ag = matrix(data = NA, nrow = ncol(ag_ucl), ncol = 4)
    for (freq in 1:ncol(ag_ucl)){
      audiogram$ag_ucl$q_first[freq] = quantile(ag_ucl[,freq], probs = q[1], na.rm = TRUE)
      audiogram$ag_ucl$q_second[freq] = quantile(ag_ucl[,freq], probs = q[2], na.rm = TRUE)
      audiogram$ag_ucl$q_third[freq] = quantile(ag_ucl[,freq], probs = q[3], na.rm = TRUE)
      audiogram$ag_ucl$q_fourth[freq] = quantile(ag_ucl[,freq], probs = q[4], na.rm = TRUE)
    }
    audiogram$ag_ucl$all <- ag_ucl
    audiogram$ag_ucl$median <- Rfast::colMedians(ag_ucl, na.rm = TRUE)
    audiogram$ag_ucl$sd <- Rfast::colVars(ag_ucl, std = TRUE, na.rm = TRUE)
  }
  
  # ag_pta_asym ----------------------------------------------------------------
  if (asym == TRUE){
    asym <-  tidyselect::vars_select(names(df), starts_with('ag_asym', ignore.case = TRUE)) 
    ag_pta_asym <- as.matrix(df[,asym])
    audiogram$ag_pta_asym$q_first = quantile(ag_pta_asym, probs = q[1], na.rm = TRUE)
    audiogram$ag_pta_asym$q_second= quantile(ag_pta_asym, probs = q[2], na.rm = TRUE)
    audiogram$ag_pta_asym$q_third = quantile(ag_pta_asym, probs = q[3], na.rm = TRUE)
    audiogram$ag_pta_asym$q_fourth= quantile(ag_pta_asym, probs = q[4], na.rm = TRUE)

    audiogram$ag_pta_asym$all <- ag_pta_asym
    audiogram$ag_pta_asym$median <- Rfast::colMedians(ag_pta_asym, na.rm = TRUE)
    audiogram$ag_pta_asym$sd <- Rfast::colVars(ag_pta_asym, std = TRUE, na.rm = TRUE)
  }
  
  audiogram$quantiles <- q
  
  if (abg == TRUE){
    abg <- tidyselect::vars_select(names(df), starts_with('ag_abg', ignore.case = TRUE))
    ag_abg <- as.matrix(df[,abg])
    audiogram$ag_abg$q_first = quantile(ag_abg, probs = q[1], na.rm = TRUE)
    audiogram$ag_abg$q_second= quantile(ag_abg, probs = q[2], na.rm = TRUE)
    audiogram$ag_abg$q_third = quantile(ag_abg, probs = q[3], na.rm = TRUE)
    audiogram$ag_abg$q_fourth= quantile(ag_abg, probs = q[4], na.rm = TRUE)
    
    audiogram$ag_abg$all <- ag_abg
    audiogram$ag_abg$median <- Rfast::colMedians(ag_abg, na.rm = TRUE)
    audiogram$ag_abg$sd <- Rfast::colVars(ag_abg, std = TRUE, na.rm = TRUE)
  }
  
  
  return (audiogram)
}


plot_audiogram <- function(audiogram, ac = TRUE, bc = TRUE, ucl = TRUE, asym = TRUE, abg = TRUE){
  # function to plot the audiogram 
  # 
  # if data for ac, bc, ucl, asym does not exist: set to false
  # 
  # audiogram : list containing data for ac, bc, ucl, asym, ac_pta, bc_pta
  # ac = air-conduction
  # bc = bone conduction
  # ucl = uncomfortable loudness scaling
  # asym = pta asymmetry score
  #-----------------------------------------------------------------------------
  
  get_legend<-function(myggplot){
    tmp <- ggplot_gtable(ggplot_build(myggplot))
    leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
    legend <- tmp$grobs[[leg]]
    return(legend)
  }
  
  
  q_col = "darkblue"
  sd_col = "darkblue"
  min_max_col = "darkgrey"
  alpha_ = 0.9
  width_ = 0.2
  size_ = 0.3
  size_p = 2
  
  # calculate quantiles as given into percentage
  perc <- as.character((audiogram$quantiles[3]-audiogram$quantiles[2])*100)
  if (audiogram$quantiles[1] == 0 && audiogram$quantiles[4] == 1){q1_4 = "Min/Max"
  }else{q1_4 = paste0(as.character((audiogram$quantiles[4]-audiogram$quantiles[1])*100), "%")}
 
  
  
  if (ac == TRUE){
    ag_ac <- data.frame(median = -audiogram$ag_ac$median, sd = audiogram$ag_ac$sd, idx = c(1:length(audiogram$ag_ac$median)),
                        q1 = -audiogram$ag_ac$q_first, q2 = -audiogram$ag_ac$q_second, q3= -audiogram$ag_ac$q_third, q4 = -audiogram$ag_ac$q_fourth)
    
    p_ag_ac <-ggplot(ag_ac, aes(x = idx))+ 
      geom_line(aes(y = median, colour = "Median")) + 
      geom_point(aes(y = median, colour = "Median")) + 
      geom_line(aes(y = median-sd, colour = "SD"))+ #sd
      geom_line(aes(y = median+sd, colour = "SD"))+ #sd
      geom_ribbon(aes(ymin=q2, ymax=q3, fill = paste0(perc, "%")), alpha=0.3)+ # q1:q2
      geom_line(aes(y = q1, colour = q1_4))+ #q1
      geom_line(aes(y = q4, colour = q1_4))+ #q2
      geom_hline(yintercept = 0) +
      geom_hline(yintercept = -20, linetype = 2, colour = "grey") +
      theme_bw() + 
     # theme(legend.position = "none") +#,
      #       legend.direction = "horizontal") +
      scale_fill_manual(name = "", labels = c(paste0(perc, "%")), values = q_col) +
      scale_color_manual(name = "", values = c("black", min_max_col, sd_col))+
      ylab("db")+
      scale_x_continuous("Hz", breaks=c(1:11), 
                         labels=c( "125", "250", "500", "750", "1000", "1500", "2000", "3000", "4000", "6000", "8000")) +
      scale_y_reverse() + 
      ylim(-120,10) #+ 
      ggtitle("Air conduction")
  }
  if (bc == TRUE){
    ag_bc <- data.frame(median = -audiogram$ag_bc$median, sd = audiogram$ag_bc$sd, idx = c(1:length(audiogram$ag_bc$median)),
                        q1 = -audiogram$ag_bc$q_first, q2 = -audiogram$ag_bc$q_second, q3= -audiogram$ag_bc$q_third, q4 = -audiogram$ag_bc$q_fourth)
    p_ag_bc <-ggplot(ag_bc, aes(x = idx))+ 
      geom_line(aes(y = median, colour = "median")) + 
      geom_point(aes(y = median, colour = "median")) + 
      geom_line(aes(y = median-sd, colour = "SD"))+ #sd
      geom_line(aes(y = median+sd, colour = "SD"))+ #sd
      geom_ribbon(aes(ymin=q2, ymax=q3, fill = paste0(perc, "%")), alpha=0.3)+ # q1:q2
      geom_line(aes(y = q1, colour = q1_4))+ #q1
      geom_line(aes(y = q4, colour = q1_4))+ #q2
      geom_hline(yintercept = 0) +
      geom_hline(yintercept = -20, linetype = 2, colour = "grey") +
      theme_bw() + 
      # theme(legend.position = c(0.9, 0.9),
      #       legend.direction = "horizontal") +
      scale_fill_manual(name = "", labels = c(paste0(perc, "%")), values = q_col) +
      scale_color_manual(name = "", values = c("black", min_max_col, sd_col))+
      ylab("db")+
      scale_x_continuous("Hz", breaks=c(1:11), 
                         labels=c( "125", "250", "500", "750", "1000", "1500", "2000", "3000", "4000", "6000", "8000")) +
      scale_y_reverse() + 
      ylim(-120,10) + 
      ggtitle("Bone conduction")  
    
  }
  if (ucl == TRUE){
    ag_ucl <- data.frame(median = -audiogram$ag_ucl$median, sd = audiogram$ag_ucl$sd, idx = c(1:length(audiogram$ag_ucl$median)),
                         q1 = -audiogram$ag_ucl$q_first, q2 = -audiogram$ag_ucl$q_second, q3= -audiogram$ag_ucl$q_third, q4 = -audiogram$ag_ucl$q_fourth)
    
    p_ucl <- ggplot(ag_ucl, aes(x = idx))+ 
      geom_line(aes(y = median, colour = "median")) + 
      geom_point(aes(y = median, colour = "median")) + 
      geom_line(aes(y = median-sd, colour = "SD"))+ #sd
      geom_line(aes(y = median+sd, colour = "SD"))+ #sd
      geom_ribbon(aes(ymin=q2, ymax=q3, fill = paste0(perc, "%")), alpha=0.3)+ # q1:q2
      geom_line(aes(y = q1, colour = q1_4))+ #q1
      geom_line(aes(y = q4, colour = q1_4))+ #q2
      geom_hline(yintercept = 0) +
      geom_hline(yintercept = -20, linetype = 2, colour = "grey") +
      theme_bw() + 
      # theme(legend.position = c(0.9, 0.9),
      #       legend.direction = "horizontal") +
      scale_fill_manual(name = "", labels = c(paste0(perc, "%")), values = q_col) +
      scale_color_manual(name = "", values = c("black", min_max_col, sd_col))+
      ylab("db")+
      scale_x_continuous("Hz", breaks=c(1:5), 
                         labels=c( "500", "1000", "2000", "3000", "4000")) +
      scale_y_reverse() + 
      ylim(-120,10) + 
      ggtitle("Uncomfortable loudness")  
  }
  
  if (ac == TRUE && bc == TRUE && asym == TRUE && abg == TRUE && ucl == TRUE){
    ag_pta <- data.frame(group = c("1", "2", "3", "4"),
                         median = c(-audiogram$ag_ac_pta$median,-audiogram$ag_bc_pta$median,-audiogram$ag_pta_asym$median, -audiogram$ag_abg$median),
                         SD = c(-audiogram$ag_ac_pta$sd,-audiogram$ag_bc_pta$sd,-audiogram$ag_pta_asym$sd, -audiogram$ag_abg$sd),
                         q1 = c(-audiogram$ag_ac_pta$q_first,-audiogram$ag_bc_pta$q_first,-audiogram$ag_pta_asym$q_first,-audiogram$ag_abg$q_first),
                         q2 = c(-audiogram$ag_ac_pta$q_second,-audiogram$ag_bc_pta$q_second,-audiogram$ag_pta_asym$q_second,-audiogram$ag_abg$q_second),
                         q3 = c(-audiogram$ag_ac_pta$q_third,-audiogram$ag_bc_pta$q_third,-audiogram$ag_pta_asym$q_third,-audiogram$ag_abg$q_third),
                         q4 = c(-audiogram$ag_ac_pta$q_fourth,-audiogram$ag_bc_pta$q_fourth,-audiogram$ag_pta_asym$q_fourth,-audiogram$ag_abg$q_fourth)
    )

    
    p_pta<- ggplot(data = ag_pta, aes(x = group, y = median)) +
      geom_tile(aes(height = q2-q3, width=0.3,  fill=paste0(perc, "%")), color="black", alpha = 0.8) +
      geom_segment(aes(x = group[1], xend = group[1], y = q1[1], yend = q4[1], color= q1_4))+
      geom_segment(aes(x = group[2], xend = group[2], y = q1[2], yend = q4[2], color= q1_4))+
      geom_segment(aes(x = group[3], xend = group[3], y = q1[3], yend = q4[3], color= q1_4))+
      geom_segment(aes(x = group[4], xend = group[4], y = q1[4], yend = q4[4], color= q1_4))+
      geom_hline(yintercept = 0)+
      geom_point(aes(y = median, colour = "median"), size = 2) +
      theme_bw()+
      scale_fill_manual(name = "", labels = c(paste0(perc, "%")), values = "grey") +
      scale_color_manual(name = "", values = c("black", "darkgrey"))+
      scale_x_discrete(labels = c("pta_ac", "pta_bc", "pta_asym", "ABG")) +
      ylim(-120,10)
    
   
  }
  
  if (ac == TRUE && bc == TRUE && ucl == TRUE){
    
    legend_ag <- get_legend(p_ag_ac)
    legend_pta <- get_legend(p_pta)
    
    p_ag_ac <- p_ag_ac + theme(legend.position = "none")
    p_ag_bc <- p_ag_bc + theme(legend.position = "none")
    p_ucl <- p_ucl + theme(legend.position = "none")
    p_pta <- p_pta + theme(legend.position = "none")
    
    p <- grid.arrange(p_ag_ac, p_ag_bc, legend_ag,
                      p_ucl, p_pta, legend_pta,
                      ncol = 3, 
                      top = "Median Cluster Audiogram",
                      widths = c(2.3,2.3,0.5))
    return(p)
    
  } else if (ac == TRUE && bc == TRUE && ucl == FALSE){
    
    legend_ag <- get_legend(p_ag_ac)
    legend_pta <- get_legend(p_pta)
    
    p_ag_ac <- p_ag_ac + theme(legend.position = "none")
    p_ag_bc <- p_ag_bc + theme(legend.position = "none")
    p_pta <- p_pta + theme(legend.position = "none")
    
    
    p <- grid.arrange(p_ag_ac, p_ag_bc, legend_ag,
                      p_pta, legend_pta,
                      ncol = 3, 
                      top = "median Cluster Audiogram",
                      widths = c(2.3,2.3,0.5))
    return(p)
    
  } else if (ac == TRUE && bc == FALSE && ucl == FALSE){
    return(p_ag_ac)
  }
  
}






get_bisgaard <- function(audiogram_data, bis_freqs){
  
  # bisgaard profiles 
  bisgaard_profiles <- data.frame(N1 = c(10, 10, 10,   10, 10, 15, 20, 30, 40), 
                                  N2 = c(20, 20, 22.5, 25, 30, 35, 40, 45, 50),
                                  N3 = c(35, 35, 35,   40, 45, 50, 55, 60, 65), 
                                  N4 = c(55, 55, 55,   55, 60, 65, 70, 75, 80), 
                                  N5 = c(65, 70, 72.5, 75, 80, 80, 80, 80, 80), 
                                  N6 = c(75, 80, 82.5, 85, 90, 90, 95, 100, 100),
                                  N7 = c(90, 95, 100, 105, 105, 105, 105, 105, 105),
                                  S1 = c(10, 10, 10,   10, 10, 15, 30, 55, 70),
                                  S2 = c(20, 20, 22.5, 25, 35, 55, 75, 95, 95),
                                  S3 = c(30, 35, 47.5, 60, 70, 75, 80, 80, 85) 
  )
  rownames(bisgaard_profiles) <- c(250, 500, 750, 1000, 1500, 2000, 3000, 4000, 6000)
  
  bisgaard <- NULL
  
  # loop over subjects of data
  for (s in 1:nrow(audiogram_data)){
    
    similarity_ag <- NULL
    # get the profile with the smallest difference to patient audiogram
    for (b in 1:ncol(bisgaard_profiles)){
      similarity_ag[b] <-   sum(abs(ag[s,bis_freqs] - bisgaard_profiles[,b]))
    }
    
    idx <- which.min(similarity_ag)
    
    bisgaard[s] <- idx
  }
  
  return (bisgaard)
  
}




plot_bisgaard <- function(df, bisgaard){
  ag_mat<- as.matrix(df[,which(colnames(df)=="ag_ac_125"):which(colnames(df)=="ag_ac_8000")])
  p <- list()  
  
  bisgaard_profiles <- data.frame(N1 = c(10, 10, 10,   10, 10, 15, 20, 30, 40), 
                                  N2 = c(20, 20, 22.5, 25, 30, 35, 40, 45, 50),
                                  N3 = c(35, 35, 35,   40, 45, 50, 55, 60, 65), 
                                  N4 = c(55, 55, 55,   55, 60, 65, 70, 75, 80), 
                                  N5 = c(65, 70, 72.5, 75, 80, 80, 80, 80, 80), 
                                  N6 = c(75, 80, 82.5, 85, 90, 90, 95, 100, 100),
                                  N7 = c(90, 95, 100, 105, 105, 105, 105, 105, 105),
                                  S1 = c(10, 10, 10,   10, 10, 15, 30, 55, 70),
                                  S2 = c(20, 20, 22.5, 25, 35, 55, 75, 95, 95),
                                  S3 = c(30, 35, 47.5, 60, 70, 75, 80, 80, 85),
                                  freq = c(250, 500, 750, 1000, 1500, 2000, 3000, 4000, 6000)) 
  
  for (b in 1:max(bisgaard)){
    
    idx <-  which(bisgaard == b)  
    ag_ac <- data.frame(median = Rfast::colMedians(ag_mat[idx,]),
                        sd = Rfast::colVars(ag_mat[idx,], std = TRUE), 
                        idx = c(1:length(Rfast::colMedians(ag_mat))),
                        orig_bisgaard = c(NA, -bisgaard_profiles[,b], NA))
    q_ag = matrix(data = NA, nrow = 11, ncol = 4)
    for (freq in 1:ncol(ag_mat)){
      q_ag[freq,1] = quantile(ag_mat[idx,freq], probs = 0)
      q_ag[freq,2] = quantile(ag_mat[idx,freq], probs = 0.10) 
      q_ag[freq,3] = quantile(ag_mat[idx,freq], probs = 0.90)
      q_ag[freq,4] = quantile(ag_mat[idx,freq], probs = 1)
    }
    
    ag_ac$min = -q_ag[,1] 
    ag_ac$q_25 = -q_ag[,2] 
    ag_ac$q_75 = -q_ag[,3] 
    ag_ac$max = -q_ag[,4] 
    
    error_col = "darkblue"
    alpha_ = 0.9
    width_ = 0.2
    size_ = 0.3
    size_p = 2
    lab_size = 17
    line_size = 2
    point_size = 3
    
    p[[b]] <- ggplot(ag_ac, aes(x = idx))+ 
      geom_hline(yintercept = 0) +
      geom_hline(yintercept = -20, linetype = 2, colour = "darkgrey") +
      geom_line(aes(y = -median, x = idx), size = line_size) + 
      geom_point(aes(y = -median, x = idx), size = point_size) + 
      geom_ribbon(aes(ymin=-median - sd, ymax=-median + sd), alpha=0.2, fill = "blue")+
      geom_line(aes(y = min, x = idx), colour = "darkgrey")+
      geom_line(aes(y = q_25, x = idx), colour = "blue")+
      geom_line(aes(y = q_75, x = idx), colour = "blue")+
      geom_line(aes(y = max, x = idx), colour = "darkgrey")+
      geom_line(data = ag_ac, aes(y = orig_bisgaard), colour = "red", size = line_size)+
      geom_point(data = ag_ac, aes(y = orig_bisgaard), colour = "red", alpha = 0.6, size = point_size)+
      theme_bw() + 
      ylab("db")+
      scale_x_continuous("Hz", breaks=c(1:11), 
                         labels=c( "125", "250", "500", "750", "1000", "1500", "2000", "3000", "4000", "6000", "8000")) +
      scale_y_reverse() + 
      ylim(-120,10) + 
      #ggtitle(paste0("Bisgaard ", as.character(b))) +
      theme(text = element_text(size=lab_size),
      ) 
  }
  
  return(p)
}