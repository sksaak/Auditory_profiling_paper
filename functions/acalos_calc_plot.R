##### Functions to calculate and plot acalos 

av_acalos <- function(df, q = c(0,0.1,0.9,1)){
  # calculate average scores for the loudness curve 
  # 
  # df: data.frame containing the loudness curve for 1.5 kHz and 4 kHz
  #    -> acalos stored in the form: acalos_l_curve_1_5 & acalos_1_5_L2_5
  #
  #-------------------------------------------------------------------------

  # libraries
  if (!require(matrixStats)) install.packages('matrixStats')
  library(matrixStats)
  # libraries
  if (!require(Rfast)) install.packages('Rfast')
  library(Rfast)
  
  
    q <- q
  
  # acalos_NH: normal hearing reference 
  l_curve_1_5_NH <- c(-0.6682508,15.8333993,32.3350495,48.8366997,63.8191218,75.7630880,84.6685981,90.5356522,94.8834783,99.2313043,103.5791304)
  l_curve_4_NH <- c(5.615556,19.504444,33.393333,47.282222,60.041796,70.542740,78.785054,84.768738,89.623107,94.477476,99.331845)
  

  # extract loudness curves
  l_curve_1_5 <- df$l_curve_1_5
  l_curve_4 <- df$l_curve_4
 

  
  cu = c(0,5,10,15,20,25,30,35,40,45,50)
  
  tmp_df_1_5 <- data.frame(curve = Rfast::colMedians(l_curve_1_5, na.rm = TRUE),
                           std = colSds(l_curve_1_5, na.rm = TRUE),
                           q_1 = colQuantiles(l_curve_1_5, probs = q[1], na.rm = TRUE),
                           q_2 = colQuantiles(l_curve_1_5, probs = q[2], na.rm = TRUE),
                           q_3 = colQuantiles(l_curve_1_5, probs = q[3], na.rm = TRUE),
                           q_4 = colQuantiles(l_curve_1_5, probs = q[4], na.rm = TRUE),
                           nh_ref = l_curve_1_5_NH,
                           y_scale = cu)
  tmp_df_4 <- data.frame(curve = colMedians(l_curve_4, na.rm = TRUE),
                         std = colSds(l_curve_4, na.rm = TRUE),
                         q_1 = colQuantiles(l_curve_4, probs = q[1], na.rm = TRUE),
                         q_2 = colQuantiles(l_curve_4, probs = q[2], na.rm = TRUE),
                         q_3 = colQuantiles(l_curve_4, probs = q[3], na.rm = TRUE),
                         q_4 = colQuantiles(l_curve_4, probs = q[4], na.rm = TRUE),
                         nh_ref = l_curve_4_NH,
                         y_scale = cu)
  av_acalos <-   list(tmp_df_1_5)
  av_acalos[[2]] <-   tmp_df_4
  av_acalos[[3]] <- l_curve_1_5
  av_acalos[[4]] <- l_curve_4
  av_acalos$q <- q
  
  return(av_acalos)
  
}


plot_acalos <- function(df){
  # plots acalos given an output from av_acalos() 
  #
  # df: output of av_acalos
  #
  #
  #
  #-----------------------------------------------------------------------------
  
  get_legend<-function(myggplot){
    tmp <- ggplot_gtable(ggplot_build(myggplot))
    leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
    legend <- tmp$grobs[[leg]]
    return(legend)
  }
  
  
  df_1_5 <- df[[1]]
  df_4 <- df[[2]]
  acalos_1_5 <- df[[3]]
  acalos_4 <- df[[4]]
  
  q_col = "darkblue"
  min_max_col = "grey"
  sd_col = "darkblue"
  
  # calculate quantiles as given into percentage
  perc <- as.character((df$q[3]-df$q[2])*100)
  if (df$q[1] == 0 && df$q[4] == 1){q1_4 = "Min/Max"
  }else{q1_4 = paste0(as.character((df$q[4]-df$q[1])*100), "%")}
  
  
  x_limits = c(-20,170)
  alp = 0.7
  
  p_1_5 <- ggplot(data = df_1_5, aes(y = y_scale)) +
    geom_line(aes(x = curve, color = "Median"), size = 1.1) + 
 #   geom_line(aes(x = q_1, color = q1_4), linetype = "dashed")+
    geom_line(aes(x = curve-std, color = "SD"), linetype = "dashed")+
    geom_line(aes(x = curve+std, color= "SD"), linetype = "dashed")+
 #   geom_line(aes(x = q_4, color = q1_4), linetype = "dashed")+
    geom_ribbon(aes(xmin=q_2, xmax=q_3, fill = perc), alpha=0.3)+
    geom_line(aes(x = nh_ref, color = "NH Ref."), size = 1.1) + 
    theme_bw() +
    scale_fill_manual(name = "", labels = c(paste0(perc, "%")), values = q_col) +
    scale_color_manual(name = "", values = c("darkblue",  "black", sd_col))+
    theme(legend.position = "none")+
   
    xlab("Level [dB HL]") + 
    ylab("Loudness [CU]") +
    xlim( x_limits) +
    ggtitle("1.5 kHz")
  
  p_4 <- ggplot(data = df_4, aes(y = y_scale)) +
    geom_line(aes(x = curve, color = "Median"), size = 1.1) + 
    geom_line(aes(x = q_1, color = q1_4), linetype = "dashed")+
    geom_line(aes(x = curve-std, color = "SD"), linetype = "dashed")+
    geom_line(aes(x = curve+std, color= "SD"), linetype = "dashed")+
    geom_line(aes(x = q_4, color = q1_4), linetype = "dashed")+
    geom_ribbon(aes(xmin=q_2, xmax=q_3, fill = perc), alpha=0.3)+
    geom_line(aes(x = nh_ref, color = "NH Ref."), size = 1.1) + 
    theme_bw() +
    scale_fill_manual(name = "", labels = c(paste0(perc, "%")), values = q_col) +
    scale_color_manual(name = "", values = c("darkblue", min_max_col, "black", sd_col))+
    theme(legend.position = "bottom") +
    xlab("Level [dB HL]") + 
    ylab("Loudness [CU]") +
    xlim( x_limits) +
    ggtitle("4 kHz")
  
  
  p_dens_1_5 <- ggplot(data = acalos_1_5) +
    geom_density(aes(x = L2_5, fill = "L2_5"), alpha = alp) +
    geom_segment(aes(x = mean(L2_5), y = 0, xend = mean(L2_5), yend = 0.1),
                 linetype = "dashed", color= "darkred") +
    geom_density(aes(x = L25, fill = "L25"), alpha = alp) +
    geom_segment(aes(x = mean(L25), y = 0, xend = mean(L25), yend = 0.1),
                 linetype = "dashed", color= "darkgreen") +
    geom_density(aes(x = L50, fill = "L50"), alpha = alp) +
    geom_segment(aes(x = mean(L50), y = 0, xend = mean(L50), yend = 0.1),
                 linetype = "dashed", color= "blue") +
    theme_bw() +
    guides(fill=guide_legend(title="CU"))+
    theme(legend.position = "none")+
    xlim(x_limits)+
    ylim(0,0.1) +
    xlab("Level [dB HL]") +
    ggtitle("1.5 kHz")
  
  
  p_dens_4 <- ggplot(data = acalos_4) +
    geom_density(aes(x = L2_5, fill = "L2_5"), alpha = alp) +
    geom_segment(aes(x = mean(L2_5), y = 0, xend = mean(L2_5), yend = 0.1),
                 linetype = "dashed", color= "darkred") +
    geom_density(aes(x = L25, fill = "L25"), alpha = alp) +
    geom_segment(aes(x = mean(L25), y = 0, xend = mean(L25), yend = 0.1),
                 linetype = "dashed", color= "darkgreen") +
    geom_density(aes(x = L50, fill = "L50"), alpha = alp) +
    geom_segment(aes(x = mean(L50), y = 0, xend = mean(L50), yend = 0.1),
                 linetype = "dashed", color= "blue") +
    theme_bw() +
    guides(fill=guide_legend(title="CU"))+
    theme(legend.position = "bottom")+
    xlim(x_limits)+
    ylim(0,0.1) +
    xlab("Level [dB HL]") +
    ggtitle("4 kHz")
  
  legend_curve <- get_legend(p_4)
  legend_dens <- get_legend(p_dens_4)
  
  p_4 <- p_4 + theme(legend.position="none")
  p_dens_4 <- p_dens_4 + theme(legend.position = "none")
  
  p <- grid.arrange(p_1_5, p_dens_1_5,
                    p_4, p_dens_4,
                    legend_curve, legend_dens,
                    ncol = 2, nrow = 3, 
                    top = "Adaptive categorical loudness scaling",
                    widths= c(2.3,2.3),
                    heights= c(2.2,2.2,0.5))
  
  return(p)
  
  
  
}
