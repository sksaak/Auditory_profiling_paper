##################################################
#
#  Plot profile ranges
#
#
##################################################

# clear console, objects and plots 
cat("\014")  
rm(list = ls())
dev.off()

# get functions
source("code/functions/audiograms_calc_plot.R")
source("code/functions/acalos_calc_plot.R")
source("code/functions/get_clusters.R")
source("code/functions/get_legend.R")
source("code/functions/%not_in%.R")
source("code/functions/calc_quantile_stats.R")


plot_boxplot = function(df, feature){
  
  
  ggplot(data = df, aes(x = feature, y = profile, group = profile, fill = profile, colour = medcol))+
    geom_boxplot()+
    theme_bw()+
    scale_fill_viridis_d()+
    scale_color_manual(values = c(light = "white", dark = "black"))+
    geom_boxplot(color = "black", fill = NA, fatten = NULL)+
    theme(legend.position = "none")+
    xlab("") + ylab("")
  
  
}

# load data --------------------------------------------------------------
load("data/final_impset.Rdata")
load(paste0("data/clustering/mc_",selected_imp, ".Rdata"))
dat = data 
load(paste0("data/data_reduc.Rdata"))
load(paste0("data/bisgaard_idx.Rdata"))
load(paste0("data/acalos_params.Rdata"))
# merge features not used for clustering with cluster data -----
l_curve_1_5[is.nan(l_curve_1_5)] = NA
l_curve_4[is.nan(l_curve_4)] = NA
data$l_curve_1_5 = l_curve_1_5
data$l_curve_4 <- l_curve_4
data$bisgaard <- bisgaard

cluster <- get_clusters(mc,data)

## PLOTS --------------------------
profile_n = mc$G
profile_char = seq(from = 10, to = 9+profile_n, by = 1)
graphics.off()

# plotting parameters
lab_size = 15
text_size = 15
color_selected = c("darkblue", "darkorange")

#  plot all goesa -------------------------------------------------------------------------------
# 
srt_median <- c()
srt_lower <- c()
srt_upper <-c()

slope_median <- c()
slope_lower <- c()
slope_upper <-c()

all_srt = c()
for (c in 1:length(cluster)){
  goesa_srt <- calc_quantile_stats(as.matrix(cluster[[c]][["goesa_srt"]]), c(0,0.1,0.9,01))
  srt_median <- cbind(srt_median, goesa_srt$median)
  srt_lower <-cbind(srt_lower, goesa_srt$q2)
  srt_upper <-cbind(srt_upper, goesa_srt$q3)
  
  srt = cluster[[c]][["goesa_srt"]]
  tmp = data.frame(srt = srt, ap = c, ap_char = profile_char[c])
  
  all_srt = rbind(all_srt, tmp)
  goesa_slope <- calc_quantile_stats(as.matrix(cluster[[c]][["goesa_slope"]]), c(0,0.1,0.9,1))
  slope_median <- cbind(slope_median, goesa_slope$median)
  slope_lower <-cbind(slope_lower, goesa_slope$q2)
  slope_upper <-cbind(slope_upper, goesa_slope$q3)
}


goesa = data.frame(srt_median = as.numeric(srt_median),
                   srt_lower = as.numeric(srt_lower),
                   srt_upper = as.numeric(srt_upper),
                   slope_median = as.numeric(slope_median),
                   slope_lower= as.numeric(slope_lower),
                   slope_upper = as.numeric(slope_upper),
                   Profile = as.character(1:profile_n)
)

goesa_sorted <- goesa[order(srt_median, decreasing = TRUE),]
goesa_sorted$idx = as.character(profile_char)


df = dat
df$profile = factor(mc$classification, levels = goesa_sorted$Profile)
df = df[mc$uncertainty < 0.4,]
df$medcol = rep("dark", times = nrow(df))
df$medcol[df$profile %in% c(6,2,3,12)] = "light"
 

# srt ---------------
p_goesa_srt = plot_boxplot(df, df$goesa_srt)

# slope
p_goesa_slope = plot_boxplot(df, df$goesa_slope)

# digit triplet test
p_d3t_srt = plot_boxplot(df, df$d3t_L50)

# age
p_age = plot_boxplot(df, df$age)

# asym 
p_asym = plot_boxplot(df, df$ag_asym)

# abg
p_abg = plot_boxplot(df, df$ag_abg)

# ucl 
p_ucl = plot_boxplot(df, df$ag_ucl_pta)

# acalos diff (4 & 1.5 kHz)
p_aca_1_5_diff = plot_boxplot(df, df$acalos_diff_1_5)
p_aca_4_diff =plot_boxplot(df, df$acalos_diff_4)

## demtect
p_demtect = plot_boxplot(df, df$demtect)

## wst results 
p_wst = plot_boxplot(df, df$wst_raw)

## socio-economic status
p_swi = plot_boxplot(df, df$swi_sum)

## Audiogram----

ag_median <- c()
ag_lower <- c()
ag_upper <-c()

for (c in 1:length(cluster)){
  
  ag <- av_audiogram(cluster[[c]], q = c(0,0.1,0.9,1))
  ag_median <- cbind(ag_median, ag$ag_ac$median)
  ag_lower <-cbind(ag_lower, ag$ag_ac$q_second)
  ag_upper <-cbind(ag_upper, ag$ag_ac$q_third)
}

ag_median <- stack(as.data.frame(ag_median))
ag_lower <- stack(as.data.frame(ag_lower))
ag_upper <- stack(as.data.frame(ag_upper))

ag <- data.frame(median = ag_median$values,
                 lower = ag_lower$values,
                 upper = ag_upper$values,
                 Profile = ag_median$ind,
                 freq = c(0.125,0.25,0.5,0.75,1,1.5,2,3,4,6,8))

ag$Profile <- sapply(ag$Profile, function(x){substring(x, 2)})
ag_sorted <- ag
ag_sorted$Profile <- factor(ag_sorted$Profile, levels = levels(df$profile), ordered=TRUE)


p_ag_sorted <- ggplot(ag_sorted, aes(x = freq))+ 
  geom_ribbon(aes(ymin=-lower, ymax=-upper, group = Profile, fill = Profile), alpha=0.1)+ # q1:q2
  geom_line(aes(y = -median, group = Profile, colour = Profile)) + 
  geom_point(aes(y = -median, group = Profile, colour = Profile)) +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = -20, linetype = 2, colour = "grey") +
  theme_bw() + 
  theme(legend.position = "none")+
  ylab("")+
  scale_color_viridis_d()+
  scale_fill_viridis_d()+
  scale_x_continuous("Frequency [kHz]", breaks=c(0.125,0.25,0.5,1,2,4,8), 
                     labels=c( "0.125", "0.25", "0.5", "1", "2", "4", "8"),
                     trans = 'log2') +
  scale_y_reverse() + 
  ylab("Threshold [dB HL]")+
  ylim(-118,5) 

## acalos --------------------

acalos_median_1_5 <- c()
acalos_lower_1_5 <- c()
acalos_upper_1_5 <-c()
acalos_median_4 <- c()
acalos_lower_4 <- c()
acalos_upper_4 <-c()

cu = c(0,5,10,15,20,25,30,35,40,45,50)

for (c in 1:length(cluster)){
  
  acalos <- av_acalos(cluster[[c]])
  acalos_median_1_5 <- cbind(acalos_median_1_5, acalos[[1]][["curve"]])
  acalos_lower_1_5 <-cbind(acalos_lower_1_5, acalos[[1]][["q_2"]])
  acalos_upper_1_5 <-cbind(acalos_upper_1_5, acalos[[1]][["q_3"]])
  
  acalos_median_4 <- cbind(acalos_median_4, acalos[[2]][["curve"]])
  acalos_lower_4 <-cbind(acalos_lower_4, acalos[[2]][["q_2"]])
  acalos_upper_4 <-cbind(acalos_upper_4, acalos[[2]][["q_3"]])
}

acalos_median_1_5 <- stack(as.data.frame(acalos_median_1_5))
acalos_lower_1_5 <- stack(as.data.frame(acalos_lower_1_5))
acalos_upper_1_5 <- stack(as.data.frame(acalos_upper_1_5))
acalos_median_4 <- stack(as.data.frame(acalos_median_4))
acalos_lower_4 <- stack(as.data.frame(acalos_lower_4))
acalos_upper_4 <- stack(as.data.frame(acalos_upper_4))

acalos_1_5 <- data.frame(curve = acalos_median_1_5$values,
                         acalos_lower = acalos_lower_1_5$values,
                         acalos_upper = acalos_upper_1_5$values,
                         Profile = acalos_median_1_5$ind,
                         cu = rep(cu, times = length(cluster)),
                         ref = rep(acalos[[1]][["nh_ref"]], times = length(cluster)))

acalos_4 <- data.frame(curve = acalos_median_4$values,
                       acalos_lower = acalos_lower_4$values,
                       acalos_upper = acalos_upper_4$values,
                       Profile = acalos_median_4$ind,
                       cu = rep(cu, times = length(cluster)),
                       ref = rep(acalos[[2]][["nh_ref"]], times = length(cluster)))

acalos_1_5$Profile <- sapply(acalos_1_5$Profile, function(x){substring(x, 2)})
acalos_4$Profile <- sapply(acalos_4$Profile, function(x){substring(x, 2)})

acalos_1_5_sorted  <- acalos_1_5
acalos_1_5_sorted$Profile <- factor(acalos_1_5_sorted$Profile, levels = levels(df$profile), ordered=TRUE)
acalos_4_sorted  <- acalos_4
acalos_4_sorted$Profile <- factor(acalos_4_sorted$Profile, levels = levels(df$profile), ordered=TRUE)

p_1_5<- ggplot(data = acalos_1_5_sorted, aes(y = cu)) +
  geom_line(aes(x = ref), color = "black", size = 2) + 
  geom_line(aes(x = curve, group = Profile, color = Profile), size = 1) + 
  theme_bw() +
  theme(legend.position = "none") +
  scale_color_viridis_d()+
  scale_x_continuous(breaks=c(0,20,40,60,80,100,120,140), 
                     labels=c( "0", "20", "40", "60", "80", "100", "120","140"),
                     limits = c(-14,120), 
                     expand = c(0,2))+
  xlab("Level [dB HL]") + 
  ylab("Loudness [CU]") 

p_4 <- ggplot(data = acalos_4_sorted, aes(y = cu)) +
  geom_line(aes(x = ref), color = "black", size = 2) + 
  geom_line(aes(x = curve, group = Profile, color = Profile), size = 1) + 
  theme_bw() +
  theme(legend.position = "none")+
  scale_color_viridis_d()+
  scale_x_continuous(breaks=c(0,20,40,60,80,100,120,140), 
                     labels=c( "0", "20", "40", "60", "80", "100", "120","140"),
                     limits = c(-14,120), 
                     expand = c(0,2))+
  xlab("Level [dB HL]") + 
  ylab("Loudness [CU]") 


###############################################
png("plots/profile_ranges.png", width= 2480, height = 3508, res=200)
gridExtra::grid.arrange(
  # row 1
  p_goesa_srt+ xlab("SRT [dB SNR]")+ ylab("Profiles")+ ggtitle("GOESA SRT"),
  p_ag_sorted + ggtitle("Audiogram"),
  p_1_5 + ggtitle("ACALOS 1.5 kHz"),
  
  # row 2
  p_goesa_slope + xlab("Slope [%/dB]") + ylab("Profiles") +ggtitle("GOESA slope") ,
  p_asym + xlab("Asym. [dB]") + ylab("Profiles") + ggtitle("Asymmetry"),
  p_aca_1_5_diff + xlab("L35-L15 [dB HL]")+ ylab("Profiles")+ ggtitle("ACALOS 1.5 kHz") + xlim(0,70),
  
  # row 3
  p_d3t_srt + xlab("SRT [dB SNR]") + ylab("Profiles") + ggtitle("DTT SRT"), 
  p_abg + xlab("ABG [dB]") + ylab("Profiles") + ggtitle("Air-bone gap"),
  p_4+ ggtitle("ACALOS 4 kHz"),
  
  # row 4
  p_age + xlab("Age [years]") + ylab("Profiles")+ ggtitle("Age"),
  p_ucl + xlab("UCL [dB]") + ylab("Profiles") + ggtitle("UCL"),
  p_aca_4_diff + xlab("L35-L15 [dB HL]")+ ylab("Profiles")+ ggtitle("ACALOS 4 kHz") + xlim(0,70) ,
  
  # row 5
  p_demtect + xlab("Sum score")+ ylab("Profiles")+ ggtitle("DemTect"),
  p_wst + xlab("Raw score") + ylab("Profiles") + ggtitle("WST"),
  p_swi + xlab("Raw score") + ylab("Profiles")+ ggtitle("Socio-economic status"),
  
  ncol = 3, nrow = 5)
dev.off()
