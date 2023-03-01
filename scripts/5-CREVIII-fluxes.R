#----------------------------------------------------------------------------------------------------------------------------#
#      CRE VIII- Fluxes, YL May 22                                                                                             #
#----------------------------------------------------------------------------------------------------------------------------#

#----------------------------------------------------------------------------------------------------------------------------#
#Library, directory & data                                                                                                   #
#----------------------------------------------------------------------------------------------------------------------------#

# library
library(ibts)
library(data.table)
library(readxl)
library(ggplot2)

# paths
Path <- '...'
PathbLS <- file.path(Path, "/bls_run_results_summer_time")

# read in crds
crds <- readRDS(file.path(Path, "/crds_cest.rds"))

# read in bls with 14 degrees correction 
bls <- readRDS(file.path(PathbLS, "/bls-cre22-results.rds"))

#join bLS and concentrations
bls_con <- merge(
  bls ,crds,
  by = "Time"
)

#----------------------------------------------------------------------------------------------------------------------------#
# Calculate QbLS                                                                                                             #
#----------------------------------------------------------------------------------------------------------------------------#

# calculate mgs
bls_con$nh3_mgs <- 0
bls_con$ch4_mgs <- 0
for (i in 1:nrow(bls_con)){
  if (bls_con$Sensor[i] == "LineSensor1")
  {
    bls_con$nh3_mgs[i] <- (bls_con$nh3_line1_emission_ugm3[i] / bls_con$CE[i] * bls_con$SourceArea[i]) / 1000
  }
  else
  {
    bls_con$nh3_mgs[i] <- (bls_con$nh3_line2_emission_ugm3[i] / bls_con$CE[i] * bls_con$SourceArea[i]) / 1000
  }
}
for (i in 1:nrow(bls_con)){
  if (bls_con$Sensor[i] == "LineSensor1")
  {
    bls_con$ch4_mgs[i] <- (bls_con$ch4_line1_emission_ugm3[i] / bls_con$CE[i] * bls_con$SourceArea[i]) / 1000
  }
  else
  {
    bls_con$ch4_mgs[i] <- (bls_con$ch4_line2_emission_ugm3[i] / bls_con$CE[i] * bls_con$SourceArea[i]) / 1000
  }
}


# add release
bls_con$nh3_rel_mgs <- 14.46
bls_con$nh3_rel_sd_mgs <- 0.3

bls_con$ch4_rel_mgs <- 118.85
bls_con$ch4_rel_sd_mgs <- 2.8

# calculate % measured
bls_con$nh3_perct <- (as.numeric(bls_con$nh3_mgs)/as.numeric(bls_con$nh3_rel_mgs))*100
bls_con$ch4_perct <- (as.numeric(bls_con$ch4_mgs)/as.numeric(bls_con$ch4_rel_mgs))*100

#----------------------------------------------------------------------------------------------------------------------------#
# Add information                                                                                                            #
#----------------------------------------------------------------------------------------------------------------------------#
# add release name
bls_con$Release <- "VIII-DK" 

# add distance
bls_con$Distance_m <- 15  

#----------------------------------------------------------------------------------------------------------------------------#
# Plot to select the ranges                                                                                                  #
#----------------------------------------------------------------------------------------------------------------------------#

p1 <- ggplot(bls_con, aes(x=Time)) +
  geom_point(data = bls_con[Sensor == "LineSensor1"],aes(y=nh3_mgs, color = "a", shape = "a"), size =5, stroke = 1.2)+
  geom_point(data = bls_con[Sensor == "LineSensor2"],aes(y=nh3_mgs, color = "b", shape = "b"), size =5, stroke = 1.2)+
  geom_point(data = bls_con[Sensor == "LineSensor1"][6:16],aes(y=nh3_mgs, color = "c", shape = "c"), size =8, stroke = 1.2)+
  geom_point(data = bls_con[Sensor == "LineSensor2"][6:16],aes(y=nh3_mgs, color = "c", shape = "c"), size =8, stroke = 1.2)+
  geom_hline(yintercept = 14.46, color = "#99CCCC")+
  scale_color_manual("", values = c("#FF9933", "#003333", "#99CCCC"), 
                     labels = c("Line 1 163cm", "Line 2 187", "SEL"))+
  scale_shape_manual("", values = c(3,4,1), 
                     labels = c("Line 1 163cm", "Line 2 187", "SEL"))+
  ggtitle("CRE 212 (VIII) 50cm ")+
  ylab(expression(paste("NH"[3]* " [mg s"^-1*"]"))) +
  theme(legend.position = "bottom")


p2 <- ggplot(bls_con, aes(x=Time)) +
  geom_point(data = bls_con[Sensor == "LineSensor1"],aes(y=ch4_mgs, color = "a", shape = "a"), size =5, stroke = 1.2)+
  geom_point(data = bls_con[Sensor == "LineSensor2"],aes(y=ch4_mgs, color = "b", shape = "b"), size =5, stroke = 1.2)+
  geom_point(data = bls_con[Sensor == "LineSensor1"][6:16],aes(y=ch4_mgs, color = "c", shape = "c"), size =8, stroke = 1.2)+
  geom_point(data = bls_con[Sensor == "LineSensor2"][6:16],aes(y=ch4_mgs, color = "c", shape = "c"), size =8, stroke = 1.2)+
  geom_hline(yintercept = 118.85, color = "#99CCCC")+
  scale_color_manual("", values = c("#FF9933", "#003333", "#99CCCC"), 
                     labels = c("Line 1 163cm", "Line 2 187", "SEL"))+
  scale_shape_manual("", values = c(3,4,1), 
                     labels = c("Line 1 163cm", "Line 2 187", "SEL"))+
  ggtitle("CRE 212 (VIII) 50cm")+
  ylab(expression(paste("CH"[4]* " [mg s"^-1*"]"))) +
  theme(legend.position = "bottom")

p3 <- ggplot(bls_con, aes(x=Time)) +
  geom_point(data = bls_con[Sensor == "LineSensor1"],aes(y=nh3_mgs, color = "a", shape = "a"), size =5, stroke = 1.2)+
  geom_point(data = bls_con[Sensor == "LineSensor1"],aes(y=ch4_mgs/5, color = "b", shape = "b"), size =5, stroke = 1.2)+
  geom_point(data = bls_con[Sensor == "LineSensor1"][6:16],aes(y=nh3_mgs, color = "c", shape = "c"), size =8, stroke = 1.2)+
  geom_point(data = bls_con[Sensor == "LineSensor1"][6:16],aes(y=ch4_mgs/5, color = "c", shape = "c"), size =8, stroke = 1.2)+
  geom_hline(yintercept = 14.46, color = "#FF9933")+
  geom_hline(yintercept = 118.85/5, color = "#003333")+
  scale_color_manual("", values = c("#FF9933", "#003333", "#99CCCC"), 
                     labels = c(expression(paste("NH"[3])), expression(paste("CH"[4])), "SEL"))+
  scale_shape_manual("", values = c(3,4,1), 
                     labels = c(expression(paste("NH"[3])), expression(paste("CH"[4])), "SEL"))+
  ggtitle("CRE 212 (VIII) 50cm 14 WD Line 1")+
  ylab(expression(paste("NH"[3]* " [mg s"^-1*"]"))) +
  scale_y_continuous(sec.axis = sec_axis(~ . * 10, 
                                         name = expression(paste("CH"[4]* "[mg s"^-1*"]"))))+
  theme(legend.position = "bottom")

p4 <- ggplot(bls_con, aes(x=Time)) +
  geom_point(data = bls_con[Sensor == "LineSensor2"],aes(y=nh3_mgs, color = "a", shape = "a"), size =5, stroke = 1.2)+
  geom_point(data = bls_con[Sensor == "LineSensor2"],aes(y=ch4_mgs/5, color = "b", shape = "b"), size =5, stroke = 1.2)+
  geom_point(data = bls_con[Sensor == "LineSensor2"][6:16],aes(y=nh3_mgs, color = "c", shape = "c"), size =8, stroke = 1.2)+
  geom_point(data = bls_con[Sensor == "LineSensor2"][6:16],aes(y=ch4_mgs/5, color = "c", shape = "c"), size =8, stroke = 1.2)+
  geom_hline(yintercept = 14.46, color = "#FF9933")+
  geom_hline(yintercept = 118.85/5, color = "#003333")+
  scale_color_manual("", values = c("#FF9933", "#003333", "#99CCCC"), 
                     labels = c(expression(paste("NH"[3])), expression(paste("CH"[4])), "SEL"))+
  scale_shape_manual("", values = c(3,4,1), 
                     labels = c(expression(paste("NH"[3])), expression(paste("CH"[4])), "SEL"))+
  ggtitle("CRE 212 (VIII) 50cm 14 WD Line 2")+
  ylab(expression(paste("NH"[3]* " [mg s"^-1*"]"))) +
  scale_y_continuous(sec.axis = sec_axis(~ . * 10, 
                                         name = expression(paste("CH"[4]* "[mg s"^-1*"]"))))+
  theme(legend.position = "bottom")

# export the select time
## option 1
#bls_con_sel <- bls_con[Time >= "2022-04-22 13:30:00"]
#bls_con_sel <- bls_con_sel[Time < "2022-04-22 15:15:00"]

## option 2
bls_con_sel <- bls_con[Time >= "2022-04-22 12:30:00"]
bls_con_sel <- bls_con_sel[Time < "2022-04-22 15:15:00"]

# add soilT, RH and SR
bls_con_sel[, ":="(
  soilT = as.numeric(c(8.29, 8.29, 8.34, 8.34, 8.47, 8.47, 8.60, 8.60, 8.64, 8.64, 8.68, 8.68, 8.80, 8.80, 8.87, 
                       8.87, 8.97, 8.97, 9.00, 9.00, 9.08, 9.08)),
  RH = as.numeric(c(54.91, 54.91, 54.62, 54.62, 53.65, 53.65, 52.55, 52.55, 52.20, 52.20, 51.18, 51.18, 50.89, 50.89,
                    50.50, 50.50, 48.76, 48.76, 48.36, 48.36, 48.06, 48.06)),
  SR = as.numeric(c(753.45, 753.45, 752.85, 752.85, 743.51, 743.51, 726.19, 726.19, 718.36, 718.36, 706.01, 706.01, 
                    681.31, 681.31, 668.81, 668.81, 639.75, 639.75, 626.19, 626.19, 592.76, 592.76)),
  Pair = as.numeric(c(1009.10, 1009.10, 1009.20, 1009.20, 1009.10, 1009.10, 1009.00, 1009.00, 1009.00, 1009.00, 1008.90, 
                      1008.90, 1008.80, 1008.80, 1008.70, 1008.70, 1008.60, 1008.60, 1008.60, 1008.60, 1008.60, 1008.60 ))
)]

saveRDS(bls_con_sel, file = file.path(Path, "cre21_VIII_selec_results.rds"))

#----------------------------------------------------------------------------------------------------------------------------#
# Compararing lines                                                                                                          #
#----------------------------------------------------------------------------------------------------------------------------#

# commparing raw concentration

bls_con_sel$ration_ch4nh3_raw_line1 <- (bls_con_sel$ch4_line1_emission_ppm*1000)/bls_con_sel$nh3_line1_emission_ppb 
bls_con_sel$ration_ch4nh3_raw_line2 <- (bls_con_sel$ch4_line2_emission_ppm*1000)/bls_con_sel$nh3_line2_emission_ppb 

#lm <- lm(ration_ch4nh3_raw_line1 ~ ration_ch4nh3_raw_line2, bls_con_sel)
lm <- lm(ration_ch4nh3_raw_line2 ~ ration_ch4nh3_raw_line1, bls_con_sel)
lm_0 <- lm(ration_ch4nh3_raw_line2 ~ 0 + ration_ch4nh3_raw_line1, bls_con_sel)


p5 <- ggplot()+
  geom_point(data = bls_con_sel, aes(x=ration_ch4nh3_raw_line1, y =ration_ch4nh3_raw_line2)) +
  geom_abline(intercept = 0)+
  #geom_abline(intercept = 0, slope = 8.21, color = "red")+
  ylab("CH4/NH3 Line1")+
  xlab("CH4/NH3 Line2")+
  ylim(c(0,13))+
  xlim(c(0,13))


# comparing fluxes
bls_con_sel$ratio_ch4nh3 <-  bls_con_sel$ch4_mgs/bls_con_sel$nh3_mgs
lm_a <- lm(bls_con_sel[Sensor== "LineSensor2"]$ratio_ch4nh3 ~ bls_con_sel[Sensor== "LineSensor1"]$ratio_ch4nh3, bls_con_sel)

p6 <- ggplot()+
  geom_point(data = bls_con_sel[Sensor == "LineSensor2"], aes(x=bls_con_sel[Sensor== "LineSensor1"]$ratio_ch4nh3, 
                              y =bls_con_sel[Sensor== "LineSensor2"]$ratio_ch4nh3)) +
  geom_abline()+
  geom_abline(intercept = 0, slope = 8.21, color = "red")+
  ylab("CH4/NH3 Line1")+
  xlab("CH4/NH3 Line2")


#----------------------------------------------------------------------------------------------------------------------------#
# Calculations                                                                                                               #
#----------------------------------------------------------------------------------------------------------------------------#
nh3_line1 <- mean(bls_con_sel[Sensor == "LineSensor1"]$nh3_mgs)
nh3_line1_sd <- sd(bls_con_sel[Sensor == "LineSensor1"]$nh3_mgs)
nh3_line2 <- mean(na.omit(bls_con_sel[Sensor == "LineSensor2"]$nh3_mgs))
nh3_line2_sd <- sd(na.omit(bls_con_sel[Sensor == "LineSensor2"]$nh3_mgs))

ch4_line1 <- mean(bls_con_sel[Sensor == "LineSensor1"]$ch4_mgs)
ch4_line1_sd <- sd(bls_con_sel[Sensor == "LineSensor1"]$ch4_mgs)
ch4_line2 <- mean(na.omit(bls_con_sel[Sensor == "LineSensor2"]$ch4_mgs))
ch4_line2_sd <- sd(na.omit(bls_con_sel[Sensor == "LineSensor2"]$ch4_mgs))

nh3_release <- 14.46
nh3_release_sd <- 0.3
ch4_release <- 118.85
ch4_release_sd <- 2.8

nh3_q_line1 <- (nh3_line1/nh3_release)*100
nh3_q_line1_sd <- sqrt(((nh3_line1_sd/nh3_line1*100)^2)+((nh3_release_sd/nh3_release*100)^2))
nh3_q_line2 <- (nh3_line2/nh3_release)*100
nh3_q_line2_sd <- sqrt(((nh3_line2_sd/nh3_line2*100)^2)+((nh3_release_sd/nh3_release*100)^2))

ch4_q_line1 <- (ch4_line1/ch4_release)*100
ch4_q_line1_sd <- sqrt(((ch4_line1_sd/ch4_line2*100)^2)+((ch4_release_sd/ch4_release*100)^2))
ch4_q_line2 <- (ch4_line2/ch4_release)*100
ch4_q_line2_sd <- sqrt(((ch4_line2_sd/ch4_line2*100)^2)+((ch4_release_sd/ch4_release*100)^2))

nh3_q_line1/100
nh3_q_line1_sd/100
nh3_q_line2/100
nh3_q_line2_sd/100

bls_con_sel$SensorHeight

ch4_q_line1/100
ch4_q_line1_sd/100
ch4_q_line2/100
ch4_q_line2_sd/100


# create dataframe
b <- list("NH3", "IV-DK", nh3_release, nh3_release_sd, nh3_line1, nh3_line1_sd, nh3_line2, nh3_line2_sd, nh3_q_line1, nh3_q_line1_sd, nh3_q_line2, nh3_q_line2_sd) 
c <- list("CH4", "IV-DK", ch4_release, ch4_release_sd, ch4_line1, ch4_line1_sd, ch4_line2, ch4_line2_sd, ch4_q_line1, ch4_q_line1_sd, ch4_q_line2, ch4_q_line2_sd)
df <- as.data.frame(rbind(b,c), stringsAsFactors = FALSE)
names(df) <- c("Gas","Release", "Release_mgs", "Release_sd", "Line1_mgs", "Line1_sd_mgs", "Line2_mgs","Line2_sd_mgs", "Qbls/Q.line1_per", "Qbls/Q.line1.sd", "Qbls/Q.line2_per", "Qbls/Q.line2.sd")
saveRDS(df, file = file.path(Path, "cre2_VIII_selec_results_mean.rds"))
