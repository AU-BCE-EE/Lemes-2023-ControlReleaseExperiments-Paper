#-----------------------------------------------------------------------------------------------------------------------------------------#
# CRE VIII - Deposition                                                                                                          #
#-----------------------------------------------------------------------------------------------------------------------------------------#

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

# read bLS results with deposition
res_dep <- readRDS(file.path(Path, "bls-cre22-results-line1-deposition.rds"))
res2_dep <- readRDS(file.path(Path, "bls-cre22-results-line2-deposition.rds"))

# read results without deposition
res <- readRDS(file.path(Path, "cre21_VIII_selec_results.rds")) 
res2 <- readRDS(file.path(Path, "cre21_VIII_selec_results.rds"))

#----------------------------------------------------------------------------------------------------------------------------#
# Line 1                                                                                                                     #
#----------------------------------------------------------------------------------------------------------------------------#

# join with nh3 anc ch4 measurement
res <- res[Sensor == "LineSensor1"]
res_dt <- data.table(dNH3  = res$nh3_line1_emission_ugm3, 
                     rCH4 = res$ch4_perct/100,
                     #down_nh3.ppb = res$down_nh3.ppb,
                     nh3_rel_mgs = res$nh3_rel_mgs, 
                     nh3_rel_sd_mgs = res$nh3_rel_sd_mgs,
                     #nh3_perct = res$nh3_perct,
                     Release  = res$Release ,
                     Distance_m  = res$Distance_m,
                     airT  = res$airT,
                     airP_hPa = res$airP_hPa,
                     RH  = res$RH,
                     soilT = res$soilT,
                     SR = res$Distance_m,
                     st = res$st) 

# join bls with deposition and results
res_dp <- merge(res_dt, res_dep, by = "st")

# change name variables
res_dp$Rc <- 0
res_dp[ResDep == "vDep10"]$Rc <- 10
res_dp[ResDep == "vDep25"]$Rc <- 25
res_dp[ResDep == "vDep50"]$Rc <- 50
res_dp[ResDep == "vDep75"]$Rc <- 75
res_dp[ResDep == "vDep100"]$Rc <- 100
res_dp[ResDep == "vDep125"]$Rc <- 125
res_dp[ResDep == "vDep150"]$Rc <- 150
res_dp[ResDep == "vDep175"]$Rc <- 175
res_dp[ResDep == "vDep200"]$Rc <- 200
res_dp[ResDep == "vDep300"]$Rc <- 300
res_dp[ResDep == "vDep500"]$Rc <- 500


# calculate recovery fraction without deposition 
res[,":="(
  Rf = nh3_perct/100
)]

# calculate Rfdep
res_dp[,":="(
  Rfdep = (dNH3/1000/CE_Dep)*SourceArea/(as.numeric(nh3_rel_mgs))
)]

# plot reults without deposition
res[,{
  mt <- st + (et - st)/2
  plot(Rf ~ mt,type="n",main=.BY[[1]],xlab=""
       ,ylab=expression(paste(Q["bLS"],"/Q [-]")),ylim=c(0,2))
  abline(h=1,col="darkgrey")
  lines(Rf ~ mt,type="b",lty=3)
},by=Sensor]

# plot reults with deposition
res_dp[, {
  ggplot(.SD, aes(x=Rc,y=Rfdep,group=st, color = st)) +
    geom_point(cex = 3)+
    geom_hline(yintercept = 1, color = "darkgrey", lty =1)+
    xlim(c(500, 0))+
    theme_bw(base_size = 20) +
    ylab(expression(paste("Q"[bLS],"/Q")))+
    xlab(expression(paste("R"[c],"[s m"^-1,"]")))
}]

# recalculate Rb due to error in the code
res_dp[,":="(
  Rb = (1.45 * ((Zo*Ustar/(1.46*10^-5))^0.24) * (((1.46*10^-5)/(Dnh3/10000))^0.8))/Ustar
  # a = (Zo*Ustar/(1.46*10^-5))^0.24,
  # b = ((1.46*10^-5)/(Dnh3/10000))^0.8
)]

# res_dp[,":="(
#   c = (1.45 * a * b)/Ustar
# )]


# estimate best fitting vDep/Rc using loess
# BestRc_0 <- res_dp[,{
#   mod <- loess(Rfdep~Rc)
#   xpred <- seq(0,max(mod$x),length.out=1E5)
#   pmod <- predict(mod,newdata=data.frame(Rc=xpred))
#   ind <- which.min(abs(pmod - 1))
#   .(
#     Rc = xpred[ind]
#     ,vDepf = (1/(xpred[ind] + Rb[1]))*100
#     ,Rb = Rb[1]
#     ,Recov = pmod[ind]
#     ,st = st[1]
#     ,et = et[1]
#   )
# },by=.(Sensor,rn)]

# estimate best fitting vDep/Rc using loess version with CH4
BestRc <- res_dp[,{
  mod <- loess(Rfdep~Rc)
  xpred <- seq(0,max(mod$x),length.out=1E5)
  pmod <- predict(mod,newdata=data.frame(Rc=xpred))
  ind <- which.min(abs(pmod - rCH4[1]))
  .(
    Rc = xpred[ind]
    ,vDepf = (1/(xpred[ind] + Rb[1]))*100
    ,Rb = Rb[1]
    ,Recov = pmod[ind]
    ,st = st[1]
    ,et = et[1]
  )
},by=.(Sensor,rn)]

# check results
BestRc
plot(Rfdep ~ Rc, res_dp[st == "2022-04-22 12:30:00"])
abline(h = 1, col = "darkgrey")
points(Rfdep ~ Rc, res_dp[st == "2022-04-22 14:00:00"], col = "blue")

#----------------------------------------------------------------------------------------------------------------------------#
# Line 2                                                                                                                     #
#----------------------------------------------------------------------------------------------------------------------------#
# join with nh3 anc ch4 measurement
res2 <- res2[Sensor == "LineSensor2"]
res2_dt <- data.table(dNH3  = res2$nh3_line1_emission_ugm3,
                      rCH4 = res$ch4_perct/100,
                     #down_nh3.ppb = res$down_nh3.ppb,
                     nh3_rel_mgs = res2$nh3_rel_mgs, 
                     nh3_rel_sd_mgs = res2$nh3_rel_sd_mgs,
                     #nh3_perct = res$nh3_perct,
                     Release  = res2$Release ,
                     Distance_m  = res2$Distance_m,
                     airT  = res2$airT,
                     airP_hPa = res2$airP_hPa,
                     RH  = res2$RH,
                     soilT = res2$soilT,
                     SR = res2$Distance_m,
                     st = res2$st) 

# join bls with deposition and results
res2_dp <- merge(res2_dt, res2_dep, by = "st")

# change name variables
res2_dp$Rc <- 0
res2_dp[ResDep == "vDep10"]$Rc <- 10
res2_dp[ResDep == "vDep25"]$Rc <- 25
res2_dp[ResDep == "vDep50"]$Rc <- 50
res2_dp[ResDep == "vDep75"]$Rc <- 75
res2_dp[ResDep == "vDep100"]$Rc <- 100
res2_dp[ResDep == "vDep125"]$Rc <- 125
res2_dp[ResDep == "vDep150"]$Rc <- 150
res2_dp[ResDep == "vDep175"]$Rc <- 175
res2_dp[ResDep == "vDep200"]$Rc <- 200
res2_dp[ResDep == "vDep300"]$Rc <- 300
res2_dp[ResDep == "vDep500"]$Rc <- 500


# calculate recovery fraction without deposition 
res2[,":="(
  Rf = nh3_perct/100
)]

# calculate Rfdep
res2_dp[,":="(
  Rfdep = (dNH3/1000/CE_Dep)*SourceArea/(as.numeric(nh3_rel_mgs))
)]

# recalculate Rb due to error in the code
res2_dp[,":="(
  Rb = (1.45 * ((Zo*Ustar/(1.46*10^-5))^0.24) * (((1.46*10^-5)/(Dnh3/10000))^0.8))/Ustar
)]

# plot reults without deposition
res2[,{
  mt <- st + (et - st)/2
  plot(Rf ~ mt,type="n",main=.BY[[1]],xlab=""
       ,ylab=expression(paste(Q["bLS"],"/Q [-]")),ylim=c(0,2))
  abline(h=1,col="darkgrey")
  lines(Rf ~ mt,type="b",lty=3)
},by=Sensor]

# plot reults with deposition
res2_dp[, {
  ggplot(.SD, aes(x=Rc,y=Rfdep,group=st, color = st)) +
    geom_point(cex = 3)+
    geom_hline(yintercept = 1, color = "darkgrey", lty =1)+
    xlim(c(500, 0))+
    theme_bw(base_size = 20) +
    ylab(expression(paste("Q"[bLS],"/Q")))+
    xlab(expression(paste("R"[c],"[s m"^-1,"]")))
}]

# estimate best fitting vDep/Rc using loess
# BestRc2_v0 <- res2_dp[,{
#   mod <- loess(Rfdep~Rc)
#   xpred <- seq(0,max(mod$x),length.out=1E5)
#   pmod <- predict(mod,newdata=data.frame(Rc=xpred))
#   ind <- which.min(abs(pmod - 1))
#   .(
#     Rc = xpred[ind]
#     ,vDepf = (1/(xpred[ind] + Rb[1]))*100
#     ,Rb = Rb[1]
#     ,Recov = pmod[ind]
#     ,st = st[1]
#     ,et = et[1]
#   )
# },by=.(Sensor,rn)]


# estimate best fitting vDep/Rc using loess VERSION WITH CH4
BestRc2 <- res2_dp[,{
  mod <- loess(Rfdep~Rc)
  xpred <- seq(0,max(mod$x),length.out=1E5)
  pmod <- predict(mod,newdata=data.frame(Rc=xpred))
  ind <- which.min(abs(pmod - rCH4[1])) 
  .(
    Rc = xpred[ind]
    ,vDepf = (1/(xpred[ind] + Rb[1]))*100
    ,Rb = Rb[1]
    ,Recov = pmod[ind]
    ,st = st[1]
    ,et = et[1]
  )
},by=.(Sensor,rn)]

# check results
BestRc2
plot(Rfdep ~ Rc, res2_dp[st == "2022-04-22 12:30:00"])
abline(h = 1, col = "darkgrey")
points(Rfdep ~ Rc, res2_dp[st == "2022-04-22 13:00:00"], col = "red")

#----------------------------------------------------------------------------------------------------------------------------#
# Join lines                                                                                                                 #
#----------------------------------------------------------------------------------------------------------------------------#

fRC <- rbind(BestRc, BestRc2)

# export results
saveRDS(fRC, file = file.path(Path, "cre22_vdep_qch4.rds"))
