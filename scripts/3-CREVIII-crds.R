#----------------------------------------------------------------------------------------------------------------------------#
#      CRE VIII- CRDs, YL May 22                                                                                                #
#----------------------------------------------------------------------------------------------------------------------------#

#----------------------------------------------------------------------------------------------------------------------------#
#Library, directory & data                                                                                                   #
#----------------------------------------------------------------------------------------------------------------------------#
#library
library(ibts)
library(data.table)
library(ggplot2)


# paths
Path <- '...'
PathRSaves <- file.path(Path, "rsaves")
Pathback <- file.path(Path, "data/crds_back") #background
Filesback <- dir(Pathback, full.names = TRUE, recursive = TRUE)
Pathpoint <- file.path(Path, "data/crds_point") #downwind point
Filespoint <- dir(Pathpoint, full.names = TRUE, recursive = TRUE)

Pathline1 <- file.path(Path, "data/crds_old_line") #old line
Filesline1 <- dir(Pathline1, full.names = TRUE, recursive = TRUE)
Pathline2 <- file.path(Path, "data/crds_new_line") #old line
Filesline2 <- dir(Pathline2, full.names = TRUE, recursive = TRUE)

# read tair and pair 
wea_dt <- na.omit(fread(paste0(PathRSaves,"/CRE22_TT_14correct_EC.txt")))
wea_dt[,st := as.POSIXct(Time, tz = "CET", "%d-%b-%Y %H:%M:%S")] 
wea_pt <- wea_dt[,c("st", "airT", "Pair")]


#----------------------------------------------------------------------------------------------------------------------------#
#CRDS                                                                                                                        #
#----------------------------------------------------------------------------------------------------------------------------#
#time intervals
exp_start <- ibts::parse_date_time3('22.04.2022 12:00', tz = "CET")
exp_stop <- ibts::parse_date_time3('22.04.2022 15:30', tz = "CET")

# read in data from crds 
back_dt <- rbindlist(
  lapply(Filesback, function(f) {
    fread(f)
  })
)

point_dt <- rbindlist(
  lapply(Filespoint, function(f) {
    fread(f)
  })
)

line1_dt <- rbindlist(
  lapply(Filesline1, function(f) {
    fread(f)
  })
)

line2_dt <- rbindlist(
  lapply(Filesline2, function(f) {
    fread(f)
  })
)

# add Time column + calibration time from crds
back_dt[, st := fast_strptime(paste(DATE, TIME), "%Y-%m-%d %H:%M:%OS", lt = FALSE, tz = "CET") + (60 * 60 - (3*60 + 53))] 
back_dt[, et := c(st[-1], st[.N] + 2)]

point_dt[, st := fast_strptime(paste(DATE, TIME), "%Y-%m-%d %H:%M:%OS", lt = FALSE, tz = "CET") + 60 * 60]
point_dt[, et := c(st[-1], st[.N] + 2)]

line1_dt[, st := fast_strptime(paste(DATE, TIME), "%Y-%m-%d %H:%M:%OS", lt = FALSE, tz = "CET") + 2 * 60 * 60]
line1_dt[, et := c(st[-1], st[.N] + 2)]

line2_dt[, st := fast_strptime(paste(DATE, TIME), "%Y-%m-%d %H:%M:%OS", lt = FALSE, tz = "CET") + (2 * 60 * 60 - (2*60 +46))]
line2_dt[, et := c(st[-1], st[.N] + 2)]


# convert objs to ibts
back <- as.ibts(back_dt[, .(st, et, CH4_dry, NH3)])
point <- as.ibts(point_dt[, .(st, et, CH4_dry, NH3)])
line1 <- as.ibts(line1_dt[, .(st, et, CH4_dry, NH3)])
line2 <- as.ibts(line2_dt[, .(st, et, CH4_dry, NH3)])

back_30min <- pool(back, granularity = "15mins", st.to = exp_start, et.to = exp_stop)
point_30min <- pool(point, granularity = "15mins", st.to = exp_start, et.to = exp_stop)
line1_30min <- pool(line1, granularity = "15mins", st.to = exp_start, et.to = exp_stop)
line2_30min <- pool(line2, granularity = "15mins", st.to = exp_start, et.to = exp_stop)

back_30min_pt <- merge(wea_pt, cbind(back_30min,st=st(back_30min)),by="st",all=TRUE)
point_30min_pt <- merge(wea_pt, cbind(point_30min,st=st(point_30min)),by="st",all=TRUE)
line1_30min_pt <- merge(wea_pt, cbind(line1_30min,st=st(line1_30min)),by="st",all=TRUE)
line2_30min_pt <- merge(wea_pt, cbind(line2_30min,st=st(line2_30min)),by="st",all=TRUE)

# 1min average to see why line 1 is jumping

back_1min <- pool(back, granularity = "1mins", st.to = exp_start, et.to = exp_stop)
point_1min <- pool(point, granularity = "1mins", st.to = exp_start, et.to = exp_stop)
line1_1min <- pool(line1, granularity = "1mins", st.to = exp_start, et.to = exp_stop)
line2_1min <- pool(line2, granularity = "1mins", st.to = exp_start, et.to = exp_stop)

# convert ppb -> ug/m3 and ppm -> ug/m3
R <- 8.31446 #m3Pa/K mol
mwnh3 <- 17.031 #g/mol
mwch4 <- 16.043 #g/mol

back_30min_pt$nh3_back_ugm3 <- back_30min_pt$NH3 / 1000 * mwnh3 / ((back_30min_pt$airT + 273.15) / (back_30min_pt$Pair) * R)
back_30min_pt$ch4_back_ugm3 <- back_30min_pt$CH4_dry * mwch4 / ((back_30min_pt$airT + 273.15) / (back_30min_pt$Pair) * R)

point_30min_pt$nh3_point_ugm3 <- point_30min_pt$NH3 / 1000 * mwnh3 / ((point_30min_pt$airT + 273.15) / (point_30min_pt$Pair) * R)
point_30min_pt$ch4_point_ugm3 <- point_30min_pt$CH4_dry * mwch4 / ((point_30min_pt$airT + 273.15) / (point_30min_pt$Pair) * R)

line1_30min_pt$nh3_line1_ugm3 <- line1_30min_pt$NH3 / 1000 * mwnh3 / ((line1_30min_pt$airT + 273.15) / (line1_30min_pt$Pair) * R)
line1_30min_pt$ch4_line1_ugm3 <- line1_30min_pt$CH4_dry  * mwch4 / ((line1_30min_pt$airT + 273.15) / (line1_30min_pt$Pair) * R)

line2_30min_pt$nh3_line2_ugm3 <- line2_30min_pt$NH3 / 1000 * mwnh3 / ((line2_30min_pt$airT + 273.15) / (line2_30min_pt$Pair) * R)
line2_30min_pt$ch4_line2_ugm3 <- line2_30min_pt$CH4_dry  * mwch4 / ((line2_30min_pt$airT + 273.15) / (line2_30min_pt$Pair) * R)

# change columns name
colnames(back_30min_pt) <- c("Time", "airT", "Pair", "CH4_dry_back_ppm", "NH3_back_ppb", "nh3_back_ugm3", "ch4_back_ugm3")
colnames(point_30min_pt) <- c("Time", "airT", "Pair", "CH4_dry_point_ppm", "NH3_point_ppb", "nh3_point_ugm3", "ch4_point_ugm3")
colnames(line1_30min_pt) <- c("Time", "airT", "Pair", "CH4_dry_line1_ppm", "NH3_line1_ppb", "nh3_line1_ugm3", "ch4_line1_ugm3")
colnames(line2_30min_pt) <- c("Time", "airT", "Pair", "CH4_dry_line2_ppm", "NH3_line2_ppb", "nh3_line2_ugm3", "ch4_line2_ugm3")

# join data in 2 steps to avoid to use list
crds_1_join <- merge(back_30min_pt[, c('Time', 'CH4_dry_back_ppm', 'NH3_back_ppb', 'nh3_back_ugm3', 'ch4_back_ugm3')], 
                   point_30min_pt[,c('Time', 'CH4_dry_point_ppm', 'NH3_point_ppb', 'nh3_point_ugm3', 'ch4_point_ugm3')],
                   by = "Time", all = TRUE)

crds_2_join <- merge(crds_1_join[, c('Time', 'CH4_dry_back_ppm', 'NH3_back_ppb', 'nh3_back_ugm3', 'ch4_back_ugm3', 'CH4_dry_point_ppm', 
                                     'NH3_point_ppb', 'nh3_point_ugm3', 'ch4_point_ugm3')],
                     line1_30min_pt[,c('Time', 'CH4_dry_line1_ppm', 'NH3_line1_ppb', 'nh3_line1_ugm3', 'ch4_line1_ugm3')],
                     #line2_30min_pt[,c('Time', 'CH4_dry_line2_ppm', 'NH3_line2_ppb', 'nh3_line2_ugm3', 'ch4_line2_ugm3')],
                     by = "Time", all = TRUE)

crds_join <- merge(crds_2_join[, c('Time', 'CH4_dry_back_ppm', 'NH3_back_ppb', 'nh3_back_ugm3', 'ch4_back_ugm3', 'CH4_dry_point_ppm', 
                                     'NH3_point_ppb', 'nh3_point_ugm3', 'ch4_point_ugm3', 'CH4_dry_line1_ppm', 'NH3_line1_ppb', 
                                     'nh3_line1_ugm3', 'ch4_line1_ugm3')],
                     line2_30min_pt[,c('Time', 'CH4_dry_line2_ppm', 'NH3_line2_ppb', 'nh3_line2_ugm3', 'ch4_line2_ugm3')],
                     by = "Time", all = TRUE)

# emissions in ppb or ppm
crds_join$nh3_point_emission_ppb <- crds_join$NH3_point_ppb - crds_join$NH3_back_ppb
crds_join$ch4_point_emission_ppm <- crds_join$CH4_dry_point_ppm - crds_join$CH4_dry_back_ppm

crds_join$nh3_line1_emission_ppb <- crds_join$NH3_line1_ppb - crds_join$NH3_back_ppb
crds_join$ch4_line1_emission_ppm <- crds_join$CH4_dry_line1_ppm - crds_join$CH4_dry_back_ppm

crds_join$nh3_line2_emission_ppb <- crds_join$NH3_line2_ppb - crds_join$NH3_back_ppb
crds_join$ch4_line2_emission_ppm <- crds_join$CH4_dry_line2_ppm - crds_join$CH4_dry_back_ppm

# emission in ugm3 or ugm3
crds_join$nh3_point_emission_ugm3 <- crds_join$nh3_point_ugm3 - crds_join$nh3_back_ugm3
crds_join$ch4_point_emission_ugm3 <- crds_join$ch4_point_ugm3 - crds_join$ch4_back_ugm3

crds_join$nh3_line1_emission_ugm3 <- crds_join$nh3_line1_ugm3 - crds_join$nh3_back_ugm3
crds_join$ch4_line1_emission_ugm3 <- crds_join$ch4_line1_ugm3 - crds_join$ch4_back_ugm3

crds_join$nh3_line2_emission_ugm3 <- crds_join$nh3_line2_ugm3 - crds_join$nh3_back_ugm3
crds_join$ch4_line2_emission_ugm3 <- crds_join$ch4_line2_ugm3 - crds_join$ch4_back_ugm3

# join selected variables
crds_join_selec <- crds_join[, c('Time', 'nh3_point_emission_ppb', 'ch4_point_emission_ppm', 'nh3_line1_emission_ppb',
                             'ch4_line1_emission_ppm', 'nh3_line2_emission_ppb', 'ch4_line2_emission_ppm',
                             'nh3_point_emission_ugm3', 'ch4_point_emission_ugm3', 'nh3_line1_emission_ugm3',
                             'ch4_line1_emission_ugm3', 'nh3_line2_emission_ugm3', 'ch4_line2_emission_ugm3')]


#-----------------------------------------------------------------------------------------------------------------------------------------#
# Plot and export                                                                                                                         #
#-----------------------------------------------------------------------------------------------------------------------------------------#

# plot 
plot(crds_join_selec$nh3_point_emission_ugm3, col = 'black', ylim = c(0, 140)) 
lines(crds_join_selec$nh3_line1_emission_ugm3, col = 'blue')
lines(crds_join_selec$nh3_line2_emission_ugm3, col = 'red')

plot(crds_join_selec$ch4_point_emission_ugm3, col = 'black', ylim = c(10, 1000)) 
lines(crds_join_selec$ch4_line1_emission_ugm3, col = 'blue')
lines(crds_join_selec$ch4_line2_emission_ugm3, col = 'red')

# export
saveRDS(crds_join_selec, file.path(PathRSaves, "crds_cest.rds"))


# plot 1min concentration
## nh3
plot(back_1min$NH3, col = 'black', ylim = c(0, 80))
#plot(back_1min$NH3, col = 'black', ylim = c(0, 160))
#lines(point_1min$NH3, col = 'lightblue')
lines(line1_1min$NH3, col = 'darkblue')
lines(line2_1min$NH3, col = 'orange')
     
## ch4
plot(back_1min$CH4_dry, col = 'black', ylim = c(1.8, 3))
#plot(back_1min$NH3, col = 'black', ylim = c(0, 160))
#lines(point_1min$CH4_dry, col = 'lightblue')
lines(line1_1min$CH4_dry, col = 'darkblue')
lines(line2_1min$CH4_dry, col = 'darkorange')
