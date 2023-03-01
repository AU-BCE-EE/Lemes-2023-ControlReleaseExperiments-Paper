#-----------------------------------------------------------------------------------------------------------------------------------------#
# CRE VIII - bLS                                                                                                                          #
#-----------------------------------------------------------------------------------------------------------------------------------------#

#-----------------------------------------------------------------------------------------------------------------------------------------#
# Library and Directory                                                                                                                   #
#-----------------------------------------------------------------------------------------------------------------------------------------#
# library
library(bLSmodelR)
library(lubridate)
library(ibts)


# directory paths for computer
Path <- '...'
# directory paths for PRIME
Path <- "/home/ymlp/CRE22/data"
data_dirs <- list.dirs(Path, recursive = FALSE)
PathGPS <- file.path(Path, 'gps')
Cat.Path <- paste0(Path,"/catalogs")


#-----------------------------------------------------------------------------------------------------------------------------------------#
#  bLS parameters                                                                                                                         #
#-----------------------------------------------------------------------------------------------------------------------------------------#

# select of number of trajectories
N_traj <- 5E5
MaxFetch <- -50

# define slurm parameters
slurm_options <- list(
  mem = "100G", 
  "mail-user" = "ymlp@bce.au.dk", 
  "mail-type" = "ALL", 
  time = "3:30:00"
)

#-----------------------------------------------------------------------------------------------------------------------------------------#
#  Source and Sensor geometry                                                                                                             #
#-----------------------------------------------------------------------------------------------------------------------------------------#

# read in gps 
source <- read.table(paste0(PathGPS,"/source.txt"), header = TRUE, stringsAsFactors = FALSE)
#point <- read.table(paste0(PathGPS,"/point.txt"), header = TRUE, stringsAsFactors = FALSE)
line1 <- read.table(paste0(PathGPS,"/line.txt"), header = TRUE, stringsAsFactors = FALSE)
line2 <- read.table(paste0(PathGPS,"/line.txt"), header = TRUE, stringsAsFactors = FALSE)

# source
Sources <- apply(source,1,function(x)list("c",M=x,R=0.05))
names(Sources)[1:24] <- "Artif. Source"
Sources <- genSources(Sources)

# sensors
line1$z <- 1.63 # Provide line height
line2$z <- 1.87
#point$z <- 1.65

line_sensors <- genSensors(LineSensor1 = line1,
                           LineSensor2 = line2)
#point_sensor <- genSensors(PointSensor = point)

# join senors
#Sensors <- join(line_sensors, point_sensor)
Sensors <- line_sensors

# plot
plot(Sensors, Sources)
#-----------------------------------------------------------------------------------------------------------------------------------------#
#  Sonic                                                                                                                         #
#-----------------------------------------------------------------------------------------------------------------------------------------#
#Read sonic data

sonic <- na.omit(fread(paste0(Path,"/CRE22_TT_-6correct_EC.txt")))

#sonic[,Time:= as.POSIXct(Time, tz = "Etc/GMT-1", "%d-%b-%Y %H:%M:%S")]
sonic[,Time:= as.POSIXct(Time, tz = "CET", "%d-%b-%Y %H:%M:%S")]

sonic_ibts <- as.ibts(sonic, st= "Time", granularity="15mins")

Interval <- genInterval(
  cbind(
    setnames(as.data.frame(sonic)[,c("UST","L","z0","WD","d","sigU","sigV","sigW","z_sonic")],
             c("Ustar","L","Zo","WD","d","sUu","sVu","sWu","z_sWu")),
    st=st(sonic_ibts),
    et=et(sonic_ibts),
    Sonic = "Sonic",
    as.data.frame(sonic)[,!(names(sonic) %in% c("Ustar","L","z0","WD","sd_WD","d","sUu","sVu","sWu","z_sonic"))]
  ),
  MaxFetch = MaxFetch,
  N0 = N_traj
)

Interval <- Interval[Interval$"Zo [m]" < 0.25 & !is.na(Interval$"Zo [m]"), ]


#-----------------------------------------------------------------------------------------------------------------------------------------#
#  Run model                                                                                                                              #
#-----------------------------------------------------------------------------------------------------------------------------------------#

# build input list
InList <- genInputList(Interval, Sources, Sensors)

# run model
res <- runbLSlurm(InList, Cat.Path, slurm_options)
res <- runbLS(InList, Cat.Path, ncores = "64")

# export data
saveRDS(res, file.path(Path, "bls-cre22-results-6corr_line1_163_line2_187_cest.rds"))

#-----------------------------------------------------------------------------------------------------------------------------------------#
#  Plot Footprint                                                                                                                         #
#-----------------------------------------------------------------------------------------------------------------------------------------#

# load data results
res <- readRDS(file.path(Path, "bls-cre22-results-14corr_line1_163_line2_183_cest.rds"))

## Plot footfrint with scale, North symbol and export it
# add map scale & north symbol
bx <- mean(Sources[,2])-90 # starting point of the lower or upper left scale bar corner 535370.7
by <- mean(Sources[,3])-110 # dito
dx1 <- 25 # scale range step 1
dx2 <- 50 # scale range setp 2
dy <- 4 # half height of the scale bar (and used for other things)
wr.x <- bx+190 # starting point of the direction north N symbol
wr.y <- by+155 # dito

# footprint
#png(file.path(Path,'Figures/footprint_bls-CRE22_line1.png'),width=15,height=15,units = 'in',res = 600)
pdf(file.path(Path,'Figures/footprint_bls-CRE22_line2.pdf'),width=8,height=8)
par(cex=1.8, mar=c(0,0,0,0)+0.2)
plot(type='n',1,xlim=c(535075,535315),ylim=c(6261025,6261225),xaxt='n',yaxt='n',xlab='',ylab='',asp=1)
plotFootprint(res, SensorName = "LineSensor2", type = 'CE',breaks = function(x) quantile(c(0, max(x)), c(0.01,0.1,0.3, 0.5, 0.9)),
              leg.bg.col = 'white', showLegend = TRUE, main = NA,
              origin = colMeans(Sources[,2:3],na.rm=TRUE),add=TRUE)
plot(Sources,cex=3,add=TRUE, sources.text.args=list(labels='Source'))
plot(Sensors,cex=3,add=TRUE, sources.text.args=list(labels=''))

# Massstab

polygon(c(bx,bx+dx1,bx+dx1,bx),c(by,by,by-dy,by-dy),col="black")
polygon(c(bx+dx1,bx+dx2,bx+dx2,bx+dx1),c(by,by,by-dy,by-dy),col="white",border="black")
lines(c(bx,bx),c(by,by+dy*(2/3)),col="black")
lines(c(bx+dx1,bx+dx1),c(by,by+dy*(1/3)),col="black")
lines(c(bx+dx2,bx+dx2),c(by,by+dy*(2/3)),col="black")
text(bx,by+dy*3,"0 m",cex=1,adj=c(0.5,0.4),col="black")
text(bx+dx1,by+dy*3,"25 m",cex=1,adj=c(0.5,0.4),col="black")
text(bx+dx2,by+dy*3,"50 m",cex=1,adj=c(0.5,0.4),col="black")

## Richtung Nord

polygon(c(wr.x,wr.x-5,wr.x),c(wr.y,wr.y-5,wr.y+30),border=1)
polygon(c(wr.x,wr.x+5,wr.x),c(wr.y,wr.y-5,wr.y+30),border=1,col=1)
text(x=wr.x,y=wr.y+40,labels="N",cex=1)
dev.off()

#-----------------------------------------------------------------------------------------------------------------------------------------#
#  Calculate Contour                                                                                                                       #
#-----------------------------------------------------------------------------------------------------------------------------------------#

#Source contour script
source('/home/ymlp/scripts/contourXY.r')

# resolution
dx <- 2
dy <- 2

# origin
origin <- colMeans(Sources[,2:3])

# run function
png(file = tempfile(fileext = '.png')) # save temp. png file so that it doesn't open a plot window
XY_cre_22 <- contourXY(Sources, z.meas = 1.87,origin=origin, #without map. either define an origin (then xlim and ylim are == 100, or define xlim and ylim)
                       Interval = Interval, Cat.Path = Cat.Path, dx = dx, dy = dy,
                       fill = TRUE,slurm_options)
dev.off() # close png
saveRDS(XY_cre_22,paste0(Path,"/XY_cre_22.rds"))
#

sopt <- list(time = "10:00:00", nodes = 2, mem = 90000, partition = "q64", "mail-user" = "ymlp@bce.au.dk", "mail-type" = "ALL")

png(file = tempfile(fileext = '.png')) # save temp. png file so that it doesn't open a plot window
XY_cre_22 <- contourXY(Sources, z.meas = 1.87,origin=origin, #without map. either define an origin (then xlim and ylim are == 100, or define xlim and ylim)
                       Interval = Interval, Cat.Path = Cat.Path, dx = dx, dy = dy,
                       fill = TRUE,sopt)
dev.off() # close png
saveRDS(XY_cre_22,paste0(Path,"/XY_cre_22.rds"))

#-----------------------------------------------------------------------------------------------------------------------------------------#
#  Ammonia deposition                                                                                                                     #
#-----------------------------------------------------------------------------------------------------------------------------------------#

# rebuild catalogs
rebuildCatListFile(Cat.Path, fromScratch = TRUE)

# load results 
res <- readRDS(file.path(Path, "bls-cre22-results-14corr_line1_163_line2_187_cest.rds"))

# calculate nh3 diffusion coefficient, D. D = Do(po/p)(T/To)^3/2 [cm2 /s]
# res_dt[, ":="(
#   Dnh3 = 0.2036981
# )]

res[, ":="(
  Dnh3 = 0.20487 *(101325/(Pair)) * (273/ (273+airT))^(3/2)
)]

# calculate pseudo-laminar boundary resistance, Rb
res[, ":="(
  Rb = (1.45 * ((Zo*Ustar/1.46*10^-5)^0.24) * ((1.46*10^-5/(Dnh3/10000))^0.8))/Ustar
)]

# bulk canopy resistance, Rc
Rc <- c(0, 10, 25, 50, 75, 100, 125, 150, 175, 200, 300, 500)

# calculate deposition velocity with different Rc
res[,paste0("vDep",Rc) := as.data.table(1/outer(get("Rb"),Rc,"+"))]

# run dry deposition for all Rc values
ResDep <- vector("list",length(Rc))
names(ResDep) <- paste0("vDep",Rc)

# run depositions
for(i in names(ResDep)){
  out <- depoSlurm(res,vDep = i, slurm_options, Sensor = "LineSensor2")
  out[,ResDep := i]
  ResDep[[i]] <- out
}

# join results
ResDep_out <- rbindlist(ResDep)
saveRDS(ResDep_out, file.path(Path, "bls-cre22-results-line1-deposition.rds"))
saveRDS(ResDep_out, file.path(Path, "bls-cre22-results-line2-deposition.rds"))
#rm(res)


