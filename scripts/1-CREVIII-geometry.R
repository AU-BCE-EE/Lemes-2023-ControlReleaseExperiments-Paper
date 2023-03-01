#-----------------------------------------------------------------------------------------------------------------------------------------#
# CRE VIII - Geometry                                                                                                             #
#-----------------------------------------------------------------------------------------------------------------------------------------#

#-----------------------------------------------------------------------------------------------------------------------------------------#
# Library and Directory                                                                                                                   #
#-----------------------------------------------------------------------------------------------------------------------------------------#
# library
library(bLSmodelR)
library(lubridate)
library(ibts)

# directory 
Path <- '...'

# read in gps 
source <- read.table(paste0(Path,"/data/gps/source.txt"), header = TRUE, stringsAsFactors = FALSE)
line <- read.table(paste0(Path,"/data/gps/line.txt"), header = TRUE, stringsAsFactors = FALSE)
back <- read.table(paste0(Path,"/data/gps/back.txt"), header = TRUE, stringsAsFactors = FALSE)
sonic <- read.table(paste0(Path,"/data/gps/sonic.txt"), header = TRUE, stringsAsFactors = FALSE)

# source
Sources_22 <- apply(source,1,function(x)list("c",M=x,R=0.05))
names(Sources_22)[1:24] <- " "
Sources_22 <- genSources(Sources_22)

# sensor
line_22 <- genSensors(Line3CRDS=list(x=line$x, y=line$y, z=1.5))

# background
back_22 <- genSensors(Back=list(x=back$x, y=back$y, z=1.5))

# sonic
sonic_22 <- genSensors(USA=list(x=sonic$x, y=sonic$y, z=1.5))

# join sensors
Sensors_22 <- join(sonic_22, line_22, back_22)

# plot
plot(Sources_22, Sensors_22)

# export geometry
save(Sources_22,line_22,back_22,sonic_22,file=file.path(Path,'/rsaves/cre22_geometry.RData'))

# check
load(file.path(Path,'/rsaves/cre22_geometry.RData'))
