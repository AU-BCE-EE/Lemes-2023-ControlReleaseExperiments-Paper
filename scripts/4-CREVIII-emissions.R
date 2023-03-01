#----------------------------------------------------------------------------------------------------------------------------#
#      CRE 22- Emission, YL May 22                                                                           #
#----------------------------------------------------------------------------------------------------------------------------#
# Comments
# --> Qbls was calculated with average selected by nraw (see section "Calculations")

#----------------------------------------------------------------------------------------------------------------------------#
#Library, directory & data                                                                                                   #
#----------------------------------------------------------------------------------------------------------------------------#

# library
library(ibts)
# library(tidyverse)
library(data.table)
library(ggplot2)

# paths
Path <- '...'

# read in crdss
crds <- readRDS(file.path(Path, "/crds_cest.rds"))


# read in bls
bls_1 <- readRDS(file.path(Path, "/bls-cre22-results-line1_line2_170_point_160.rds")) 

# add sensor name
crds_point <- data.table(nh3_point_emission_ugm3 = crds$nh3_point_emission_ugm3, 
                         ch4_point_emission_ugm3 = crds$ch4_point_emission_ugm3,
                         Sensor = "PointSensor", st = crds$Time)

crds_line1 <- data.table(nh3_line1_emission_ugm3 = crds$nh3_line1_emission_ugm3, 
                         ch4_line1_emission_ugm3 = crds$ch4_line1_emission_ugm3,
                         Sensor = "LineSensor1", st = crds$Time)

crds_line2 <- data.table(nh3_line2_emission_ugm3 = crds$nh3_line2_emission_ugm3, 
                         ch4_line2_emission_ugm3 = crds$ch4_line2_emission_ugm3,
                         Sensor = "LineSensor2", st = crds$Time)

#----------------------------------------------------------------------------------------------------------------------------#
# Calculate QbLS                                                                                            #
#----------------------------------------------------------------------------------------------------------------------------#

# join bLS and concentrations
bls2_crds_point <- merge(
  bls_2,crds_point,
  by = c("Sensor", "st")
)

bls2_crds_line1 <- merge(
  bls_2,crds_line1,
  by = c("Sensor", "st")
)

bls2_crds_line2 <- merge(
  bls_2,crds_line2,
  by = c("Sensor", "st")
)
# calculate emissions
bls2_crds_point$nh3_point_emission_ugm2s <- bls2_crds_point$nh3_point_emission_ugm3 / bls2_crds_point$CE * bls2_crds_point$SourceArea
bls2_crds_point$ch4_point_emission_ugm2s <- bls2_crds_point$ch4_point_emission_ugm3 / bls2_crds_point$CE * bls2_crds_point$SourceArea

bls2_crds_line1$nh3_line1_emission_ugm2s <- bls2_crds_line1$nh3_line1_emission_ugm3 / bls2_crds_line1$CE * bls2_crds_line1$SourceArea
bls2_crds_line1$ch4_line1_emission_ugm2s <- bls2_crds_line1$ch4_line1_emission_ugm3 / bls2_crds_line1$CE * bls2_crds_line1$SourceArea

bls2_crds_line2$nh3_line2_emission_ugm2s <- bls2_crds_line2$nh3_line2_emission_ugm3 / bls2_crds_line2$CE * bls2_crds_line2$SourceArea
bls2_crds_line2$ch4_line2_emission_ugm2s <- bls2_crds_line2$ch4_line2_emission_ugm3 / bls2_crds_line2$CE * bls2_crds_line2$SourceArea

