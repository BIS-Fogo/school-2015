### Author
# Name
# Version 2015-09-23

### Define pathes ##############################################################
setwd("D:/active/bis-fogo/school2015/myStory/Corrine")

### Load libraries #############################################################
library(raster)
library(rgdal)
library(sp)


### Read data ##################################################################
corrine_org <- read.table("FitoAgua.csv", 
                          sep = ",", dec = ".", header = TRUE)
str(corrine_org)


### Create GIS vector shape from data frame ####################################
corrine_shp <- corrine_org

coordinates(corrine_shp) <- ~UTMX+UTMY
projection(corrine_shp) <- 
  CRS("+proj=utm +zone=27 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
str(corrine_shp)
plot(corrine_shp)
