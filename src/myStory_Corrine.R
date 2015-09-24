### Author
# Name
# Version 2015-09-23

### Define pathes ##############################################################
setwd("D:/active/bis-fogo/school2015/myStory/Corrine")

### Load libraries #############################################################
library(corrplot)
library(raster)
library(rgdal)
library(sp)


### Read data ##################################################################
corrine_org <- read.table("FitoAgua.csv", 
                          sep = ",", dec = ".", header = TRUE)
str(corrine_org)


### Create GIS vector shape from data frame ####################################
# Not necessary, just to show how it is done
corrine_shp <- corrine_org

coordinates(corrine_shp) <- ~UTMX+UTMY
projection(corrine_shp) <- 
  CRS("+proj=utm +zone=27 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
str(corrine_shp)
plot(corrine_shp)


### Analyse correlations #######################################################
# Analyse correlation between two specific variables
cor(corrine_org$Sal, corrine_org$pH, method = "spearman")

# Analyse correlation between all variables of the data set
cor_matrix <- cor(corrine_org[, c(5, 7, 9, 11)])
corrplot(cor_matrix)


