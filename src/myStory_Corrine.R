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


dev.off()
boxplot(corrine_org[, "S"] ~ corrine_org$Year, notch = TRUE,
        ylab = "Species richness")
boxplot(corrine_org[, "N"] ~ corrine_org$Year, notch = TRUE)

power <- 0.125
# plot the boxplot without y axis tics and lables
boxplot(corrine_org[, "N"]**power ~ corrine_org$Year, notch = TRUE, yaxt = "n")

# get maximum value as power of 10 (by counting digits of the max value)
# and create a vector of these powers starting with 10^0
ndigits <- nchar(as.character(as.integer(max(corrine_org[, "N"]))))
ylabls <- 10^(0:ndigits)
print(ylabls)

# transform the power of then values above analogous to the data values
ytics <- ylabls**power

# add axis tics and labels to the plot
axis(2, at=ytics, labels=ylabls, las=2, tck=-.01, cex.axis=0.6)



