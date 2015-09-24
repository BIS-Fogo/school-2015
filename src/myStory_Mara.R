### Author
# Name
# Version 2015-09-23

### Define pathes ##############################################################
setwd("D:/active/bis-fogo/school2015/myStory/Mara")

### Load libraries #############################################################
library(rgdal)
library(sp)


### Read data ##################################################################
mara_org <- read.table("Data.Mara.csv", sep = "\t", dec = ".", header = FALSE)
str(mara_org)


### Transforme data from "wide" to "long" format ###############################
# Transform data and make sure that each column has only one data type.
mara_t <- data.frame(t(mara_org))
colnames(mara_t) <- mara_org[,1]
mara_t <- mara_t[-1,]
str(mara_t)

for(i in seq(2:20)){
  mara_t[,i] <- as.numeric(as.character(mara_t[,i]))
}

str(mara_t)

