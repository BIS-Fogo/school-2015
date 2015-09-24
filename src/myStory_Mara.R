### Author
# Name
# Version 2015-09-23

### Define pathes ##############################################################
setwd("D:/active/bis-fogo/school2015/myStory/Mara")

### Load libraries #############################################################
library(corrplot)
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

### Analyse correlations #######################################################
# Analyse correlation between all variables which have no NA values
# Create a subset of the original data frame which has only columns that do not
# contain any NA values, then use this new subset to compute the correlations.
mara_t_no_na <- mara_t[, colSums(is.na(mara_t)) == 0]

mara_t_no_na_cor_matrix <- cor(mara_t_no_na[,1:10])
corrplot(mara_t_no_na_cor_matrix)


# T-test analysis to compare the mean polutant concentration between snails 
# and sediments for a selected pollutant
t.test(mara_t_no_na$Naph[mara_t_no_na$Type == "PlatusGD"], 
    mara_t_no_na$Naph[mara_t_no_na$Type == "SD"])


### Make some scatter plots ####################################################
plot(mara_t_no_na$Phe ~ mara_t_no_na$Pyr)

