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

for(i in c(seq(20),23)){
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


### Some boxplots to describe variability ######################################
dev.off()
boxplot(mara_t_no_na$Naph[mara_t_no_na$Type == "PlatusGD"], 
       mara_t_no_na$Naph[mara_t_no_na$Type == "SD"],
       notch = TRUE)

# Define polutants to be visualized as part of the box plots
str(mara_t)
polutants <- names(mara_t)[1:20]

# Simple boxplot has the problam of very large outliers on the y-axis
boxplot(mara_t[,polutants], notch = TRUE, las = 2)

# Plot boxplot with specific definition of y-axis limits and vertical x-axis 
# labels
boxplot(mara_t[,polutants], ylim = c(0, 50), notch = TRUE, las = 2)

# Alternative solution instead of limiting the y-values is transforming the 
# values
power <- 0.25
boxplot(mara_t[,polutants]**power, notch = TRUE, las = 2)

# Adapt y-axis values to show original values and not the transformed one
# First, plot the boxplot without a y axis
boxplot(mara_t[,polutants]**power, notch = TRUE, las = 2, yaxt = "n")

# Get maximum value as a power of 10 (by counting digits of the maximum value
# in our dataset) and create a vector of these powers starting with 10**0
ndigits <- nchar(as.character(as.integer(max(mara_t[,polutants], na.rm = TRUE))))
ylabs <- 10**(0:ndigits)

# Transform the power of these values according to the transformation of the
# variables
ytics <- ylabs**power

# Add tics and labels to the boxplot
axis(2, at = ytics, labels = ylabs, las = 2, tck = -0.05, cex.axis = 0.6)

# Adding color to boxplots
clrs <- colors(1)[1:length(polutants)]
boxplot(mara_t[,polutants]**power, notch = TRUE, las = 2, yaxt = "n", 
        col = clrs)
axis(2, at = ytics, labels = ylabs, las = 2, tck = -0.05, cex.axis = 0.6)


### Simple heatmap of the polutants distribution ###############################
polutants <- names(mara_t_no_na)[1:10]
heatmap(mara_t_no_na[, polutants]**power, Rowv = NA, Colv = NA)

rownames(mara_t_no_na) <- 
  paste(seq(nrow(mara_t_no_na)), mara_t_no_na$Location, mara_t_no_na$Type, mara_t_no_na$Year, sep = "_")
heatmap(mara_t_no_na[, polutants]**power, Rowv = NA, Colv = NA)
heatmap(mara_t_no_na[, polutants]**power)
