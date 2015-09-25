### Author
# Name
# Version 2015-09-23

### Define pathes ##############################################################
setwd("D:/active/bis-fogo/school2015/myStory/Nevsky")

### Load libraries #############################################################
library(corrplot)
library(rgdal)
library(MuMIn)
library(sp)


### Read data ##################################################################
# Read meta-data
nevsky_org_meta <- read.table("IE_2007_pontos_esp_meta.csv", 
                              sep = "\t", dec = ".", header = TRUE,
                              stringsAsFactors = FALSE)
str(nevsky_org_meta)

# Read GIS/field survey data
nevsky_org_shp <- readOGR("IE_2007_pontos_esp.shp", 
                          layer = "IE_2007_pontos_esp")
str(nevsky_org_shp)

# Correct entries for CYD_OBL to be numeric
nevsky_org_df <- data.frame(nevsky_org_shp)
str(nevsky_org_df)
nevsky_org_df$CYD_OBL <- as.numeric(as.character(nevsky_org_df$CYD_OBL))

# Change all values in the species columns to 1 if the value of the respecitve
# cell (i.e. observation plot) is at least 1 the cells result in a binary 
# coding with 0 = no occurence on the plot and 1 = occurence on the plot
nevsky_org_df[, 14:89][nevsky_org_df[, 14:89] > 1] <- 1

# endemic <- c("DRA_DRA", "LOT_JAC", "ZIN_PER")
# which(colnames(nevsky_org_df) %in% endemic)
# 
# nevsky_org_df[, c(37, 60, 89)]
# 
# rowSums(nevsky_org_df[, which(colnames(nevsky_org_df) %in% endemic)])


### Aggregate data regarding endemics, natives, introduced #####################
species_introduced <- nevsky_org_meta[nevsky_org_meta$Endemic == 0, "Species"]
species_native <- nevsky_org_meta[nevsky_org_meta$Endemic == 1, "Species"]
species_endemic <- nevsky_org_meta[nevsky_org_meta$Endemic == 2, "Species"]
species_all <- nevsky_org_meta[, "Species"]

nevsky_org_df$SPE_END <- 
  rowSums(nevsky_org_df[, which(colnames(nevsky_org_df) %in% species_endemic)])

nevsky_org_df$SPE_NAT <- 
  rowSums(nevsky_org_df[, which(colnames(nevsky_org_df) %in% species_native)])

nevsky_org_df$SPE_INT <- 
  rowSums(nevsky_org_df[, which(colnames(nevsky_org_df) %in% species_introduced)])

nevsky_org_df$SPE_ALL <- 
  rowSums(nevsky_org_df[, which(colnames(nevsky_org_df) %in% species_all)])

# Helper variable defining independent and dependent variables (i.e. column names)
var_independent <- names(nevsky_org_df)[c(3:10, 12:13)]
var_dependent <- names(nevsky_org_df)[92:95]
var_species <- names(nevsky_org_df)[14:89]


### Analyse correlations #######################################################
# Analyse correlation between all variables of the data set
nevsky_org_df_cor <- cor(nevsky_org_df[, c(var_independent, var_dependent)])
corrplot(nevsky_org_df_cor)


### Make some scatter plots ####################################################
dev.off()
plot(nevsky_org_df$ALT_GPS_M ~ nevsky_org_df$SPE_ALL)

# There are some errors (elevation equal to 0)
# Let's fill it using alternative elevation information (ALT_ALT_M)
# To preserve all original Alt_GPS_M values, the filled elevation data
# is stored in a new column (ALT)
nevsky_org_df$ALT <- nevsky_org_df$ALT_GPS_M
nevsky_org_df$ALT[nevsky_org_df$ALT == 0] <- 
  nevsky_org_df$ALT_ALT_M[nevsky_org_df$ALT == 0]

# Check plot if all altitudes are larger than 0.
plot(nevsky_org_df$ALT ~ nevsky_org_df$SPE_ALL)


### Simple linear regression analysis ##########################################
# linear_model <- lm(nevsky_org_df$SPE_ALL ~ nevsky_org_df$ALT)
linear_model <- lm(SPE_ALL ~ ALT, data = nevsky_org_df)
summary(linear_model)
# plot(linear_model)

# Compute confidence intervals of the linear model
linear_model_confidence <- predict(linear_model, interval="confidence") 

# Visualize the linear model including confidence intervals
plot(nevsky_org_df$SPE_ALL ~ nevsky_org_df$ALT)
abline(linear_model, col = "red")
lines(linear_model_confidence[, 2] ~ nevsky_org_df$ALT, col = "blue", lty = 3)
lines(linear_model_confidence[, 3] ~ nevsky_org_df$ALT, col = "blue", lty = 3)

# Prediction of species richness for many altitutes
# (not very good because without any error estimate)
linear_model_predict <- predict(linear_model, new_data = data.frame(ALT = 50:2500))
linear_model_predict_df <- data.frame(ALT = 50:2500,
                                      SPE_ALL_PRED = linear_model_predict)


### Simple leave-one-out cross-validation of a linear model ####################
linear_model <- lm(SPE_ALL ~ ALT, data = nevsky_org_df)

inter <- lapply(seq(nrow(nevsky_org_df)), function(x){
  linear_model <- lm(SPE_ALL ~ ALT, data = nevsky_org_df[-x,])
  act_dependent <- nevsky_org_df$SPE_ALL[x]
  act_independent <- data.frame(ALT = nevsky_org_df$ALT[x])
  est_dependent <- predict(linear_model, act_independent)
  act_error <- abs(act_dependent - est_dependent)
  return(act_error)
})
linear_model_mean_error <- mean(unlist(inter))


### Multiple linear regression model ###########################################
# One version of a multiple linear regression model
mult_linerar_model <- lm(SPE_ALL ~ DECL_GR + EXP_GR + N + E_ + ALT + 
                           SOLO + MAT_ORG, data = nevsky_org_df)

# Looking for the best performing model (avoiding overfitting etc.)
# Function dredge will iterate over potential combinations of independent
# variables to be inlcuded into individual multiple linear regression models.
# After averaging the outcoming models, one can select the best performing model
# (or one of the better performing which runs with the smalest number of
# independent variables) by looking into the summary statistics 
# (smalest AICc values).
options(na.action = "na.fail")
model_selection <- dredge(mult_linerar_model)
model_average <- model.avg(model_selection)
summary(model_average)

# It turned out that one of the best models just requires three independent
# variables, so this model is chosen as the optimum one.
opt_mult_linerar_model <- lm(SPE_ALL ~ N + E_ + ALT, data = nevsky_org_df)
summary(opt_mult_linerar_model)
# plot(opt_mult_linerar_model)

# Compute leave-one-out cross-validation for this optimum multiple linear model
# (same as above for a linear one, except that more independent variables are
# used)
inter <- lapply(seq(nrow(nevsky_org_df)), function(x){
  opt_mult_linerar_model <- lm(SPE_ALL ~ N + E_ + ALT, data = nevsky_org_df[-x,])
  act_dependent <- nevsky_org_df$SPE_ALL[x]
  act_independent <- data.frame(N = nevsky_org_df$N[x],
                                E_ = nevsky_org_df$E_[x],
                                ALT = nevsky_org_df$ALT[x])
  est_dependent <- predict(opt_mult_linerar_model, act_independent)
  act_error <- abs(act_dependent - est_dependent)
  return(act_error)
})
linear_opt_mult_model_mean_error <- mean(unlist(inter))



### Simple non-linear regression analysis ######################################
model_loess <- loess(SPE_ALL ~ ALT, data = nevsky_org_df)

# For visualising the model, we need to estimate it for a sufficient number of
# values on the x axis. E.g. for each full value between the minimum and maximum
# x-axis value
values_x <- seq(min(nevsky_org_df$ALT), max(nevsky_org_df$ALT))
model_loess_y_est <- predict(model_loess, values_x)

plot(nevsky_org_df$SPE_ALL ~ nevsky_org_df$ALT)
lines(values_x, model_loess_y_est, lwd = 2, col = "blue")

linear_model <- lm(SPE_ALL ~ ALT, data = nevsky_org_df)
abline(linear_model, col = "red")



### Distribution of species along the elevational gradient #####################
# Static example using one explicitly selected species  
plot(LAV_ROT ~ ALT, data = nevsky_org_df)

glm_model <- glm(LAV_ROT ~ ALT + I(ALT**2), data = nevsky_org_df, family = "binomial")
values_x <- data.frame(ALT = seq(min(nevsky_org_df$ALT), max(nevsky_org_df$ALT)))
glm_model_y_est <- predict(glm_model, values_x)

plot(LAV_ROT ~ ALT, data = nevsky_org_df)
lines(values_x[,1], exp(glm_model_y_est) / (1 + exp(glm_model_y_est)))


# Loop implementation for common species (e.g. 9 most common ones)
# Compute species occurence across all plots (i.e. occupancy)
species_occupancy <- colSums(nevsky_org_df[, var_species])
species_common <- names(sort(species_occupancy, decreasing = TRUE)[1:9])
species_common <- which(names(nevsky_org_df) %in% species_common)


# Loop over each common species and produce a binary plot including a glm fit
# Set plot environment to fit the plots (save current one first)
user<-par(no.readonly=T)
par(mar=c(4,4,1,1),las=1,cex=0.9,mfcol=c(3,3))
values_x <- data.frame(ALT = seq(min(nevsky_org_df$ALT), max(nevsky_org_df$ALT)))
for(i in species_common){
  glm_model <- glm(nevsky_org_df[, i] ~ ALT + I(ALT**2), 
                   data = nevsky_org_df, family = "binomial")
  glm_model_y_est <- predict(glm_model, values_x)
  plot(nevsky_org_df[, i] ~ ALT, data = nevsky_org_df,
       ylab = names(nevsky_org_df)[i], xlab = "Elevation (m)")
  lines(values_x[,1], exp(glm_model_y_est) / (1 + exp(glm_model_y_est)))
}
par(user)
