### Author
# Name
# Version 2015-09-23

### Define pathes ##############################################################
setwd("D:/active/bis-fogo/school2015/myStory/Nevsky")

### Load libraries #############################################################
library(rgdal)
library(sp)


### Read data ##################################################################
nevsky_org_meta <- read.table("IE_2007_pontos_esp_meta.csv", 
                              sep = "\t", dec = ".", header = TRUE,
                              stringsAsFactors = FALSE)
str(nevsky_org_meta)

nevsky_org_shp <- readOGR("IE_2007_pontos_esp.shp", 
                          layer = "IE_2007_pontos_esp")
str(nevsky_org_shp)

nevsky_org_df <- data.frame(nevsky_org_shp)
str(nevsky_org_df)
nevsky_org_df$CYD_OBL <- as.numeric(as.character(nevsky_org_df$CYD_OBL))

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

nevsky_org_df$SPE_END <- 
  rowSums(nevsky_org_df[, which(colnames(nevsky_org_df) %in% species_endemic)])

nevsky_org_df$SPE_NAT <- 
  rowSums(nevsky_org_df[, which(colnames(nevsky_org_df) %in% species_native)])

nevsky_org_df$SPE_INT <- 
  rowSums(nevsky_org_df[, which(colnames(nevsky_org_df) %in% species_introduced)])
