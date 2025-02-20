#rstudioapi::getActiveDocumentContext
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

setwd('/home/yyinak/Dams_Violence/Raw_Data')

library(sf)
library(readxl)
library(raster)
library(terra)
library(ggplot2)
library(mapview)
library(dplyr)
library(exactextractr)
library(foreach)
library(doParallel)
library(units)
library(tidyr)
library(lubridate)
library(data.table)

rm(list=ls())

#load(file = 'Datasets/Delineate_River.RData')

sf_use_s2(FALSE)


###########################################################################################################
######################################## Landcover Data ###################################################
###########################################################################################################

# 1. Land Cover Raster
### load dams_basin shp
dams_basins <- readRDS(file = "Datasets/dams_basins.rds")

### repeat landcover data
files = list.files('Modis_Yearly_Land_Cover/10km', pattern = '.tif', full.names = F)
### loop
landcover_grid <- data.table()
for (i in seq_along(files)){
  # load land cover raster
  lc <- raster(paste('Modis_Yearly_Land_Cover/10km', files[i], sep='/'))
  ### extract landcover data to dams_basins shp.
  lc_list <- exact_extract(lc, dams_basins, include_cell=T, force_df=T, include_xy=F)
  ## convert list to dataframe
  registerDoParallel(cl <- makeCluster(detectCores()))
  lc_grid <- foreach(m = 1:length(lc_list), .combine = 'rbind', .packages = c('exactextractr','raster', 'data.table')) %dopar% {
    temp <- data.table(lc_list[[m]])
    if (nrow(temp) >0) {
      temp$dam_id <- dams_basins$dam_id[m]
      temp$HYBAS_ID <- dams_basins$HYBAS_ID[m]
    }
    temp
  }
  stopCluster(cl)
  ## drop duplicates
  lc_grid <- lc_grid %>%
    dplyr::group_by(cell, dam_id) %>%
    dplyr::mutate(check = n()) %>%
    filter(check==1 | coverage_fraction == max(coverage_fraction)) %>%
    dplyr::select(., -check, -coverage_fraction) %>%
    unique()
  ## year panel
  lc_grid <- lc_grid %>%
    dplyr::mutate(year = as.numeric(substr(files[i], 11,14)))
  landcover_grid <- rbind(landcover_grid, data.table(lc_grid))
}
#landcover_grid$year <- as.numeric(landcover_grid$year)


## cropland
names(landcover_grid)[names(landcover_grid)=='value'] <- 'landcover'
landcover_grid$cropland_dummy <- ifelse(landcover_grid$landcover==12|
                                          landcover_grid$landcover==14, 1, 0)

## Save
saveRDS(landcover_grid, file = "Datasets/landcover_grid.rds")

