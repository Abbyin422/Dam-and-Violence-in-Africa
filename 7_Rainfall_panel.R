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


#load(file = 'Delineate_River.RData')

sf_use_s2(FALSE)


###########################################################################################################
######################################## Rainfall Data ####################################################
###########################################################################################################

### 1. Rainfall
### load dams_basin shp
dams_basins <- readRDS(file = "Datasets/dams_basins.rds")

#### load rainfall data
files <- list.files('Rainfall Data/Monthly_97_24', pattern = '.nc', full.names = F)
files <- files[216:539]

### Monthly rainfall
#### extract to dams_basin and loop
landcover <- raster('Modis_Yearly_Land_Cover/10km/landcover_2001.tif')

rainfall_grid <- data.table()
for (i in seq_along(files)){
  # load rainfall raster
  rainfall <- raster(paste('Rainfall Data/Monthly_97_24', files[i], sep='/'))
  # resample
  rainfall <- resample(rainfall, landcover, method="bilinear")
  ### extract rainfall data to dams_basins shp.
  rainfall_list <- exact_extract(rainfall, dams_basins, include_cell=T, force_df=T, include_xy=F)
  ## convert list to dataframe
  registerDoParallel(cl <- makeCluster(detectCores()))
  rf_grid <- foreach(m = 1:length(rainfall_list), .combine = 'rbind', 
                     .packages = c('exactextractr','raster', 'data.table')) %dopar% {
    temp <- data.table(rainfall_list[[m]])
    if (nrow(temp) >0) {
      temp$dam_id <- dams_basins$dam_id[m]
      temp$HYBAS_ID <- dams_basins$HYBAS_ID[m]
    }
    temp
  }
  stopCluster(cl)
  ## drop duplicates
  rf_grid <- rf_grid %>%
    dplyr::group_by(cell, dam_id) %>%
    dplyr::mutate(check = n()) %>%
    filter(check==1 | coverage_fraction == max(coverage_fraction)) %>%
    dplyr::select(., -check, -coverage_fraction) %>%
    unique()
  ## year panel
  rf_grid <- rf_grid %>%
    dplyr::mutate(year = as.numeric(substr(files[i], 1,4)),
                  month = as.numeric(substr(files[i], 5,6)))
  rainfall_grid <- rbind(rainfall_grid, data.table(rf_grid))
}

## rename rainfall
names(rainfall_grid)[names(rainfall_grid)=='value'] <- 'rainfall'

### annual rainfall
rainfall_grid <- rainfall_grid %>%
  dplyr::group_by(cell, year, dam_id) %>%
  dplyr::mutate(rainfall_annual = sum(rainfall))

## Save
saveRDS(rainfall_grid, file = "Datasets/rainfall_grid.rds")

