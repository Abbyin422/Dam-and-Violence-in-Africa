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
######################################## Groundwater Data #################################################
###########################################################################################################

### 1. Groundwater
### load dams_basin shp
dams_basins <- readRDS(file = "Datasets/dams_basins.rds")

#### load groundwater data (LWET)
files <- list.files('Hydro Data/Water-Equivalent-Thickness Surface-Mass Anomaly RL06', pattern = '.nc4', full.names = T)

### Monthly groundwater
#### extract to dams_basin and loop
landcover <- raster('Modis_Yearly_Land_Cover/10km/landcover_2001.tif')

groundwater_grid <- data.table()
for (i in seq_along(files)){
  # load groundwater raster
  groundwater <- raster(files[i], varname = 'GAC_msc_Lmax180')
  # resample
  gw <- resample(groundwater, landcover, method="bilinear")
  ### extract groundwater data to dams_basins shp.
  gw_list <- exact_extract(gw, dams_basins, include_cell=T, force_df=T, include_xy=F)
  ## convert list to dataframe
  registerDoParallel(cl <- makeCluster(detectCores()))
  gw_grid <- foreach(m = 1:length(gw_list), .combine = 'rbind', 
                     .packages = c('exactextractr','raster', 'data.table')) %dopar% {
    temp <- data.table(gw_list[[m]])
    if (nrow(temp) >0) {
      temp$dam_id <- dams_basins$dam_id[m]
      temp$HYBAS_ID <- dams_basins$HYBAS_ID[m]
    }
    temp
  }
  stopCluster(cl)
  ## drop duplicates
  gw_grid <- gw_grid %>%
    dplyr::group_by(cell, dam_id) %>%
    dplyr::mutate(check = n()) %>%
    filter(check==1 | coverage_fraction == max(coverage_fraction)) %>%
    dplyr::select(., -check, -coverage_fraction) %>%
    unique()
  ## time panel
  gw_grid$time <- getZ(groundwater)
  gw_grid <- gw_grid %>%
    dplyr::mutate(year = as.numeric(substr(time, 1,4)),
                  month = as.numeric(substr(time, 6,7)))
  groundwater_grid <- rbind(groundwater_grid, data.table(gw_grid))
}

## rename groundwater
names(groundwater_grid)[names(groundwater_grid)=='value'] <- 'groundwater'


## Save
saveRDS(groundwater_grid, file = "Datasets/groundwater_grid.rds")

