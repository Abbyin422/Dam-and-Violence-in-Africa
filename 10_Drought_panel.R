rstudioapi::getActiveDocumentContext
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#setwd('/home/yyinak/Dams_Violence/Raw_Data')

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
######################################## Drought Severity Data #################################################
###########################################################################################################

### 1. Drought Severity
### load dams_basin shp
dams_basins <- readRDS(file = "Datasets/dams_basins.rds")

#### load drought severity data
files <- list.files('Hydro Data/Drought_Severity_10km', pattern = '.tif', full.names = T)

### Monthly Drought Severity
#### extract to dams_basin and loop
landcover <- raster('Modis_Yearly_Land_Cover/10km/landcover_2001.tif')


droughtsev_grid <- data.table()
for (i in seq_along(files)){
  # load drought severity raster
  drought <- raster(files[i])
  # resample
  drought_resample <- resample(drought, landcover, method="bilinear")
  ### extract drought severity data to dams_basins shp.
  drought_list <- exact_extract(drought_resample, dams_basins, include_cell=T, force_df=T, include_xy=F)
  ## convert list to dataframe
  registerDoParallel(cl <- makeCluster(detectCores()))
  drought_grid <- foreach(m = 1:length(drought_list), .combine = 'rbind', 
                     .packages = c('exactextractr','raster', 'data.table')) %dopar% {
    temp <- data.table(drought_list[[m]])
    if (nrow(temp) >0) {
      temp$dam_id <- dams_basins$dam_id[m]
      temp$HYBAS_ID <- dams_basins$HYBAS_ID[m]
    }
    temp
  }
  stopCluster(cl)
  ## drop duplicates
  drought_grid <- drought_grid %>%
    dplyr::group_by(cell, dam_id) %>%
    dplyr::mutate(check = n()) %>%
    filter(check==1 | coverage_fraction == max(coverage_fraction)) %>%
    dplyr::select(., -check, -coverage_fraction) %>%
    unique()
  ## time panel
  drought_grid$time <- substr(files[i],nchar("Hydro Data/Drought_Severity_10km/drought_severity_")+1,
                           nchar("Hydro Data/Drought_Severity_10km/drought_severity_")+7)
  drought_grid <- drought_grid %>%
    dplyr::mutate(year = as.numeric(substr(time, 1,4)),
                  month = as.numeric(substr(time, 6,7)))
  droughtsev_grid <- rbind(droughtsev_grid, data.table(drought_grid))
}

## rename drought severity
names(droughtsev_grid)[names(droughtsev_grid)=='value'] <- 'drought'


## Save
saveRDS(droughtsev_grid, file = "Datasets/droughtsev_grid.rds")

