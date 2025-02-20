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
######################################## Water Deficit Data #################################################
###########################################################################################################

### 1. Water Deficit
### load dams_basin shp
dams_basins <- readRDS(file = "Datasets/dams_basins.rds")

#### load water deficit data
files <- list.files('Hydro Data/Water_Deficit_10km', pattern = '.tif', full.names = T)

deficit <- raster(files[1])
plot(deficit)

### Monthly water deficit
#### extract to dams_basin and loop
landcover <- raster('Modis_Yearly_Land_Cover/10km/landcover_2001.tif')

# Set threads
setDTthreads(10)


waterdef_grid <- data.table()
for (i in seq_along(files)){
  # load water deficit raster
  deficit <- raster(files[i])
  # resample
  deficit_resample <- resample(deficit, landcover, method="bilinear")
  ### extract water deficit data to dams_basins shp.
  deficit_list <- exact_extract(deficit_resample, dams_basins, include_cell=T, force_df=T, include_xy=F)
  ## convert list to dataframe
  registerDoParallel(cl <- makeCluster(detectCores()))
  deficit_grid <- foreach(m = 1:length(deficit_list), .combine = 'rbind', 
                     .packages = c('exactextractr','raster', 'data.table')) %dopar% {
    temp <- data.table(deficit_list[[m]])
    if (nrow(temp) >0) {
      temp$dam_id <- dams_basins$dam_id[m]
      temp$HYBAS_ID <- dams_basins$HYBAS_ID[m]
    }
    temp
  }
  stopCluster(cl)
  ## drop duplicates
  deficit_grid <- deficit_grid %>%
    dplyr::group_by(cell, dam_id) %>%
    dplyr::mutate(check = n()) %>%
    filter(check==1 | coverage_fraction == max(coverage_fraction)) %>%
    dplyr::select(., -check, -coverage_fraction) %>%
    unique()
  ## time panel
  deficit_grid$time <- substr(files[i],nchar("Hydro Data/Water_Deficit_10km/water_def_")+1,
                           nchar("Hydro Data/Water_Deficit_10km/water_def_")+7)
  deficit_grid <- deficit_grid %>%
    dplyr::mutate(year = as.numeric(substr(time, 1,4)),
                  month = as.numeric(substr(time, 6,7)))
  waterdef_grid <- rbind(waterdef_grid, data.table(deficit_grid))
}

## rename water deficit
names(waterdef_grid)[names(waterdef_grid)=='value'] <- 'waterdef'


## Save
saveRDS(waterdef_grid, file = "Datasets/waterdef_grid.rds")



