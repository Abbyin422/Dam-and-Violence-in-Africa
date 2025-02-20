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
######################################## Groundwater Storage Data #########################################
###########################################################################################################

### 1. Water gws
### load dams_basin shp
dams_basins <- readRDS(file = "Datasets/dams_basins.rds")

#### load water gws data
files <- list.files('Hydro Data/Afr_Groundwater_Storage_10km', pattern = '.tif', full.names = T)

### Monthly water gws
#### extract to dams_basin and loop
landcover <- raster('Modis_Yearly_Land_Cover/10km/landcover_2001.tif')

# Set threads
setDTthreads(10)

gws_grid <- data.table()
for (i in seq_along(files)){
  # load water gws raster
  gws <- raster(files[i])
  # resample
  gws_resample <- resample(gws, landcover, method="bilinear")
  ### extract water gws data to dams_basins shp.
  gws_list <- exact_extract(gws_resample, dams_basins, include_cell=T, force_df=T, include_xy=F)
  ## convert list to dataframe
  registerDoParallel(cl <- makeCluster(detectCores()))
  gws_month_grid <- foreach(m = 1:length(gws_list), .combine = 'rbind', 
                          .packages = c('exactextractr','raster', 'data.table')) %dopar% {
                            temp <- data.table(gws_list[[m]])
                            if (nrow(temp) >0) {
                              temp$dam_id <- dams_basins$dam_id[m]
                              temp$HYBAS_ID <- dams_basins$HYBAS_ID[m]
                            }
                            temp
                          }
  stopCluster(cl)
  ## drop duplicates
  gws_month_grid <- gws_month_grid %>%
    dplyr::group_by(cell, dam_id) %>%
    dplyr::mutate(check = n()) %>%
    filter(check==1 | coverage_fraction == max(coverage_fraction)) %>%
    dplyr::select(., -check, -coverage_fraction) %>%
    unique()
  ## time panel
  gws_month_grid$time <- substr(files[i],nchar("Hydro Data/Afr_Groundwater_Storage_10km/gws_")+1,
                              nchar("Hydro Data/Afr_Groundwater_Storage_10km/gws_")+7)
  gws_month_grid <- gws_month_grid %>%
    dplyr::mutate(year = as.numeric(substr(time, 1,4)),
                  month = as.numeric(substr(time, 6,7)))
  gws_grid <- rbind(gws_grid, data.table(gws_month_grid))
}

## rename water gws
names(gws_grid)[names(gws_grid)=='value'] <- 'gws_mm'


## Save
saveRDS(gws_grid, file = "Datasets/gws_grid.rds")



