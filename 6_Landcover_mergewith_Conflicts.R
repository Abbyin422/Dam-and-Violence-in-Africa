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

#load(file = 'Datasets/Delineate_River.RData')

sf_use_s2(FALSE)


###########################################################################################################
######################################## Combine Landcover and All-Type Conflicts ##################################
###########################################################################################################
setDTthreads(10)

# 1. Merge LandCover with Conflicts Loop
### load landcover data
landcover_grid <- readRDS(file = "Datasets/landcover_grid.rds")

### load conflicts data
conflict_panel <- readRDS(file = "Datasets/conflict_panel_10km.rds")
### merge
for (i in 2001:2022){
  if(i==2001){
    conflict_landcover <- as.data.table(conflict_panel) %>%
      filter(year <= i) %>%
      left_join(
        as.data.table(landcover_grid) %>% filter(year == i),
        by = c('cell', 'year', 'dam_id', 'HYBAS_ID')
      )
    conflict_landcover <- conflict_landcover %>%
      arrange(cell, dam_id, HYBAS_ID, year, .by_group = T) %>%
      fill(., landcover, cropland_dummy, .direction = 'up')
  } else{
    conflict_landcover <- as.data.table(conflict_panel) %>%
      filter(year == i) %>%
      left_join(
        as.data.table(landcover_grid) %>% filter(year == i),
        by = c('cell', 'year', 'dam_id', 'HYBAS_ID')
      )
  }
  ## Save as RDS
  saveRDS(conflict_landcover, file = paste("Datasets/conflict_landcover", i, ".rds", sep=''))
}

  

## combine yearly conflict_landcover
conflict_landcover <- data.table()

for (i in 2001:2022){
  conflict_landcover_year <- readRDS(file = paste("Datasets/conflict_landcover",i, ".rds", sep=''))
  conflict_landcover <- rbind(conflict_landcover, as.data.table(conflict_landcover_year))
}

## Save
saveRDS(conflict_landcover, file = "Datasets/conflict_landcover_10km.rds")









