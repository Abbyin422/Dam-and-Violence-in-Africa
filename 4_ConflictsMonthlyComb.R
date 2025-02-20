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

###########################################################################################################
######################################## Combine Conflict Data ############################################
###########################################################################################################

dams_grid_conflict_panel <- data.table()

for (i in 1:12){
  temp <- readRDS(file = paste("Datasets/dams_grid_conflict_panel", i, ".rds", sep=''))
  temp <- temp %>%
    dplyr::group_by(dam_id) %>%
    dplyr::mutate(paired = ifelse(any(affected_riv==0), 1, 0))
  temp <- temp %>%
    mutate(dry_season = ifelse(REGION=='Northern Africa' & month %in% c(6:9)|
                                 REGION=='Eastern Africa' & month %in% c(7:9)|
                                 REGION=='Middle Africa' & month %in% c(11:12,1)|
                                 REGION=='Southern Africa' & month %in% c(4:10)|
                                 REGION=='Western Africa' & month %in% c(11:12, 1:4), 1, 0),
           wet_season = ifelse(REGION=='Northern Africa' & month %in% c(1:3,11:12)|
                                 REGION=='Eastern Africa' & month %in% c(4:6,10:12)|
                                 REGION=='Middle Africa' & month %in% c(2:4,6:10)|
                                 REGION=='Southern Africa' & month %in% c(1:3,11:12)|
                                 REGION=='Western Africa' & month %in% c(4:7, 9:10), 1, 0)) %>%
    mutate(other_season = ifelse(dry_season==0 & wet_season==0, 1, 0))
  
  dams_grid_conflict_panel <- rbind(dams_grid_conflict_panel, as.data.table(temp))
  
}

saveRDS(dams_grid_conflict_panel, file ="Datasets/conflict_panel_10km.rds")







################ Alt #####################
dams_grid_conflict_panel <- data.table()
for (i in 1:12){
  temp <- readRDS(file = paste("Datasets/dams_grid_conflict_panel_Alt", i, ".rds", sep=''))
  temp <- temp %>%
    dplyr::group_by(dam_id) %>%
    dplyr::mutate(paired = ifelse(any(affected_riv==0), 1, 0))
  temp <- temp %>%
    mutate(dry_season = ifelse(REGION=='Northern Africa' & month %in% c(6:9)|
                                 REGION=='Eastern Africa' & month %in% c(7:9)|
                                 REGION=='Middle Africa' & month %in% c(11:12,1)|
                                 REGION=='Southern Africa' & month %in% c(4:10)|
                                 REGION=='Western Africa' & month %in% c(11:12, 1:4), 1, 0),
           wet_season = ifelse(REGION=='Northern Africa' & month %in% c(1:3,11:12)|
                                 REGION=='Eastern Africa' & month %in% c(4:6,10:12)|
                                 REGION=='Middle Africa' & month %in% c(2:4,6:10)|
                                 REGION=='Southern Africa' & month %in% c(1:3,11:12)|
                                 REGION=='Western Africa' & month %in% c(4:7, 9:10), 1, 0)) %>%
    mutate(other_season = ifelse(dry_season==0 & wet_season==0, 1, 0))
  dams_grid_conflict_panel <- rbind(dams_grid_conflict_panel, as.data.table(temp))
}
saveRDS(dams_grid_conflict_panel, file = paste("Datasets/conflict_panel_Alt.rds", sep=''))



