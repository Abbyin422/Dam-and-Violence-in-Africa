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
######################################## Combine Samples with Control vars ################################
###########################################################################################################
setDTthreads(10)


### load control variables data
control_vars <- readRDS(file = "Datasets/control_vars.rds")

# 1. Merge with Conflicts Loop
### load conflicts data
conflict_landcover <- readRDS(file = "Datasets/conflict_landcover_10km.rds")

### merge
for (i in 1997:2022){
  main_sample <- as.data.table(conflict_landcover) %>%
    filter(year == i) %>%
    left_join(
      as.data.table(control_vars) %>% filter(year == i),
      by = c('cell', 'year', 'month', 'dam_id', 'HYBAS_ID')
    )
  ## Save as RDS
  saveRDS(main_sample, file = paste("Datasets/main_sample", i, ".rds", sep=''))
}

## combine yearly main_sample
main_sample <- data.table()

for (i in 1997:2022){
  main_sample_year <- readRDS(file = paste("Datasets/main_sample",i, ".rds", sep=''))
  main_sample <- rbind(main_sample_year, as.data.table(main_sample))
}

## Save
saveRDS(main_sample, file = "Datasets/main_sample.rds")

head(main_sample)








