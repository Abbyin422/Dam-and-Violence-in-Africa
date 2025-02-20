rstudioapi::getActiveDocumentContext
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#setwd('/home/yyinak/Dams_Violence/Raw_Data')

library(sf)
library(haven)
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
sf_use_s2(FALSE)

# Set threads
setDTthreads(10)

###########################################################################################################
######################################## Construct Control Variables ######################################
###########################################################################################################

process_raster_data <- function(directory, data_prefix, dams_basins, landcover, value_name) {
  
  # List files in the directory, optionally filtering by pattern
  files <- list.files(directory, pattern = '.tif', full.names = TRUE)
  
  # Initialize an empty data.table to store the results
  combined_grid <- data.table()
  
  # Loop through each file
  for (i in seq_along(files)) {
    # Load the raster data
    current_raster <- raster(files[i])
    
    # Resample the raster data
    resampled_raster <- resample(current_raster, landcover, method = "bilinear")
    
    # Extract data to dams_basins shapefile
    extracted_list <- exact_extract(resampled_raster, dams_basins, include_cell = TRUE, force_df = TRUE, include_xy = FALSE)
    
    # Convert the list to a data.table using parallel processing
    registerDoParallel(cl <- makeCluster(detectCores()))
    grid_data <- foreach(m = 1:length(extracted_list), .combine = 'rbind', 
                         .packages = c('exactextractr','raster', 'data.table')) %dopar% {
                           temp <- data.table(extracted_list[[m]])
                           if (nrow(temp) > 0) {
                             temp$dam_id <- dams_basins$dam_id[m]
                             temp$HYBAS_ID <- dams_basins$HYBAS_ID[m]
                           }
                           temp
                         }
    stopCluster(cl)
    
    
    # Remove duplicates based on cell and dam_id, prioritizing higher coverage_fraction
    grid_data <- grid_data %>%
      dplyr::group_by(cell, dam_id) %>%
      dplyr::mutate(check = n()) %>%
      dplyr::filter(check == 1 | coverage_fraction == max(coverage_fraction)) %>%
      dplyr::select(-check, -coverage_fraction) %>%
      dplyr::ungroup() %>% # Important to ungroup after filtering
      unique()
    
    # Extract the time information from the filename
    prefix_length <- nchar(data_prefix)
    grid_data$time <- str_sub(files[i], prefix_length + 1, -5) # Extract time dynamically
    
    # Append the current month's data to the combined data.table
    combined_grid <- rbind(combined_grid, data.table(grid_data))
  }
  
  # Rename the 'value' column to the specified value_name
  names(combined_grid)[names(combined_grid) == 'value'] <- value_name
  
  return(combined_grid)
}

### load dams_basin shp
dams_basins <- readRDS(file = "Datasets/dams_basins.rds")

#### extract to dams_basin and loop
landcover <- raster('Modis_Yearly_Land_Cover/10km/landcover_2001.tif')

#### 1. Population
pop_grid <- process_raster_data('Africa_Yearly_Pop_10km',
                                     data_prefix = 'Africa_Yearly_Pop_10km/Pop_',
                                     dams_basins = dams_basins,
                                     landcover = landcover,
                                     value_name = 'PopCount')
## time
pop_grid <- pop_grid %>%
  mutate(year = as.numeric(substr(time, nchar(time)-3, nchar(time)))) %>%
  dplyr::select(-time)

## save
saveRDS(pop_grid, 'Datasets/pop_grid.rds')


#### 2. Net Primary Production
Npp_grid <- process_raster_data('Africa_Npp_10km',
                                data_prefix = 'Africa_Npp_10km/Npp_',
                                dams_basins = dams_basins,
                                landcover = landcover,
                                value_name = 'Npp')

## time
Npp_grid <- Npp_grid %>%
  mutate(year = as.numeric(substr(time, nchar(time)-3, nchar(time)))) %>%
  dplyr::select(-time)

## save
saveRDS(Npp_grid, 'Datasets/npp_grid.rds')



#### 3. Temperature Max
tempmax_grid <- process_raster_data('Africa_Monthly_Temp_Max_10km',
                                data_prefix = 'Africa_Monthly_Temp_Max_10km/TempMax_',
                                dams_basins = dams_basins,
                                landcover = landcover,
                                value_name = 'TempMax')

## time
tempmax_grid <- tempmax_grid %>%
  mutate(year = as.numeric(substr(time, 1, 4)),
         month = as.numeric(substr(time, 6, 7))) 

## save
saveRDS(tempmax_grid, 'Datasets/tempmax_grid.rds')


#### 4. Temperature Max
tempmin_grid <- process_raster_data('Africa_Monthly_Temp_Min_10km',
                                    data_prefix = 'Africa_Monthly_Temp_Min_10km/TempMin_',
                                    dams_basins = dams_basins,
                                    landcover = landcover,
                                    value_name = 'TempMin')

## time
tempmin_grid <- tempmin_grid %>%
  mutate(year = as.numeric(substr(time, 1, 4)),
         month = as.numeric(substr(time, 6, 7))) 

## save
saveRDS(tempmin_grid, 'Datasets/tempmin_grid.rds')


### 5. Rainfall/Water Deficit/Drought/Soil Moisture/Groundwater Storage
rainfall_grid <- readRDS('Datasets/rainfall_grid.rds')
soilms_grid <- readRDS('Datasets/soilmoisture_grid.rds')
waterdef_grid <- readRDS('Datasets/waterdef_grid.rds')
droughtsev_grid <- readRDS('Datasets/droughtsev_grid.rds')
gws_grid <- readRDS('Datasets/gws_grid.rds')

### 6. Mine Deposit
mine_prod <- read_dta('mine_deposit/v2_all_minerals_withGPS.dta')
mine_points <- mine_prod %>%
  dplyr::select(-year, -production, -var44, -NAMES_STD, 
                -mineral, -unit, -id_mine, -DomesticMine_Broad) %>%
  unique() %>%
  st_as_sf(coords = c('longitude', 'latitude'), crs = '+init=EPSG:4326')

## extract mine to grid cell
mine_grid <- raster::extract(landcover, mine_points, cellnumbers=T, sp=TRUE)
mine_grid <- mine_grid@data

## count # of mines
mine_num <- mine_grid %>%
  group_by(cells, actual_start_up_y, actual_closure_y) %>%
  summarise(num_mine = n())

## panel data of #
create_panel_data <- function(df) {
  # Create a panel data frame with years from 1997 to 2022
  panel_data <- df %>%
    crossing(year = 1997:2022) %>%
    as.data.table() # Convert to data.table for efficiency
  
  # Calculate num_mine based on year, actual_start_up_y, and actual_closure_y
  panel_data[, num_mine := {
    ifelse(is.na(actual_start_up_y) | is.na(actual_closure_y), num_mine, # Corrected: Use | and ifelse
           ifelse(year >= actual_start_up_y & (is.na(actual_closure_y) | year <= actual_closure_y), 1L, 0L))
  }, by = .(cells, year)] # Group by cells and year for the calculation
  
  return(panel_data)
}

mine_num_panel <- create_panel_data(mine_num)

mine_num_panel <- mine_num_panel %>%
  group_by(cells, year) %>%
  summarise(num_mine = sum(num_mine))


### 7. Combine 
control_vars <- as.data.table(tempmax_grid) %>%
  left_join(as.data.table(tempmin_grid),
            by = c('cell', 'dam_id', 'HYBAS_ID', 'time', 'year', 'month')) %>%
  left_join(as.data.table(Npp_grid),
            by = c('cell', 'dam_id', 'HYBAS_ID', 'year')) %>%
  left_join(as.data.table(pop_grid),
            by = c('cell', 'dam_id', 'HYBAS_ID', 'year')) %>%
  left_join(as.data.table(mine_num_panel),
            join_by(cell==cells, year==year)) %>%
  left_join(as.data.table(rainfall_grid),
            by = c('cell', 'dam_id', 'HYBAS_ID', 'year', 'month')) %>%
  left_join(as.data.table(droughtsev_grid),
            by = c('cell', 'dam_id', 'HYBAS_ID', 'year', 'month', 'time')) %>%
  left_join(as.data.table(gws_grid),
            by = c('cell', 'dam_id', 'HYBAS_ID', 'year', 'month', 'time')) %>%
  left_join(as.data.table(soilms_grid),
            by = c('cell', 'dam_id', 'HYBAS_ID', 'year', 'month', 'time')) %>%
  left_join(as.data.table(waterdef_grid),
            by = c('cell', 'dam_id', 'HYBAS_ID', 'year', 'month', 'time'))

control_vars <- control_vars %>%
  mutate(num_mine = ifelse(is.na(num_mine), 0, num_mine))


## save
saveRDS(control_vars, 'Datasets/control_vars.rds')

#control_vars <- readRDS('Datasets/control_vars.rds')



