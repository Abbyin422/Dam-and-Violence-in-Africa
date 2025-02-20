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


rm(list=ls())

load(file = 'Datasets/Delineate_River.RData')

sf_use_s2(FALSE)

############################################################################################
############################## Grid Level Dataset #############################################
############################################################################################

## 1. Make baseline grid cell
### Landcover raster
landcover <- raster('Modis_Yearly_Land_Cover/10km/landcover_2001.tif')

### extract landcover data to dams_basins shp.
dams_basins_list <- exact_extract(landcover, dams_basins, include_cell=T, force_df=T, include_xy=T)

## convert list to dataframe
registerDoParallel(cl <- makeCluster(8))
dams_basins_grid <- foreach(m = 1:length(dams_basins_list), .combine = 'rbind') %dopar% {
  temp <- data.frame(dams_basins_list[[m]])
  if (nrow(temp) >0) {
    temp$dam_id <- dams_basins$dam_id[m]
    temp$HYBAS_ID <- dams_basins$HYBAS_ID[m]
  }
  temp
}
stopCluster(cl)
## drop duplicates
dams_basins_grid <- dams_basins_grid %>%
  dplyr::group_by(cell, dam_id) %>%
  dplyr::mutate(check = n()) %>%
  filter(check==1 | coverage_fraction == max(coverage_fraction)) %>%
  dplyr::select(., -check, -coverage_fraction)

## drop landcover variables
dams_basins_grid$value <- NULL
#names(dams_basins_grid)[names(dams_basins_grid)=='value'] <- 'landcover'
#dams_basins_grid$cropland_dummy <- ifelse(dams_basins_grid$landcover==12|
#                                            dams_basins_grid$landcover==14, 1, 0)

## combine dams_basin info. with grid level data
dams_basins_grid_compl <- merge(dams_basins_grid, dams_basins, by = c('dam_id', 'HYBAS_ID'),
                                all.x = T) 
## transfer raster dataset into polygon point data
dams_basins_grid_compl$geometry <- NULL
dams_basins_grid_compl <- st_as_sf(x = dams_basins_grid_compl, coords = c("x", "y"),
                                   crs = st_crs(dams), remove = F)
## distance to dams
dams_basins_grid_compl$dist_dams_m <- NA
for (i in 1:nrow(dams_basins_grid_compl)){
  dist <- st_distance(dams_basins_grid_compl[i,], 
                      dams[dams$dam_id==dams_basins_grid_compl$dam_id[i], ], 
                      by_element = F)
  dams_basins_grid_compl$dist_dams_m[i] <- dist
}


## 2. Distance to stem rivers
### st_join grid cells to nearest river
registerDoParallel(cl <- makeCluster(8))
dams_basin_river_grid <- foreach(i = 1:nrow(dams), .combine = 'rbind', .packages = c('dplyr', 'sf')) %dopar% {
  sf_use_s2(FALSE)
  ### choose rivers in basins (dam located basins)
  river_temp <- stemrivers_affected_dams[stemrivers_affected_dams$dam_id==i,]
  ### dams specific dams_basins_grid_compl
  dams_basins_grid_temp <- dams_basins_grid_compl[dams_basins_grid_compl$dam_id==i,]
  ### spatially join - nearest river
  dams_basins_grid_temp <- dams_basins_grid_temp[,-22] %>%
    st_join(river_temp[,c(1,3:14, 17:22)], join = st_nearest_feature) %>%
    distinct()
  ### distance between rivers and grid cells
  dams_basins_grid_temp$dist_rivers_m <- NA
  for (j in 1:nrow(dams_basins_grid_temp)){
    dist <- st_distance(dams_basins_grid_temp[j,], 
                        river_temp[river_temp$HYRIV_ID==dams_basins_grid_temp$HYRIV_ID[j], ], 
                        by_element = F)
    dams_basins_grid_temp$dist_rivers_m[j] <- dist
  }
  dams_basins_grid_temp
}

stopCluster(cl)


## 3. Grid level African administrative map 
### African map
africa <- st_read("Africa shapefile/afr_g2014_2013_2/afr_g2014_2013_2.shp")
### transform crs
africa <- st_transform(africa, st_crs(dams))
### st_join grid cells to adminstrative map
dams_basin_river_grid <- dams_basin_river_grid %>%
  st_join(africa) %>%
  distinct()

## 4. Climate Zone
### read climate zone shp.
climzone <- st_read('Climate zones/Shapefiles/world_climates_completed_koppen_geiger.shp') 
climzone <- st_transform(climzone, st_crs(dams))
### st_join grid cells to climate zone map
dams_basin_river_grid <- dams_basin_river_grid %>%
  st_join(climzone) %>%
  distinct()


## 5. Lithology
#### load lithology raster and convert it to shp.
litho <- terra::rast('Lithology Data/africa_surface_lithology/Africa_Surface_Lithology.tif')
litho_shp <- terra::as.polygons(litho, dissolve = T)
litho_shp2 <- st_as_sf(litho_shp)
litho_shp2 <- st_transform(litho_shp2, st_crs(dams))
### st_join grid cells to lithology map
dams_basin_river_grid <- dams_basin_river_grid %>%
  st_join(litho_shp2) %>%
  distinct()


## 6. Groundwater Storage
#### load grstorage raster and convert it to shp.
grstorage <- terra::rast('Hydro Data/GroundwaterStorage/groundwater_storage.tif')
grs_shp <- terra::as.polygons(grstorage, dissolve = T)
grs_shp2 <- st_as_sf(grs_shp)
grs_shp2 <- st_transform(grs_shp2, st_crs(dams))
### st_join grid cells to groundwater storage map
dams_basin_river_grid <- dams_basin_river_grid %>%
  st_join(grs_shp2) %>%
  distinct()

## 7. Ethnic Fraction
ethnic_map <- st_read('Ethnic_Groups_in_Africa/Ethnic_Groups_in_Africa.shp')
ethnic_map <- st_transform(ethnic_map, st_crs(dams))
ethnic_map_aggr <- ethnic_map %>%
  group_by(Ethnic_g) %>%
  summarise() %>%
  st_as_sf()

### st_join grid cells to ethnic_map
dams_basin_river_grid <- dams_basin_river_grid %>%
  st_join(ethnic_map_aggr) %>%
  distinct()

### num of ethnic groups around a dam
dams_basin_river_grid <- dams_basin_river_grid %>%
  group_by(dam_id) %>%
  mutate(num_ethnic_dam = n())

### maping on grid cell
### extract landcover data to ethnic_map shp.
ethnic_list <- exact_extract(landcover, ethnic_map, include_cell=T, force_df=T, include_xy=T)

## convert list to dataframe
registerDoParallel(cl <- makeCluster(8))
ethnic_grid <- foreach(m = 1:length(ethnic_list), .combine = 'rbind') %dopar% {
  temp <- data.frame(ethnic_list[[m]])
  if (nrow(temp) >0) {
    temp$Ethnic_g <- ethnic_map$Ethnic_g[m]
  }
  temp
}
stopCluster(cl)

## drop duplicates
ethnic_grid <- ethnic_grid %>%
  select(-coverage_fraction) %>%
  unique()

## num_ethnicity in a cell
ethnic_frac <- ethnic_grid %>%
  group_by(cell) %>%
  summarise(num_ethnic_cell = n())

### merge grid cells to num_ethnicity grid
dams_basin_river_grid <- merge(dams_basin_river_grid, ethnic_frac, by='cell', all.x = T)

## Save as RDS
saveRDS(dams_basin_river_grid, file = "Datasets/dams_basin_river_grid.rds")


