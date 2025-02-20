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

sf_use_s2(FALSE)


###########################################################################################################
######################################## Conflict Data ####################################################
###########################################################################################################

## 1. Conflicts
### load conflict
conflict <- read.csv('Conflict Datasets/ACLED Dataset/conflict_1997_2022.csv', sep = ";")
### Convert SpatialPointsDataFrame to sf object
conflict <- st_as_sf(conflict, coords = c("longitude", "latitude"), remove=F)
### Assign CRS to the sf object
dams <- readRDS(file = "Datasets/dams_combined.rds")
st_crs(conflict) <- st_crs(dams)

library(tidyr)
### reshape conflict: actors long to wide
conflict <- conflict %>%
  group_by(data_id) %>%
  dplyr::rename(., actor = actor1, 
                assoc_actor = assoc_actor_1, inter = inter1) %>%
  dplyr::mutate(actor_index = row_number()) %>%
  pivot_wider(
    names_from = 'actor_index',
    names_prefix = "",
    values_from = c('actor', 'assoc_actor', 'inter'))
### relevel inter_code
inter_level <- c('state_force', 'rebel_groups', 'political_militias',
                 'identity_militias', 'rioters','protesters', 'civilians',
                 'external/other_forces')

conflict$inter_1_name <- factor(conflict$inter_1)
conflict$inter_2_name <- factor(conflict$inter_2)
levels(conflict$inter_1_name) <- inter_level
levels(conflict$inter_2_name) <- inter_level

### count the # of conflicts in each grid cell
#### raster grid cell to conflict data
landcover <- raster('Modis_Yearly_Land_Cover/10km/landcover_2001.tif')
conflict_grid <- raster::extract(landcover, conflict, cellnumbers=T, sp=TRUE)
conflict_grid <- conflict_grid@data
#conflict_grid$LC_Type1 <- NULL

#### month
library(lubridate)
conflict_grid <- conflict_grid %>%
  dplyr::mutate(month = month(dmy(event_date)))

#### limit to riots, protests, and violence against civilians
conflict_grid <- conflict_grid %>%
  dplyr::filter(event_type %in% c('Battles','Protests', 'Riots', 
                                  'Violence against civilians')|
                  sub_event_type %in% c("Looting/property destruction"))

saveRDS(conflict_grid, 'Datasets/conflict_grid.rds')
head(conflict_grid)

### Water related conflicts
water_conflict <- conflict_grid %>%
  filter(grepl('water|Water|drought|Drought', notes)==T)

#### count monthly num by sub-types
conflict_event_num <- conflict_grid %>%
  dplyr::group_by(cells, sub_event_type, year, month) %>%
  dplyr::summarise(num = n()) %>%
  ungroup()

#### another type: water-related
conflict_water_num <- water_conflict %>%
  dplyr::group_by(cells, year, month) %>%
  dplyr::summarise(num = n()) %>%
  ungroup() %>%
  mutate(sub_event_type = "Water")

#### combine
conflict_num_long <- rbind(conflict_event_num, conflict_water_num)

#### gsub
library(stringr)
conflict_num_long <- conflict_num_long %>%
  mutate(sub_event_type=ifelse(sub_event_type=="Looting/property destruction",
                               'Looting and property destruction',
                               ifelse(sub_event_type=='Abduction/forced disappearance',
                                      'Abduction and forced disappearance', sub_event_type))) %>%
  mutate(sub_event_type = gsub('-', '', sub_event_type)) %>%
  mutate(sub_event_type = gsub(' ', '_', sub_event_type)) %>%
  mutate(sub_event_type = str_to_lower(sub_event_type))

#### long to wide
conflict_num_wide <- conflict_num_long %>%
  pivot_wider(
    id_cols = c('cells','year', 'month'),
    names_from = all_of('sub_event_type'),
    values_from = all_of('num'),
    names_prefix = "num_",
    values_fill = 0
  )

#### sum #
conflict_num_wide <- conflict_num_wide %>%
  rowwise() %>%
  mutate(num_factor = sum(num_peaceful_protest,num_violent_demonstration,
                          num_armed_clash, num_protest_with_intervention,
                          num_excessive_force_against_protesters,
                          num_government_regains_territory,
                          num_nonstate_actor_overtakes_territory),
         num_output = sum(num_mob_violence, num_attack, 
                          num_looting_and_property_destruction,
                          num_sexual_violence,
                          num_abduction_and_forced_disappearance)) %>%
  mutate(num_all = sum(num_factor, num_output))

saveRDS(conflict_num_wide, 'Datasets/conflict_num_wide.rds')

## 2. Combine conflict grid num with dams_basin_river_grid
### load dams_basin_river_grid data
#dams_basin_river_grid <- readRDS(file = "Datasets/dams_basin_river_grid.rds")

### make panel data
#time_panel <- expand.grid(year = c(1997:2022), month=(1:12))
#grid_panel <- expand.grid(cell = unique(dams_basin_river_grid$cell), year = c(1997:2022))
#dams_grid_panel <- merge(grid_panel, time_panel, by = 'year')
#dams_grid_panel <- merge(dams_basin_river_grid, dams_grid_panel, by = 'cell')

## save dams panel
#saveRDS(dams_grid_panel, file = "Datasets/dams_grid_panel.rds")

dams_grid_panel <- readRDS(file = "Datasets/dams_grid_panel.rds")
dams_updated <- readRDS(file = "Datasets/dams_combined_updated.rds")

### merge # conflicts with dams_grid_panel
for (i in 1:12){
  dams_grid_conflict_panel <- as.data.table(dams_grid_panel) %>%
    dplyr::select(dam_id, cell, year, month, HYBAS_ID, x, y, MAIN_BAS, COAST, within_200km,
                  DEM, downstream_basin, downstream_segment, dist_dams_m, HYRIV_ID, MAIN_RIV,
                  affected_riv, dist_rivers_m, ADM2_CODE, ADM2_NAME, ADM1_CODE, ADM1_NAME,
                  ADM0_CODE, ADM0_NAME, REGION, dn, climates_f, Lithology, GWSTOR_V2,
                  Ethnic_g, num_ethnic_dam, num_ethnic_cell) %>%
    filter(month==i) %>%
    left_join(as.data.table(conflict_num_wide) %>% filter(month==i),
              join_by(cell==cells, year==year, month==month))
  
  #### NA to 0
  dams_grid_conflict_panel <- dams_grid_conflict_panel %>%
    dplyr::mutate_at(vars(num_peaceful_protest:num_output),
                     list(~ ifelse(is.na(.), 0, .)))
  
  #### merge with dams info.
  dams_grid_conflict_panel <- dams_grid_conflict_panel %>%
    left_join(as.data.table(dams_updated) %>% 
                dplyr::select(dam_id, Dam_Year, Dam_Month,`Name of dam`, Country, `Major basin`, `Sub-basin`,
                              `Completed /operational since`, `Dam height (m)`,
                              `Reservoir capacity (million m3)`, `Reservoir area (km2)`,
                               SUB_NAME, treaty_basin, Sign_Year, Sign_Month),
              join_by(dam_id==dam_id))
  
  ### Proximity to Dams as Treatment
  dams_grid_conflict_panel <- dams_grid_conflict_panel %>%
    dplyr::mutate(post = ifelse(year>=`Completed /operational since`, 1, 0),
                  post2 = ifelse(year>Dam_Year|
                                   year==Dam_Year & month>Dam_Month|
                                   is.na(Dam_Month) & year>=Dam_Year, 1, 0),
                  period = as.numeric(year-Dam_Year)) %>%
    mutate(quarter = quarter(make_date(year, month, 1)),
           Dam_Quarter = quarter(make_date(Dam_Year, Dam_Month, 1))) %>%
    mutate(quarter_diff = as.numeric((year - Dam_Year) * 4 + (quarter - Dam_Quarter))) %>%
    mutate(event = ifelse(as.numeric(quarter_diff)>-21 & as.numeric(quarter_diff)<21, 
                          quarter_diff,
                          ifelse(as.numeric(quarter_diff)>=21, '21+', '-21+'))) %>%
    mutate(event_label = relevel(as.factor(event), ref ='-1')) %>%
    mutate(period_label = ifelse(as.numeric(period)>-5 & as.numeric(period)<9, 
                                 period,
                                 ifelse(as.numeric(period)>=9, '9+', '-5+'))) %>%
    mutate(period_label = relevel(as.factor(period_label), ref ='-1')) %>%
    dplyr::mutate(ring = ifelse(dist_dams_m<=20000, 'R20',
                                ifelse(dist_dams_m>20000 & dist_dams_m<=40000, 'R20_40',
                                       ifelse(dist_dams_m>40000 & dist_dams_m<=60000, 'R40_60',
                                              ifelse(dist_dams_m>60000 & dist_dams_m<=80000, 'R60_80',
                                                     ifelse(dist_dams_m>80000 & dist_dams_m<=100000, 'R80_100',
                                                            ifelse(dist_dams_m>100000 & dist_dams_m<=120000, 'R100_120',
                                                                   ifelse(dist_dams_m>120000 & dist_dams_m<=140000, 'R120_140',
                                                                          ifelse(dist_dams_m>140000 & dist_dams_m<=160000, 'R140_160',
                                                                                 ifelse(dist_dams_m>160000 & dist_dams_m<=180000, 'R160_180','Control'))))))))),
                  ring2 = ifelse(dist_dams_m<=50000, 'R50',
                                 ifelse(dist_dams_m>50000 & dist_dams_m<=100000, 'R50_100', 
                                        ifelse(dist_dams_m>100000 & dist_dams_m<=150000, 'R100_150','Control'))),
                  Treat = ifelse(dist_dams_m<=50000, 1, 0),
                  Treat2 = ifelse(dist_dams_m<=100000, 1, 0)
    )
    
  ## Save as RDS
  saveRDS(dams_grid_conflict_panel, file = paste("Datasets/dams_grid_conflict_panel", i, ".rds", sep=''))
  
}


