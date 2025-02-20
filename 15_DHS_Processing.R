rstudioapi::getActiveDocumentContext
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(sf)
library(haven)
library(readxl)
library(raster)
library(terra)
library(ggplot2)
library(mapview)
library(tidyverse)
library(exactextractr)
library(foreach)
library(doParallel)
library(units)
library(tidyr)
library(lubridate)
library(data.table)

rm(list=ls())

sf_use_s2(FALSE)
setDTthreads(8)

############################################################################################
############################## Village Geo-location ########################################
############################################################################################

## 1. Village shp
village_shp <- readRDS('DHS/village_shp.rds')

## extract to landcover raster
landcover <- raster('Modis_Yearly_Land_Cover/10km/landcover_2001.tif')
vill_grid <- raster::extract(landcover, village_shp, cellnumbers=T, sp=TRUE)
vill_grid <- vill_grid@data
vill_grid$LC_Type1 <- NULL

## 2. Merge village shp with grid cell
## load data
dams_basins_grid <- readRDS(file = "Datasets/dams_basins_grid_notime.rds")
dams_updated <- readRDS(file = "Datasets/dams_combined_updated.rds")
control_vars <- readRDS(file = 'Datasets/control_vars.rds')
landcover_grid <- readRDS(file = 'Datasets/landcover_grid.rds')

## remove time variation of dams_grid_panel
#setDT(dams_grid_panel)
#dams_basins_grid <- dams_grid_panel %>%
#  dplyr::select(dam_id, cell, HYBAS_ID, x, y, MAIN_BAS, COAST, within_200km,
#                DEM, downstream_basin, downstream_segment, dist_dams_m, HYRIV_ID, MAIN_RIV,
#                affected_riv, dist_rivers_m, ADM2_CODE, ADM2_NAME, ADM1_CODE, ADM1_NAME,
#                ADM0_CODE, ADM0_NAME, REGION, dn, climates_f, Lithology, GWSTOR_V2,
#                Ethnic_g, num_ethnic_dam, num_ethnic_cell) %>%
#  unique()

#saveRDS(dams_basins_grid, 'Datasets/dams_basins_grid_notime.rds')



## merge by cell with dams_grid_panel and dams_updated
vill_matched <- merge(as.data.table(vill_grid), dams_basins_grid,
                      by.x = 'cells', by.y = 'cell')

vill_matched <- vill_matched %>%
  left_join(as.data.table(dams_updated) %>% 
              dplyr::select(dam_id, Dam_Year, Dam_Month,`Name of dam`, Country, `Major basin`, `Sub-basin`,
                            `Completed /operational since`, `Dam height (m)`,
                            `Reservoir capacity (million m3)`, `Reservoir area (km2)`,
                            SUB_NAME, treaty_basin, Sign_Year, Sign_Month),
            join_by(dam_id==dam_id))
  

## 3. Household Data
dhs_hh <- read_dta('DHS/dhs_hh.dta')

#### Merge village info. with hh survey
dat_hh <- merge(as.data.table(dhs_hh) %>%
                  filter(interview_year>=1997),
                 as.data.table(vill_matched),
                 by.x = c('hv000', 'hv001', 'phase'),
                 by.y = c('v000', 'DHSCLUST', 'phase'))

dat_hh <- merge(as.data.table(dhs_hh),
                as.data.table(vill_matched),
                by.x = c('hv000', 'hv001', 'phase'),
                by.y = c('v000', 'DHSCLUST', 'phase'))

### Proximity to Dams as Treatment
dat_hh2 <- dat_hh %>%
  left_join(as.data.table(control_vars),
            join_by(cells==cell, interview_year==year, interview_month==month, 
                    dam_id==dam_id, HYBAS_ID==HYBAS_ID)) %>%
  left_join(as.data.table(landcover_grid),
            join_by(cells==cell, dam_id==dam_id, HYBAS_ID==HYBAS_ID, 
                    interview_year == year)) %>%
  dplyr::arrange(cells, dam_id, interview_year, .by_group=T) %>%
  fill(., landcover, cropland_dummy, .direction = 'up') %>%
  dplyr::mutate(post = ifelse(interview_year>=`Completed /operational since`, 1, 0),
                post2 = ifelse(interview_year>Dam_Year|
                                 interview_year==Dam_Year & interview_month>Dam_Month|
                                 is.na(Dam_Month) & interview_year>=Dam_Year, 1, 0),
                period = as.numeric(interview_year-Dam_Year)) %>%
  mutate(quarter = quarter(make_date(interview_year, interview_month, 1)),
         Dam_Quarter = quarter(make_date(Dam_Year, Dam_Month, 1))) %>%
  mutate(quarter_diff = as.numeric((interview_year - Dam_Year) * 4 + (quarter - Dam_Quarter))) %>%
  mutate(event = ifelse(as.numeric(quarter_diff)>-21 & as.numeric(quarter_diff)<21, 
                        quarter_diff,
                        ifelse(as.numeric(quarter_diff)>=21, '21+', '-21+'))) %>%
  #mutate(event_label = relevel(as.factor(event), ref ='-1')) %>%
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

### paired river basins
dat_hh2 <- dat_hh2 %>%
  dplyr::group_by(dam_id) %>%
  dplyr::mutate(paired = ifelse(any(affected_riv==0), 1, 0))

dat_hh2 <- dat_hh2 %>%
  mutate(landown_hectares_adj=ifelse(landown_hectares==98, NA, landown_hectares)) %>%
  mutate(landown_hectares_adj=ifelse(is.na(landown_hectares_adj) & landown_agri==0, 0, landown_hectares_adj))

summary(dat_hh2$landown_hectares_adj)

## 4. Women Data
dhs_women <- read_dta('DHS/dhs_women.dta')

#### Merge village info. with women survey
dat_women <- merge(as.data.table(dhs_women[dhs_women$interview_year>=1997,]),
                 as.data.table(vill_matched),
                 by.x = c('v000', 'v001', 'phase'),
                 by.y = c('v000', 'DHSCLUST', 'phase'))

### Proximity to Dams as Treatment
dat_women2 <- dat_women %>%
  left_join(as.data.table(control_vars),
            join_by(cells==cell, interview_year==year, interview_month==month, 
                    dam_id==dam_id, HYBAS_ID==HYBAS_ID)) %>%
  left_join(as.data.table(landcover_grid),
            join_by(cells==cell, dam_id==dam_id, HYBAS_ID==HYBAS_ID, 
                    interview_year == year)) %>%
  dplyr::arrange(cells, dam_id, interview_year, .by_group=T) %>%
  fill(., landcover, cropland_dummy, .direction = 'up') %>%
  dplyr::mutate(post = ifelse(interview_year>=`Completed /operational since`, 1, 0),
                post2 = ifelse(interview_year>Dam_Year|
                                 interview_year==Dam_Year & interview_month>Dam_Month|
                                 is.na(Dam_Month) & interview_year>=Dam_Year, 1, 0),
                period = as.numeric(interview_year-Dam_Year)) %>%
  mutate(quarter = quarter(make_date(interview_year, interview_month, 1)),
         Dam_Quarter = quarter(make_date(Dam_Year, Dam_Month, 1))) %>%
  mutate(quarter_diff = as.numeric((interview_year - Dam_Year) * 4 + (quarter - Dam_Quarter))) %>%
  mutate(event = ifelse(as.numeric(quarter_diff)>-21 & as.numeric(quarter_diff)<21, 
                        quarter_diff,
                        ifelse(as.numeric(quarter_diff)>=21, '21+', '-21+'))) %>%
  #mutate(event_label = relevel(as.factor(event), ref ='-1')) %>%
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

### paired river basins
dat_women2 <- dat_women2 %>%
  dplyr::group_by(dam_id) %>%
  dplyr::mutate(paired = ifelse(any(affected_riv==0), 1, 0))


dat_women2 <- dat_women2 %>%
  dplyr::mutate(lnsch = log(schooling+1),
                lnwater = log(water_time_adj+1),
                childmarr = ifelse((is.na(age_marr1) & age>17)|age_marr1>17,0,1),
                current_work = ifelse(occupation==0|is.na(occupation), 0, 1),
                agric_work = ifelse(is.na(occupation), 0,
                                    ifelse(occupation %in% c(4, 5), 1, 0)),
                domestic = ifelse(is.na(occupation), 0,
                                  ifelse(occupation %in% c(6), 1, 0)),
                off_farm = ifelse(is.na(occupation), 0,
                                  ifelse(occupation %in% c(1:3, 7:9), 1, 0)),
                current_work_husband = ifelse(occupation_husband==0|is.na(occupation_husband), 0, 1),
                agric_work_husband = ifelse(is.na(occupation_husband), 0,
                                    ifelse(occupation_husband %in% c(4, 5), 1, 0)),
                domestic_husband = ifelse(is.na(occupation_husband), 0,
                                  ifelse(occupation_husband %in% c(6), 1, 0)),
                off_farm_husband = ifelse(is.na(occupation_husband), 0,
                                  ifelse(occupation_husband %in% c(1:3, 7:9), 1, 0)),
                sex = 2,
                age_dam = Dam_Year-birthyear)


## 5. Men Data
dhs_men <- read_dta('DHS/dhs_men.dta')

#### Merge village info. with women survey
dhs_men2 <- merge(as.data.table(dhs_men[dhs_men$interview_year>=1997,]),
                   as.data.table(vill_matched),
                   by.x = c('mv000', 'mv001', 'phase'),
                   by.y = c('v000', 'DHSCLUST', 'phase'))

### Proximity to Dams as Treatment
dat_men <- dhs_men2 %>%
  left_join(as.data.table(control_vars),
            join_by(cells==cell, interview_year==year, interview_month==month, 
                    dam_id==dam_id, HYBAS_ID==HYBAS_ID)) %>%
  left_join(as.data.table(landcover_grid),
            join_by(cells==cell, dam_id==dam_id, HYBAS_ID==HYBAS_ID, 
                    interview_year == year)) %>%
  dplyr::arrange(cells, dam_id, interview_year, .by_group=T) %>%
  fill(., landcover, cropland_dummy, .direction = 'up') %>%
  dplyr::mutate(post = ifelse(interview_year>=`Completed /operational since`, 1, 0),
                post2 = ifelse(interview_year>Dam_Year|
                                 interview_year==Dam_Year & interview_month>Dam_Month|
                                 is.na(Dam_Month) & interview_year>=Dam_Year, 1, 0),
                period = as.numeric(interview_year-Dam_Year)) %>%
  mutate(quarter = quarter(make_date(interview_year, interview_month, 1)),
         Dam_Quarter = quarter(make_date(Dam_Year, Dam_Month, 1))) %>%
  mutate(quarter_diff = as.numeric((interview_year - Dam_Year) * 4 + (quarter - Dam_Quarter))) %>%
  mutate(event = ifelse(as.numeric(quarter_diff)>-21 & as.numeric(quarter_diff)<21, 
                        quarter_diff,
                        ifelse(as.numeric(quarter_diff)>=21, '21+', '-21+'))) %>%
  #mutate(event_label = relevel(as.factor(event), ref ='-1')) %>%
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

### paired river basins
dat_men2 <- dat_men %>%
  dplyr::group_by(dam_id) %>%
  dplyr::mutate(paired = ifelse(any(affected_riv==0), 1, 0))


dat_men2 <- dat_men2 %>%
  dplyr::mutate(lnsch = log(schooling+1),
                current_work = ifelse(occupation==0|is.na(occupation), 0, 1),
                agric_work = ifelse(is.na(occupation), 0,
                                    ifelse(occupation %in% c(4, 5), 1, 0)),
                domestic = ifelse(is.na(occupation), 0,
                                  ifelse(occupation %in% c(6), 1, 0)),
                off_farm = ifelse(is.na(occupation), 0,
                                  ifelse(occupation %in% c(1:3, 7:9), 1, 0)),
                sex = 1,
                age_dam = Dam_Year-birthyear)




############################################################################################
############################## Regression: Survey data #####################################
############################################################################################

library(fixest)
library(texreg)

dict = c('affected_riv' = 'Affected_Riv',
         'post2' = 'Post',
         'cropland_dummy' = 'Cropland',
         'ethnicfrac_cell_high' = 'HighEthnicFrac_Cell',
         'ethnicfrac_dam_high' = 'HighEthnicFrac_Dam',
         'factor(dam_id)' = 'Dam',
         'factor(cell)' = 'Cell',
         'factor(cells)' = 'Cell',
         'factor(year)' = 'Year',
         'factor(month)' = 'Month',
         'factor(interview_year)' = 'Year',
         'factor(time)'='Yearmonth',
         'factor(ADM0_NAME)'='Country')

subset_fml1.1 <- paste0("~ dist_rivers_m <= 30000 & dist_dams_m<=50000 & paired == 1 & Dam_Year %in% c(2001:2020) & 
                            downstream_segment==1")
subset_fml1.2 <- paste0("~ dist_rivers_m <= 30000 & dist_dams_m<=50000 & paired == 1 & Dam_Year %in% c(2001:2020) & 
                            downstream_segment==0")

### Water time
result1.1 <- feols(log(water_time_adj+1) ~ post2+affected_riv:post2+hh_num+women_num+men_num+
                     children_num+electricity+num_bedrooms+factor(toilet_type)+factor(urban_rural)+
                     factor(wealth)+factor(material_floor_adj)+factor(source_drinkwater_adj)+
                     +factor(location_watersource)+log(PopCount+1)+log(rainfall+1)|
                     factor(dam_id)+factor(cells)+factor(ADM0_NAME)^factor(time),
                   data = dat_hh2, 
                   subset = as.formula(subset_fml1.1),
                   cluster = ~ DHSID,
                   nthreads = 12)

result1.2 <- feols(log(water_time_adj) ~ affected_riv*post2+hh_num+women_num+men_num+
                     children_num+electricity+num_bedrooms+factor(toilet_type)+factor(urban_rural)+
                     factor(wealth)+factor(material_floor_adj)+factor(source_drinkwater_adj)+
                     factor(location_watersource)+log(PopCount+1)+log(rainfall+1)|
                     factor(dam_id)+factor(cells)+factor(ADM0_NAME)^factor(time),
                   data = dat_hh2, 
                   subset = as.formula(subset_fml1.1),
                   cluster = ~ cells,
                   nthreads = 12)


result1.3 <- feols(I(water_time_adj>0) ~ post2+affected_riv:post2+hh_num+women_num+men_num+
                     children_num+electricity+num_bedrooms+factor(toilet_type)+factor(urban_rural)+
                     factor(wealth)+factor(material_floor_adj)+factor(source_drinkwater_adj)+
                     +factor(location_watersource)+log(PopCount+1)+log(rainfall+1)|
                     factor(dam_id)+factor(cells)+factor(ADM0_NAME)^factor(time),
                   data = dat_hh2, 
                   subset = as.formula(subset_fml1.1),
                   cluster = ~ DHSID,
                   nthreads = 12)

result1.4 <- feols(log(water_time_adj+1) ~ post2+affected_riv:post2+hh_num+women_num+men_num+
                     children_num+electricity+num_bedrooms+factor(toilet_type)+factor(urban_rural)+
                     factor(wealth)+factor(material_floor_adj)+factor(source_drinkwater_adj)+
                     +factor(location_watersource)+log(PopCount+1)+log(rainfall+1)|
                     factor(dam_id)+factor(cells)+factor(ADM0_NAME)^factor(time),
                   data = dat_hh2, 
                   subset = as.formula(subset_fml1.2),
                   cluster = ~ DHSID,
                   nthreads = 12)

result1.5 <- feols(log(water_time_adj) ~ post2+affected_riv:post2+hh_num+women_num+men_num+
                     children_num+electricity+num_bedrooms+factor(toilet_type)+factor(urban_rural)+
                     factor(wealth)+factor(material_floor_adj)+factor(source_drinkwater_adj)+
                     +factor(location_watersource)+log(PopCount+1)+log(rainfall+1)|
                     factor(dam_id)+factor(cells)+factor(ADM0_NAME)^factor(time),
                   data = dat_hh2, 
                   subset = as.formula(subset_fml1.2),
                   cluster = ~ DHSID,
                   nthreads = 12)

result1.6 <- feols(I(water_time_adj>0) ~ post2+affected_riv:post2+hh_num+women_num+men_num+
                     children_num+electricity+num_bedrooms+factor(toilet_type)+factor(urban_rural)+
                     factor(wealth)+factor(material_floor_adj)+factor(source_drinkwater_adj)+
                     +factor(location_watersource)+log(PopCount+1)+log(rainfall+1)|
                     factor(dam_id)+factor(cells)+factor(ADM0_NAME)^factor(time),
                   data = dat_hh2, 
                   subset = as.formula(subset_fml1.2),
                   cluster = ~ DHSID,
                   nthreads = 12)

attach(dat_hh2)
conditions <- dist_rivers_m <= 30000 & dist_dams_m <= 60000 & paired == 1 & Dam_Year %in% c(2001:2020)
mean1 <- round(mean(log(water_time_adj[conditions & downstream_segment==1]+1), na.rm=T),3)
mean2 <- round(mean(log(water_time_adj[conditions & downstream_segment==1 & water_time_adj>0]), na.rm=T),3)
mean3 <- round(mean(I(water_time_adj[conditions & downstream_segment==1]>0), na.rm=T),3)
mean4 <- round(mean(log(water_time_adj[conditions & downstream_segment==0]+1), na.rm=T),3)
mean5 <- round(mean(log(water_time_adj[conditions & downstream_segment==0 & water_time_adj>0]), na.rm=T),3)
mean6 <- round(mean(I(water_time_adj[conditions & downstream_segment==0]>0), na.rm=T),3)

etable(result1.1, result1.2, result1.3, result1.4, result1.5, result1.6,
       title = 'The Effect of Hydropower Dams on Water Use', 
       headers = list("Downstream"=3, "Upstream"=3),
       keep = '%affected_riv|post2|ring2|ethnicfrac_cell_high|ethnicfrac_dam_high',
       order = c("Post"), dict = dict,
       extralines = list('_^Mean of Dep. Var.'=c(mean1,mean2,mean3,mean4,
                                                 mean5,mean6),
                         '_Covariates' = c('Y','Y','Y','Y','Y','Y')),
       fitstat= ~ n+r2, depvar = T, placement = 'h', tpt=T, adjustbox=1,
       page.width = "fit", tex = F, style.tex = style.tex("aer"), 
       digits = 3, digits.stats = 3)

## regression
etable(result1.1, result1.2, result1.3, result1.4, result1.5, result1.6,
       title = 'The Effect of Hydropower Dams on Water Use', 
       headers = list("Downstream"=3, "Upstream"=3),
       keep = '%affected_riv|post2|ring2|ethnicfrac_cell_high|ethnicfrac_dam_high',
       order = c("Post"), dict = dict,
       extralines = list('_^Mean of Dep. Var.'=c(mean1,mean2,mean3,mean4,
                                                 mean5,mean6),
                         '_Covariates' = c('Y','Y','Y','Y','Y','Y')),
       fitstat= ~ n+r2, depvar = T, placement = 'h', tpt=T, adjustbox=1,
       page.width = "fit", tex = T, style.tex = style.tex("aer"), 
       digits = 3, digits.stats = 3,
       label = paste('tab_water_time_30000'), replace = T,
       file = paste0('Latex Table/241225/water_time_30000.tex'))


### Wealth
#### Downstream
result1.1 <- feols(landown_agri ~ post2+affected_riv:post2+hh_num+women_num+men_num+
                     children_num+log(PopCount+1)+log(rainfall+1)|
                     factor(dam_id)+factor(cells)+factor(ADM0_NAME)^factor(time),
                   data = dat_hh2, 
                   subset = as.formula(subset_fml1.1),
                   cluster = ~ DHSID,
                   nthreads = 12)

result1.2 <- feols(landown_hectares_adj ~ post2+affected_riv:post2+hh_num+women_num+men_num+
                     children_num+log(PopCount+1)+log(rainfall+1)|
                     factor(dam_id)+factor(cells)+factor(ADM0_NAME)^factor(time),
                   data = dat_hh2, 
                   subset = as.formula(subset_fml1.1),
                   cluster = ~ DHSID,
                   nthreads = 12)

result1.3 <- feols(wealth ~ post2+affected_riv:post2+hh_num+women_num+men_num+
                     children_num+log(PopCount+1)+log(rainfall+1)|
                     factor(dam_id)+factor(cells)+factor(ADM0_NAME)^factor(time),
                   data = dat_hh2, 
                   subset = as.formula(subset_fml1.1),
                   split = ~landown_agri,
                   cluster = ~ DHSID,
                   nthreads = 12)

result1.4 <- feols(electricity ~ post2+affected_riv:post2+hh_num+women_num+men_num+
                     children_num+factor(wealth)+log(PopCount+1)+log(rainfall+1)|
                     factor(dam_id)+factor(cells)+factor(ADM0_NAME)^factor(time),
                   data = dat_hh2, 
                   subset = as.formula(subset_fml1.1),
                   #split = ~landown_agri,
                   cluster = ~ DHSID,
                   nthreads = 12)

result1.5 <- feols(refrigerator ~ post2+affected_riv:post2+hh_num+women_num+men_num+
                     children_num+factor(wealth)+log(PopCount+1)+log(rainfall+1)|
                     factor(dam_id)+factor(cells)+factor(ADM0_NAME)^factor(time),
                   data = dat_hh2, 
                   subset = as.formula(subset_fml1.1),
                   #split = ~landown_agri,
                   cluster = ~ DHSID,
                   nthreads = 12)

result1.6 <- feols(radio ~ post2+affected_riv:post2+hh_num+women_num+men_num+
                     children_num+factor(wealth)+log(PopCount+1)+log(rainfall+1)|
                     factor(dam_id)+factor(cells)+factor(ADM0_NAME)^factor(time),
                   data = dat_hh2, 
                   subset = as.formula(subset_fml1.1),
                   #split = ~landown_agri,
                   cluster = ~ DHSID,
                   nthreads = 12)

result1.7 <- feols(television ~ post2+affected_riv:post2+hh_num+women_num+men_num+
                     children_num+factor(wealth)+log(PopCount+1)+log(rainfall+1)|
                     factor(dam_id)+factor(cells)+factor(ADM0_NAME)^factor(time),
                   data = dat_hh2, 
                   subset = as.formula(subset_fml1.1),
                   #split = ~landown_agri,
                   cluster = ~ DHSID,
                   nthreads = 12)

result1.8 <- feols(bicycle ~ post2+affected_riv:post2+hh_num+women_num+men_num+
                     children_num+factor(wealth)+log(PopCount+1)+log(rainfall+1)|
                     factor(dam_id)+factor(cells)+factor(ADM0_NAME)^factor(time),
                   data = dat_hh2, 
                   subset = as.formula(subset_fml1.1),
                   #split = ~landown_agri,
                   cluster = ~ DHSID,
                   nthreads = 12)

attach(dat_hh2)
conditions <- dist_rivers_m <= 30000 & dist_dams_m <= 60000 & paired == 1 & Dam_Year %in% c(2001:2020)
mean1 <- round(mean(landown_agri[conditions & downstream_segment==1], na.rm=T),3)
mean2 <- round(mean(landown_hectares_adj[conditions & downstream_segment==1], na.rm=T),3)
mean3 <- round(mean(wealth[conditions & downstream_segment==1 & landown_agri==1], na.rm=T),3)
mean4 <- round(mean(wealth[conditions & downstream_segment==1 & landown_agri==0], na.rm=T),3)
mean5 <- round(mean(electricity[conditions & downstream_segment==1], na.rm=T),3)
mean6 <- round(mean(refrigerator[conditions & downstream_segment==1], na.rm=T),3)
mean7 <- round(mean(radio[conditions & downstream_segment==1], na.rm=T),3)
mean8 <- round(mean(television[conditions & downstream_segment==1], na.rm=T),3)
mean9 <- round(mean(bicycle[conditions & downstream_segment==1], na.rm=T),3)

## regression
etable(result1.1, result1.2, result1.3, result1.4, result1.5, 
       result1.6, result1.7, result1.8, 
       title = 'The Effect of Hydropower Dams on Wealth (Downstream)', 
       #headers = list("Downstream"=3, "Upstream"=3),
       keep = '%affected_riv|post2|ring2|ethnicfrac_cell_high|ethnicfrac_dam_high',
       order = c("Post"), dict = dict,
       extralines = list('_^Mean of Dep. Var.'=c(mean1,mean2,mean3,mean4,
                                                 mean5,mean6,mean7,mean8,mean9),
                         '_Covariates' = c('Y','Y','Y','Y','Y','Y','Y','Y','Y')),
       fitstat= ~ n+r2, depvar = T, placement = 'h', tpt=T, adjustbox=1,
       page.width = "fit", tex = T, style.tex = style.tex("aer"), 
       digits = 3, digits.stats = 3,
       label = paste('tab_wealth_downstream_30000'), replace = T,
       file = paste0('Latex Table/241225/wealth_downstream_30000.tex'))



#### Upstream
result1.1 <- feols(landown_agri ~ post2+affected_riv:post2+hh_num+women_num+men_num+
                     children_num+log(PopCount+1)+log(rainfall+1)|
                     factor(dam_id)+factor(cells)+factor(ADM0_NAME)^factor(time),
                   data = dat_hh2, 
                   subset = as.formula(subset_fml1.2),
                   cluster = ~ DHSID,
                   nthreads = 12)

result1.2 <- feols(landown_hectares_adj ~ post2+affected_riv:post2+hh_num+women_num+men_num+
                     children_num+log(PopCount+1)+log(rainfall+1)|
                     factor(dam_id)+factor(cells)+factor(ADM0_NAME)^factor(time),
                   data = dat_hh2, 
                   subset = as.formula(subset_fml1.2),
                   cluster = ~ DHSID,
                   nthreads = 12)

result1.3 <- feols(wealth ~ post2+affected_riv:post2+hh_num+women_num+men_num+
                     children_num+log(PopCount+1)+log(rainfall+1)|
                     factor(dam_id)+factor(cells)+factor(ADM0_NAME)^factor(time),
                   data = dat_hh2, 
                   subset = as.formula(subset_fml1.2),
                   split = ~landown_agri,
                   cluster = ~ DHSID,
                   nthreads = 12)

result1.4 <- feols(electricity ~ post2+affected_riv:post2+hh_num+women_num+men_num+
                     children_num+factor(wealth)+log(PopCount+1)+log(rainfall+1)|
                     factor(dam_id)+factor(cells)+factor(ADM0_NAME)^factor(time),
                   data = dat_hh2, 
                   subset = as.formula(subset_fml1.2),
                   #split = ~landown_agri,
                   cluster = ~ DHSID,
                   nthreads = 12)

result1.5 <- feols(refrigerator ~ post2+affected_riv:post2+hh_num+women_num+men_num+
                     children_num+factor(wealth)+log(PopCount+1)+log(rainfall+1)|
                     factor(dam_id)+factor(cells)+factor(ADM0_NAME)^factor(time),
                   data = dat_hh2, 
                   subset = as.formula(subset_fml1.2),
                   #split = ~landown_agri,
                   cluster = ~ DHSID,
                   nthreads = 12)

result1.6 <- feols(radio ~ post2+affected_riv:post2+hh_num+women_num+men_num+
                     children_num+factor(wealth)+log(PopCount+1)+log(rainfall+1)|
                     factor(dam_id)+factor(cells)+factor(ADM0_NAME)^factor(time),
                   data = dat_hh2, 
                   subset = as.formula(subset_fml1.2),
                   #split = ~landown_agri,
                   cluster = ~ DHSID,
                   nthreads = 12)

result1.7 <- feols(television ~ post2+affected_riv:post2+hh_num+women_num+men_num+
                     children_num+factor(wealth)+log(PopCount+1)+log(rainfall+1)|
                     factor(dam_id)+factor(cells)+factor(ADM0_NAME)^factor(time),
                   data = dat_hh2, 
                   subset = as.formula(subset_fml1.2),
                   #split = ~landown_agri,
                   cluster = ~ DHSID,
                   nthreads = 12)

result1.8 <- feols(bicycle ~ post2+affected_riv:post2+hh_num+women_num+men_num+
                     children_num+factor(wealth)+log(PopCount+1)+log(rainfall+1)|
                     factor(dam_id)+factor(cells)+factor(ADM0_NAME)^factor(time),
                   data = dat_hh2, 
                   subset = as.formula(subset_fml1.2),
                   #split = ~landown_agri,
                   cluster = ~ DHSID,
                   nthreads = 12)

attach(dat_hh2)
conditions <- dist_rivers_m <= 30000 & dist_dams_m <= 60000 & paired == 1 & Dam_Year %in% c(2001:2020) & downstream_segment==0
mean1 <- round(mean(landown_agri[conditions], na.rm=T),3)
mean2 <- round(mean(landown_hectares_adj[conditions], na.rm=T),3)
mean3 <- round(mean(wealth[conditions & landown_agri==1], na.rm=T),3)
mean4 <- round(mean(wealth[conditions & landown_agri==0], na.rm=T),3)
mean5 <- round(mean(electricity[conditions], na.rm=T),3)
mean6 <- round(mean(refrigerator[conditions], na.rm=T),3)
mean7 <- round(mean(radio[conditions], na.rm=T),3)
mean8 <- round(mean(television[conditions], na.rm=T),3)
mean9 <- round(mean(bicycle[conditions], na.rm=T),3)

## regression
etable(result1.1, result1.2, result1.3[[1]], result1.3[[2]], result1.4, result1.5, 
       result1.6, result1.7, result1.8, 
       title = 'The Effect of Hydropower Dams on Wealth (Upstream)', 
       #headers = list("Downstream"=3, "Upstream"=3),
       keep = '%affected_riv|post2|ring2|ethnicfrac_cell_high|ethnicfrac_dam_high',
       order = c("Post"), dict = dict,
       extralines = list('_^Mean of Dep. Var.'=c(mean1,mean2,mean3,mean4,
                                                 mean5,mean6,mean7,mean8,mean9),
                         '_Covariates' = c('Y','Y','Y','Y','Y','Y','Y','Y','Y')),
       fitstat= ~ n+r2, depvar = T, placement = 'h', tpt=T, adjustbox=1,
       page.width = "fit", tex = T, style.tex = style.tex("aer"), 
       digits = 3, digits.stats = 3,
       label = paste('tab_wealth_upstream_30000'), replace = T,
       file = paste0('Latex Table/241225/wealth_upstream_30000.tex'))


### Labor Participation
subset_fml1.1 <- paste0("~ dist_rivers_m <= 30000 & dist_dams_m <= 70000 & paired == 1 & Dam_Year %in% c(2001:2020) & 
                            downstream_segment==1")
subset_fml1.2 <- paste0("~ dist_rivers_m <= 30000 & dist_dams_m <= 70000 & paired == 1 & Dam_Year %in% c(2001:2020) & 
                            downstream_segment==0")

dat_work_women <- dat_women2 %>%
  dplyr::select(affected_riv, post2, age, maritus, religion, ethnicity, schooling, 
                hh_num, children_num, climates_f, Lithology, GWSTOR_V2,
                dist_dams_m, dist_rivers_m, PopCount, rainfall, TempMax, TempMin,
                dam_id, cells, ADM0_NAME, interview_year, DHSID, cropland_dummy,
                agric_work, off_farm, current_work, domestic, landown, Treat, Treat2, ring,
                ring2, paired, Dam_Year, downstream_segment, time) %>%
  dplyr::mutate(sex=2)

dat_work_men <- dat_men2 %>%
  dplyr::select(affected_riv, post2, age, maritus, religion, ethnicity, schooling, 
                hh_num, children_num, climates_f, Lithology, GWSTOR_V2,
                dist_dams_m, dist_rivers_m, PopCount, rainfall, TempMax, TempMin,
                dam_id, cells, ADM0_NAME, interview_year, DHSID, cropland_dummy,
                agric_work, off_farm, current_work, domestic, landown, Treat, 
                Treat2, ring,ring2, paired, Dam_Year, downstream_segment, time) %>%
  dplyr::mutate(sex=1)

dat_work <- rbind(dat_work_women, dat_work_men)


result1.1 <- feols(current_work ~ affected_riv*post2+sex+age+factor(maritus)+
                     factor(religion)+factor(ethnicity)+schooling+hh_num+
                     children_num+factor(climates_f)+
                     factor(Lithology)+factor(GWSTOR_V2)+
                     log(dist_dams_m)+log(dist_rivers_m)+log(PopCount+1)+log(rainfall+1)+
                     TempMax+TempMin|
                     factor(dam_id)+factor(cells)+factor(ADM0_NAME)^factor(interview_year),
                   data = dat_work, 
                   subset = as.formula(subset_fml1.1),
                   cluster = ~ cells,
                   nthreads = 12)


result1.2 <- feols(agric_work ~ affected_riv*post2+sex+age+factor(maritus)+
                     factor(religion)+factor(ethnicity)+schooling+hh_num+
                     children_num+factor(climates_f)+
                     factor(Lithology)+factor(GWSTOR_V2)+
                     log(dist_dams_m)+log(dist_rivers_m)+log(PopCount+1)+log(rainfall+1)+
                     TempMax+TempMin|
                     factor(dam_id)+factor(cells)+factor(ADM0_NAME)^factor(interview_year),
                   data = dat_work, 
                   subset = as.formula(subset_fml1.1),
                   cluster = ~ cells,
                   nthreads = 12)

result1.3 <- feols(off_farm ~ affected_riv*post2+sex+age+factor(maritus)+
                     factor(religion)+factor(ethnicity)+schooling+hh_num+
                     children_num+factor(climates_f)+
                     factor(Lithology)+factor(GWSTOR_V2)+
                     log(dist_dams_m)+log(dist_rivers_m)+log(PopCount+1)+log(rainfall+1)+
                     TempMax+TempMin|
                     factor(dam_id)+factor(cells)+factor(ADM0_NAME)^factor(interview_year),
                   data = dat_work, 
                   subset = as.formula(subset_fml1.1),
                   cluster = ~ cells,
                   nthreads = 12)

result1.4 <- feols(domestic ~ affected_riv*post2+sex+age+factor(maritus)+
                     factor(religion)+factor(ethnicity)+schooling+hh_num+
                     children_num+factor(climates_f)+
                     factor(Lithology)+factor(GWSTOR_V2)+
                     log(dist_dams_m)+log(dist_rivers_m)+log(PopCount+1)+log(rainfall+1)+
                     TempMax+TempMin|
                     factor(dam_id)+factor(cells)+factor(ADM0_NAME)^factor(interview_year),
                   data = dat_work, 
                   subset = as.formula(subset_fml1.1),
                   cluster = ~ cells,
                   nthreads = 12)


result1.5 <- feols(current_work ~ affected_riv*post2+sex+age+factor(maritus)+
                     factor(religion)+factor(ethnicity)+schooling+hh_num+
                     children_num+factor(climates_f)+
                     factor(Lithology)+factor(GWSTOR_V2)+
                     log(dist_dams_m)+log(dist_rivers_m)+log(PopCount+1)+log(rainfall+1)+
                     TempMax+TempMin|
                     factor(dam_id)+factor(cells)+factor(ADM0_NAME)^factor(interview_year),
                   data = dat_work, 
                   subset = as.formula(subset_fml1.2),
                   cluster = ~ cells,
                   nthreads = 12)


result1.6 <- feols(agric_work ~ affected_riv*post2+sex+age+factor(maritus)+
                     factor(religion)+factor(ethnicity)+schooling+hh_num+
                     children_num+factor(climates_f)+
                     factor(Lithology)+factor(GWSTOR_V2)+
                     log(dist_dams_m)+log(dist_rivers_m)+log(PopCount+1)+log(rainfall+1)+
                     TempMax+TempMin|
                     factor(dam_id)+factor(cells)+factor(ADM0_NAME)^factor(interview_year),
                   data = dat_work, 
                   subset = as.formula(subset_fml1.2),
                   cluster = ~ cells,
                   nthreads = 12)

result1.7 <- feols(off_farm ~ affected_riv*post2+sex+age+factor(maritus)+
                     factor(religion)+factor(ethnicity)+schooling+hh_num+
                     children_num+factor(climates_f)+
                     factor(Lithology)+factor(GWSTOR_V2)+
                     log(dist_dams_m)+log(dist_rivers_m)+log(PopCount+1)+log(rainfall+1)+
                     TempMax+TempMin|
                     factor(dam_id)+factor(cells)+factor(ADM0_NAME)^factor(interview_year),
                   data = dat_work, 
                   subset = as.formula(subset_fml1.2),
                   cluster = ~ cells,
                   nthreads = 12)

result1.8 <- feols(domestic ~ affected_riv*post2+sex+age+factor(maritus)+
                     factor(religion)+factor(ethnicity)+schooling+hh_num+
                     children_num+factor(climates_f)+
                     factor(Lithology)+factor(GWSTOR_V2)+
                     log(dist_dams_m)+log(dist_rivers_m)+log(PopCount+1)+log(rainfall+1)+
                     TempMax+TempMin|
                     factor(dam_id)+factor(cells)+factor(ADM0_NAME)^factor(interview_year),
                   data = dat_work, 
                   subset = as.formula(subset_fml1.2),
                   cluster = ~ cells,
                   nthreads = 12)


attach(dat_work)
conditions <- dist_rivers_m <= 30000 & dist_dams_m <= 70000 & paired == 1 & Dam_Year %in% c(2001:2020)
mean1 <- round(mean(current_work[conditions & downstream_segment==1], na.rm=T),3)
mean2 <- round(mean(agric_work[conditions & downstream_segment==1], na.rm=T),3)
mean3 <- round(mean(off_farm[conditions & downstream_segment==1], na.rm=T),3)
mean4 <- round(mean(domestic[conditions & downstream_segment==1], na.rm=T),3)
mean5 <- round(mean(current_work[conditions & downstream_segment==0], na.rm=T),3)
mean6 <- round(mean(agric_work[conditions & downstream_segment==0], na.rm=T),3)
mean7 <- round(mean(off_farm[conditions & downstream_segment==0], na.rm=T),3)
mean8 <- round(mean(domestic[conditions & downstream_segment==0], na.rm=T),3)

## regression
etable(result1.1, result1.2, result1.3, result1.4, result1.5, 
       result1.6, result1.7, result1.8, 
       title = 'The Effect of Hydropower Dams on Wealth (Downstream)', 
       headers = list("Downstream"=4, "Upstream"=4),
       keep = '%affected_riv|post2|ring2|ethnicfrac_cell_high|ethnicfrac_dam_high',
       order = c("Post"), dict = dict,
       extralines = list('_^Mean of Dep. Var.'=c(mean1,mean2,mean3,mean4,
                                                 mean5,mean6,mean7,mean8),
                         '_Covariates' = c('Y','Y','Y','Y','Y','Y','Y','Y')),
       fitstat= ~ n+r2, depvar = T, placement = 'h', tpt=T, adjustbox=1,
       page.width = "fit", tex = T, style.tex = style.tex("aer"), 
       digits = 3, digits.stats = 3,
       label = paste('tab_labor_30000'), replace = T,
       file = paste0('Latex Table/241225/labor_30000.tex'))











