rstudioapi::getActiveDocumentContext
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

rm(list=ls())

library(sf)
library(readxl)
#library(haven)
library(raster)
library(terra)
library(ggplot2)
library(mapview)
library(dplyr)
library(exactextractr)
library(foreach)
library(doParallel)
library(units)

# Unify projections
crs.geo <- sf::st_crs(4326)
sf_use_s2(FALSE)

############################################################################################
############################## Import GEO Data #############################################
############################################################################################

## 1. Dam
dams <- read_xlsx('Dams/Dam_data_20240403.xlsx', sheet=1, range = "A2:S91")
### to shp file
dams <- st_as_sf(x = dams, coords = c("Decimal degree longitude", "Decimal degree latitude"),
                 crs = crs.geo, remove = F)

## 2. River Basins
### Hydrobasins level 10
basins <- st_read('Hydro Data/hybas_af_lev01-12_v1c/hybas_af_lev10_v1c.shp')
basins <- st_transform(basins, st_crs(dams))

## 3. Treaty River Basins
basins_name <- st_read('Hydro Data/hydrobasins_africa/hydrobasins_africa.shp')
treaty_basins <- read.csv('Hydro Data/Treaty_Basin/Treaty_Basin.csv')
basins_name <- basins_name %>%
  mutate(treaty_basin = ifelse(SUB_NAME %in% treaty_basins$Basin_Name |
                                 MAJ_NAME %in% treaty_basins$Basin_Name, 1, 0))

### merge to add the sign_year/sign_month cols.
basins_name <- merge(basins_name, treaty_basins, by.x = 'MAJ_NAME', by.y = 'Basin_Name', all.x = T)
basins_name <- merge(basins_name, treaty_basins, by.x = 'SUB_NAME', by.y = 'Basin_Name', all.x = T, suffix = c('', '.x'))
basins_name <- basins_name %>%
  dplyr::mutate(Sign_Year = ifelse(is.na(Sign_Year), Sign_Year.x, Sign_Year),
                Sign_Month = ifelse(is.na(Sign_Month), Sign_Month.x, Sign_Month)) %>%
  dplyr::select(., -Sign_Year.x, -Sign_Month.x)

## 4. River
### load rivers data
rivers <- st_read('Hydro Data/HydroRIVERS_v10_af_shp/HydroRIVERS_v10_af_shp/HydroRIVERS_v10_af.shp')
rivers <- st_transform(rivers, st_crs(dams))



############################################################################################
############################## Combine Data ################################################
############################################################################################

## 5. Match dams buffer with river basins
### find dams' located river basins and st_join
dams <- dams %>%
  st_join(basins[,c(1,4)]) %>%
  filter(!is.na(HYBAS_ID)) %>%
  st_join(basins_name[,c(1, 9, 10, 11)])
dams$dam_id <- 1:nrow(dams)
dams_buffer200 <- st_buffer(dams, dist = 2)

#saveRDS(dams, file = "Datasets/dams_combined.rds")

### 6. Identify upstream/downstream river basins
### elevation raster
af_dem <- raster('Africa SRTM/AF_DEM.tif')

### identify upstream/downstream river basins
registerDoParallel(cl <- makeCluster(8))

dams_basins <- foreach(i = 1:nrow(dams), .combine = 'rbind', .packages = c('dplyr', 'sf', 'exactextractr', 'raster')) %dopar% {
  sf_use_s2(FALSE)
  ### choose large basins (river basins sharing the same downstream basins)
  basins_large <- basins[basins$MAIN_BAS==dams$MAIN_BAS[i], ]
  ### intersect to regions that within dams_buffer100
  basins_large_inters <- basins_large %>%
    st_intersects(st_sf(st_union(dams_buffer200[i,])))
  ### change row index
  rownames(basins_large) <- 1:nrow(basins_large)
  ### identify the intersection with 200km buffer
  rowindex <- t(basins_large_inters)[[1]]
  basins_large$within_200km <- ifelse(rownames(basins_large) %in% rowindex, 1, 0)
  ### keep intersection
  basins_large_200km <- basins_large %>% 
    dplyr::filter(within_200km==1)
  basins_large_200km$dam_id <- dams$dam_id[i]
  ### extract elevation and re-combine
  basins_large_dem <- exact_extract(af_dem, basins_large_200km, fun="mean", include_cell = F, force_df=T)
  names(basins_large_dem) <- 'DEM'
  basins_large_200km <- cbind(basins_large_200km, basins_large_dem)
  ### the elevation of the dam's located river basin 
  dam_DEM <- basins_large_200km$DEM[basins_large_200km$HYBAS_ID==dams$HYBAS_ID[i]]
  ### identify upstream/downstream river basins
  basins_large_200km <- basins_large_200km %>%
    dplyr::mutate(downstream_basin = ifelse(DEM<=dam_DEM, 1, 0))
  basins_large_200km
}

stopCluster(cl)

### calculate the area of river basins
dams_basins <- dams_basins %>%
  dplyr::mutate(AREA_km2 = set_units(st_area(dams_basins), km^2))
attributes(dams_basins$AREA_km2) <- NULL
dams_basins <- dams_basins %>%
  dplyr::group_by(dam_id, downstream_basin) %>%
  dplyr::mutate(AREA_buffer_km2 = sum(AREA_km2))

## Save as RDS
#saveRDS(dams_basins, file = "Datasets/dams_basins.rds")


#dams_basins_aggr <- dams_basins %>%
#  dplyr::group_by(dam_id) %>%
#  dplyr::summarise() %>%
#  st_as_sf()

dams_basins_aggr <- dams_basins %>%
  dplyr::group_by(dam_id, downstream_basin) %>%
  dplyr::summarise() %>%
  st_as_sf()

## 7. Identify river connection and up/downstream
### Stem Rivers
rivers_stem <- rivers[rivers$ORD_CLAS==1 | rivers$ORD_FLOW<=4,] %>%
  st_join(dams_basins_aggr) %>%
  filter(!is.na(dam_id)) #%>%
#group_by(dam_id) %>%
#dplyr::summarise() %>%
#st_as_sf()


registerDoParallel(cl <- makeCluster(8))
stemrivers_near_dams <- foreach(i = 1:nrow(dams), .combine = 'rbind', .packages = c('dplyr', 'sf')) %dopar% {
  sf_use_s2(FALSE)
  ### choose rivers in basins (dam located basins)
  rivers_cluster <- rivers_stem[rivers_stem$dam_id==i,]
  ### find the nearest river to the dams
  rivers_nearest <- dams[dams$dam_id==i,] %>%
    st_join(rivers_cluster[,-15], join = st_nearest_feature) %>%
    distinct()
  ### river clusters of the nearest river
  rivers_cluster <- rivers_cluster %>%
    dplyr::mutate(nearest_riv = rivers_nearest$HYRIV_ID) #%>%
    #filter(MAIN_RIV==rivers_nearest$MAIN_RIV)
  
  rivers_cluster
}

stopCluster(cl)


### Find the River Passing by the Dams

library(igraph)

stemrivers_affected_dams <- data.frame() 

for (x in 1:nrow(dams)){
  # select dams
  df <- stemrivers_near_dams[stemrivers_near_dams$dam_id==dams$dam_id[x],]
  if (nrow(df)!=0){
    # create a dataframe containing two columns
    segments <- data.frame(segment_id = df$HYRIV_ID,
                           next_segment_id = df$NEXT_DOWN)
    
    # create a graph object from the data frame
    graph <- graph_from_data_frame(segments, directed = T)
    #plot(graph)
    # get the degree of each vertex
    degree <- degree(graph)
    # get the vertices
    vertices <- unique(as.vector(get.edgelist(graph)))
    # get the ends
    ends <- vertices[degree[vertices] == 1]
    # test connectivity of the river segments from each end
    df$river_conn <- NA
    for (j in 1:length(ends)){
      connect_list <- c()
      for (i in 1:nrow(df)){
        if (as.character(df$HYRIV_ID[i])!= ends[j]){
          connect <- edge_connectivity(graph, source = ends[j], 
                                       target = as.character(df$HYRIV_ID[i]), checks = TRUE)
        }
        if (connect==1){
          connect_list <- c(connect_list, as.character(df$HYRIV_ID[i]))
        }
      }
      
      # collect all connected segments (end itself included)
      connect_list <- c(connect_list, ends[j])
      
      # record all connected ends
      df$river_conn <- ifelse(as.character(df$HYRIV_ID) %in% connect_list, 
                              paste(df$river_conn, j, sep = ','), df$river_conn)
    }
    
    # identify the river that passes by the dam
    df <- df %>%
      dplyr::mutate(river_conn = gsub('NA,', '', river_conn)) %>%
      dplyr::mutate(nearest_riv_conn = unique(river_conn[HYRIV_ID==nearest_riv]))
    
    # identify the connectivity between the nearest river segment (to dams) and other segments
    c = as.numeric(strsplit(df$nearest_riv_conn,split=",",fixed=TRUE)[[1]])
    df$affected_riv <- NA
    for (t in 1:nrow(df)){
      d = as.numeric(strsplit(df$river_conn[t],split=",",fixed=TRUE)[[1]])
      df$affected_riv[t] <- ifelse(c %in% d | d %in% c, 1, 0)
    }
    
    # identify downstream/upstream position between the nearest river segment (to dams) and other segments
    df$flow_from_dam <- NA
    for (i in 1:nrow(df)){
      if (as.character(df$HYRIV_ID[i])!= as.character(df$nearest_riv[1])){
        connect_nearest <- edge_connectivity(graph, source = as.character(df$nearest_riv[1]), 
                                             target = as.character(df$HYRIV_ID[i]), checks = TRUE)
        df$flow_from_dam[i] <- connect_nearest
      } else{
        df$flow_from_dam[i] <- 1
      }
    } 
    df$downstream_segment <- ifelse((df$affected_riv==1 & df$flow_from_dam==1)|
                                   (df$affected_riv==0 & df$downstream_basin==1), 1, 0)
  }
  stemrivers_affected_dams <- rbind(stemrivers_affected_dams, df)
}  

stemrivers_affected_dams_buffer10 <- st_buffer(stemrivers_affected_dams, dist = 0.09)
stemrivers_affected_dams_buffer20 <- st_buffer(stemrivers_affected_dams, dist = 0.18)

stemrivers_affected_dams$downstream_segment <- as.factor(stemrivers_affected_dams$downstream_segment)

mapview(dams_basins[dams_basins$dam_id==1,], zcol='downstream_basin', col.regions = c('white', 'dodgerblue'))+
  mapview(dams[dams$dam_id==1,], col.regions = 'red')+
  mapview(stemrivers_affected_dams[stemrivers_affected_dams$dam_id==1,], zcol='downstream_segment',
          color = c('blue', 'darkorange'))

mapview(dams_basins[dams_basins$dam_id==1,], zcol='downstream_basin', col.regions = c('white', 'dodgerblue'))+
  mapview(dams[dams$dam_id==1,], col.regions = 'red')+
  mapview(stemrivers_affected_dams[stemrivers_affected_dams$dam_id==1,], zcol='affected_riv',
          color = c('blue', 'red'))

mapview(dams_basins[dams_basins$dam_id==1,], zcol='downstream_basin', col.regions = c('white', 'dodgerblue'))+
  mapview(dams[dams$dam_id==1,], col.regions = 'red')+
  mapview(stemrivers_affected_dams[stemrivers_affected_dams$dam_id==1,], zcol='affected_riv',
          color = c('blue', 'red'))+
  mapview(stemrivers_affected_dams[stemrivers_affected_dams$dam_id==1,], zcol='downstream_segment',
          color = c('blue', 'darkorange'))



#save.image(file = 'Datasets/Delineate_River.RData')
#load(file = 'Datasets/Delineate_River.RData')






























