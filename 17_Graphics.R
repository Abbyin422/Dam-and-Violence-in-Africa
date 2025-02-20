rstudioapi::getActiveDocumentContext
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(tidyverse)
library(dplyr)
library(data.table)
library(sf)
library(parallel)
library(haven)
library(ggplot2)
library(ggpubr)
library(ggspatial)
library(ggpattern)
library(magick)
library(mapview)
library(rosm)
library(prettymapr)


sf_use_s2(FALSE)
rm(list=ls())

# Unify projections
crs.geo <- sf::st_crs(4326)
sf_use_s2(FALSE)

# Plot Distribution of Water Conflicts
### load world map
world <- read_sf(system.file("shapes/world.shp", package = "spData"))

### water conflicts data
waterconf <- read.csv('Conflict Datasets/Water-Conflict-Data-08222024.csv')
waterconf <- waterconf %>%
  filter(Date>1990)

### to shp file
waterconf_shp <- st_as_sf(x = waterconf, coords = c("Longitude", "Latitude"),
                 crs = crs.geo, remove = F)

### plot
conflict_distribution <- ggplot()+
  geom_sf(data = world, fill = 'white', size = 0.2)+
  geom_sf(data = waterconf_shp, 
          color = '#FF0000',    # Border
          fill = '#B22222',      # Fill (FireBrick red)
          size = 0.1,
          shape = 21,             # Circle with border and fill
          stroke = 0.5)+          # Border thickness
  theme_void()+
  labs(x = 'Lon', y='Lat', color='', shape='')

ggsave('Figure/map/conflict_distribution.pdf', conflict_distribution, device = 'pdf', width = 7, height = 4, units = 'in')


## Trend of Dam Construction

### Construct datasets
dams_all <- read_xlsx('Dams/african_dams_FAO5.xlsx', sheet=1)
dams_all <- dams_all %>%
  dplyr::filter(!is.na(Year)) %>%
  mutate(main_use = ifelse(!is.na(Hydroelectricity), 'electricity',
                           ifelse(!is.na(Irrigation)|!is.na(`Water supply`), 'irr_water', 'others')),
         period = ifelse(Year>=1895 & Year!='Incomplete' & Year!='Incomplete?', 
                          as.character(cut(as.numeric(Year), breaks = seq(1895, 2022, 5), right=FALSE, 
                                           labels=seq(1895, 2020, 5)[-1])), NA)) %>%
  mutate(period = ifelse(Year %in% c('Incomplete', 'Incomplete?'), 'Incomplete', period)) %>%
  group_by(period, main_use) %>%
  mutate(count_num = n()) %>%
  ungroup() %>%
  group_by(period) %>%
  mutate(height_mean = mean(Height, na.rm=T),
         reservoir_mean = mean(Reservoir_capacity_million_m3, na.rm=T)) %>%
  dplyr::select(period, main_use, count_num, height_mean, reservoir_mean) %>%
  mutate(height_mean = ifelse(period=='Incomplete', NA, height_mean)) %>%
  distinct() 
  
### Plot
dam_trend <- dams_all %>% filter(!is.na(period)) %>%
  ggplot(aes(x = period, y= count_num, group = main_use))+
  geom_bar(aes(x = period, y = count_num, group = main_use, fill = main_use), stat = 'identity')+
  geom_line(aes(y = height_mean*1.75, group = 1))+
  geom_vline(xintercept = 20.5, color = 'red', linetype = 2)+
  geom_vline(xintercept = 25.5, color = 'red', linetype = 2)+
  scale_y_continuous(
    # Features of the first axis
    name = "Number of Dam Commissions Per 5 Years",
    # Add a second axis and specify its features
    sec.axis = sec_axis( trans=~./1.75, name="Mean Height (m)")
  ) +
  theme_classic()+
  annotate('text', x = 21, y = 250, label = 'Sample Period', color = 'red', size = 3.5)+
  scale_fill_manual(values = c('#E1AF00', 'dodgerblue2', 'gray'),
                    labels = c('Hydropower', 'Irrigation', 'Others'))+
  labs(x='Year', fill = 'Main Usage')+
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

ggsave('Figure/dam_trend.pdf', dam_trend, device = 'pdf', width = 6, height = 5, units = 'in')


## Distribution of dams in our sample

### load dams data
dams <- read_rds('Datasets/dams_combined_updated.rds')
dams <- st_as_sf(dams)
### africa map
africa <- world %>% filter(continent == 'Africa')


### plot
dams_distribution <- ggplot()+
  annotation_map_tile(type = "osm", zoom = 3) +  # Esri topographic basemap
  geom_sf(data = dams[dams$Dam_Year %in% c(2000:2020),],  
          color = 'black',    # Deep teal border
          fill = '#00CED1',      # Turquoise fill
          size = 2.5,
          shape = 23,             # Circle with border and fill
          stroke = 0.6)+          # Border thickness
  annotation_scale() +            # Add scale bar
  #annotation_north_arrow(        # Add north arrow
   # location = "tr",             # top right
    #which_north = "true",
    #style = north_arrow_fancy_orienteering) +
  theme_void()+
  labs(x = 'Lon', y='Lat', color='', shape='')

ggsave('Figure/map/dams_distribution.pdf', dams_distribution, device = 'pdf', width = 5, height = 5, units = 'in')

nrow(dams[dams$Dam_Year%in% c(2000:2020),])


## Illustration of Empirical Design
### load river flow data
load(file = 'Datasets/Delineate_River.RData')
### load completed conflict data
conflict_grid <- readRDS('Datasets/conflict_grid.rds')
### match up with dams_grid
dams_grid_panel <- readRDS(file = "Datasets/dams_grid_panel.rds")
conflict_eg <- merge(data.table(unique(dams_grid_panel[dams_grid_panel$dam_id==1,c(1:42, 69)])), 
                                  data.table(conflict_grid),
                                  by.x = c('cell'), 
                                  by.y = c('cells'))
conflict_eg$geometry <- NULL


dams <- st_as_sf(dams)
conflict_eg <- st_as_sf(conflict_eg, coords = c("longitude", "latitude"), remove = F)
st_crs(conflict_eg) <- st_crs(dams)

### Showing how dams data matched up with conflicts data
map_conflict_match <- ggplot()+
  geom_sf(data = dams_basins[dams_basins$dam_id==1,], fill='white')+
  geom_sf(data = stemrivers_affected_dams[stemrivers_affected_dams$dam_id==1,], lwd = 0.7, aes(color = 'Rivers'))+
  geom_sf(data = dams[dams$dam_id==1, ], size = 1, aes(color = 'Dams'), alpha = 0.8)+
  geom_sf(data = dams[dams$dam_id==1, ], size = 3, aes(color = 'Dams'), alpha = 0.6)+
  geom_sf(data = dams[dams$dam_id==1, ], size = 5, aes(color = 'Dams'), alpha = 0.3)+
  geom_sf(data = dams[dams$dam_id==1, ], size = 7, aes(color = 'Dams'), alpha = 0.1)+
  geom_sf(data = st_jitter(conflict_eg[conflict_eg$dist_rivers_m<=30000,], 0.02), 
          aes(color = 'Conflicts'),    # Deep teal border
          fill = '#00CED1',      # Turquoise fill
          size = 2,
          shape = 23,             # Circle with border and fill
          stroke = 0.6,
          alpha = 0.8)+
  geom_sf(data = st_jitter(conflict_eg[conflict_eg$dist_rivers_m>30000,], 0.02), 
          aes(color = 'Conflicts>30KM'),    # Deep teal border
          fill = 'yellow',      # Turquoise fill
          size = 2,
          shape = 23,             # Circle with border and fill
          stroke = 0.6,
          alpha = 0.8)+
  #coord_sf(xlim=c(7.2, 9.05), ylim=c(10.9, 12.6))+
  scale_color_manual(name = NULL,
                     guide = "legend",
                     values = c("Dams" = "red",
                                'Rivers' = '#0492C2',
                                'Conflicts' = 'black',
                                'Conflicts>30KM' = 'black')) +
  guides(color = guide_legend(override.aes = list(linetype = c(NA,NA,NA,1),
                                                  shape = c(23,23, 19, NA),
                                                  fill = c('#00CED1','yellow',NA,NA))))+
  theme_void()+
  labs(x = 'Lon', y='Lat')

ggsave('Figure/map/map_conflict_match.pdf', map_conflict_match, device = 'pdf', width = 8, height = 5, units = 'in')



## Illstrate for grid cell
grid <- st_make_grid(dams_basins[dams_basins$dam_id==1,], cellsize = 0.1, 
                     offset = st_bbox(dams_basins[dams_basins$dam_id==1,]))

map_conflict_grid <- ggplot()+
  geom_sf(data = grid, fill = NA, col = "firebrick", lwd = 0.2)+
  geom_sf(data = dams_basins[dams_basins$dam_id==1,], fill=NA)+
  geom_sf(data = stemrivers_affected_dams[stemrivers_affected_dams$dam_id==1,], lwd = 0.7, aes(color = 'Rivers'))+
  geom_sf(data = dams[dams$dam_id==1, ], size = 1, aes(color = 'Dams'), alpha = 0.8)+
  geom_sf(data = dams[dams$dam_id==1, ], size = 3, aes(color = 'Dams'), alpha = 0.6)+
  geom_sf(data = dams[dams$dam_id==1, ], size = 5, aes(color = 'Dams'), alpha = 0.3)+
  geom_sf(data = dams[dams$dam_id==1, ], size = 7, aes(color = 'Dams'), alpha = 0.1)+
  geom_sf(data = st_jitter(conflict_eg[conflict_eg$dist_rivers_m<=30000,], 0.02), 
          aes(color = 'Conflicts'),    # Deep teal border
          fill = '#00CED1',      # Turquoise fill
          size = 2,
          shape = 23,             # Circle with border and fill
          stroke = 0.6,
          alpha = 0.8)+
  coord_sf(xlim=c(13.5, 15.5), ylim=c(-10.5, -8.5))+
  scale_color_manual(name = NULL,
                     guide = "legend",
                     values = c("Dams" = "red",
                                'Rivers' = '#0492C2',
                                'Conflicts' = 'black')) +
  guides(color = guide_legend(override.aes = list(linetype = c(NA,NA,1),
                                                  shape = c(23, 19, NA),
                                                  fill = c('#00CED1',NA,NA))))+
  theme_void()+
  labs(x = 'Lon', y='Lat')

ggsave('Figure/map/map_conflict_grid.pdf', map_conflict_grid, device = 'pdf', width = 7, height = 5, units = 'in')




#### Delinate Upstream/Downstream
mybreaks <- seq(0,1800,200)
mylabels <- seq(0,1800,200)

map_elevation <- ggplot()+
  geom_sf(data = dams_basins[dams_basins$dam_id==1,], aes(fill = DEM))+
  scale_fill_gradientn(
    colours = rev(terrain.colors(10)), 
    breaks = mybreaks,
    labels = mylabels,
    name = 'Elevation(m)')+
  geom_sf(data = dams_basins[dams_basins$dam_id==1,], fill=NA)+
  geom_sf(data = stemrivers_affected_dams[stemrivers_affected_dams$dam_id==1 & 
                                            stemrivers_affected_dams$downstream_segment==1,], 
          lwd = 0.7, aes(color = 'Downstream Segments'))+
  geom_sf(data = stemrivers_affected_dams[stemrivers_affected_dams$dam_id==1 & 
                                            stemrivers_affected_dams$downstream_segment==0,], 
          lwd = 0.7, aes(color = 'Upstream Segments'))+
  geom_sf(data = dams[dams$dam_id==1, ], size = 1, aes(color = 'Dams'), alpha = 0.8)+
  geom_sf(data = dams[dams$dam_id==1, ], size = 3, aes(color = 'Dams'), alpha = 0.6)+
  geom_sf(data = dams[dams$dam_id==1, ], size = 5, aes(color = 'Dams'), alpha = 0.3)+
  geom_sf(data = dams[dams$dam_id==1, ], size = 7, aes(color = 'Dams'), alpha = 0.1)+
  geom_sf(data = st_jitter(conflict_eg[conflict_eg$dist_rivers_m<=30000,], 0.02), 
          aes(color = 'Conflicts'),    # Deep teal border
          fill = '#00CED1',      # Turquoise fill
          size = 2,
          shape = 23,             # Circle with border and fill
          stroke = 0.6,
          alpha = 0.8)+
  #coord_sf(xlim=c(7.2, 9.05), ylim=c(10.9, 12.6))+
  scale_color_manual(name = NULL,
                     guide = "legend",
                     values = c("Dams" = "red",
                                'Downstream Segments' = 'red',
                                'Upstream Segments' = '#0492C2',
                                'Conflicts' = 'black',
                                'Conflicts>30KM' = 'black')) +
  guides(color = guide_legend(override.aes = list(linetype = c(NA,NA,1,1),
                                                  shape = c(23, 19, NA,NA),
                                                  fill = c('#00CED1',NA,NA,NA))))+
  theme_void()+
  labs(x = 'Lon', y='Lat')

ggsave('Figure/map/map_elevation.pdf', map_elevation, device = 'pdf', width = 9, height = 5, units = 'in')


### Illustrate Empirical Desgin
map_empirical <- ggplot()+
  geom_sf(data = dams_basins_aggr[dams_basins_aggr$dam_id==1 & 
                                    dams_basins_aggr$downstream_basin==0,], fill = 'lightblue', alpha=0.9)+
  geom_sf(data = dams_basins_aggr[dams_basins_aggr$dam_id==1 & 
                                    dams_basins_aggr$downstream_basin==1,], 
          fill='#FFD700', alpha=0.6)+
  geom_sf(data = stemrivers_affected_dams[stemrivers_affected_dams$dam_id==1 & 
                                            stemrivers_affected_dams$downstream_segment==1 &
                                            stemrivers_affected_dams$affected_riv==1,], 
          lwd = 0.7, aes(color = 'Affected River'))+
  geom_sf(data = stemrivers_affected_dams[stemrivers_affected_dams$dam_id==1 & 
                                            stemrivers_affected_dams$downstream_segment==1 &
                                            stemrivers_affected_dams$affected_riv==0,], 
          lwd = 0.7, aes(color = 'Unaffected River'))+
  geom_sf(data = stemrivers_affected_dams[stemrivers_affected_dams$dam_id==1 & 
                                            stemrivers_affected_dams$downstream_segment==0,], 
          lwd = 0.7, aes(color = 'Upstream Segments'))+
  geom_sf(data = dams[dams$dam_id==1, ], size = 1, aes(color = 'Dams'), alpha = 0.8)+
  geom_sf(data = dams[dams$dam_id==1, ], size = 3, aes(color = 'Dams'), alpha = 0.6)+
  geom_sf(data = dams[dams$dam_id==1, ], size = 5, aes(color = 'Dams'), alpha = 0.3)+
  geom_sf(data = dams[dams$dam_id==1, ], size = 7, aes(color = 'Dams'), alpha = 0.1)+
  geom_sf(data = st_jitter(conflict_eg[conflict_eg$dist_rivers_m<=30000 & 
                               conflict_eg$affected_riv==1 &
                               conflict_eg$downstream_segment==1,], 0.02), 
          aes(color = 'Conflicts (Affected)'),    # Deep teal border
          fill = 'red',      # Turquoise fill
          size = 2.5,
          shape = 23,             # Circle with border and fill
          stroke = 0.6,
          alpha = 0.8)+
  geom_sf(data = st_jitter(conflict_eg[conflict_eg$dist_rivers_m<=30000 & 
                               conflict_eg$affected_riv==1 &
                               conflict_eg$downstream_segment==0,], 0.02), 
          aes(color = 'Conflicts (Upstream)'),    # Deep teal border
          fill = '#0492C2',      # Turquoise fill
          size = 2.5,
          shape = 23,             # Circle with border and fill
          stroke = 0.6,
          alpha = 0.8)+
  geom_sf(data = st_jitter(conflict_eg[conflict_eg$dist_rivers_m<=30000 & 
                               conflict_eg$affected_riv==0,], 0.02), 
          aes(color = 'Conflicts (Unaffected)'),    # Deep teal border
          fill = 'orange',      # Turquoise fill
          size = 2.5,
          shape = 21,             # Circle with border and fill
          stroke = 0.6,
          alpha = 0.8)+
  #coord_sf(xlim=c(7.2, 9.05), ylim=c(10.9, 12.6))+
  scale_color_manual(name = NULL,
                     guide = "legend",
                     values = c("Dams" = "red",
                                'Affected River' = 'red',
                                'Unaffected River' = 'orange',
                                'Upstream Segments' = '#0492C2',
                                'Conflicts (Affected)' = 'black',
                                'Conflicts (Unaffected)' = 'black',
                                'Conflicts (Upstream)' = 'black')) +
  guides(color = guide_legend(override.aes = list(linetype = c(1,NA,NA,NA,NA,1,1),
                                                  shape = c(NA,23,21,23, 19, NA,NA),
                                                  fill = c(NA,'red','orange','#0492C2',NA,NA,NA))))+
  theme_void()+
  labs(x = 'Lon', y='Lat')

ggsave('Figure/map/map_empirical.pdf', map_empirical, device = 'pdf', width = 9, height = 6, units = 'in')


map_empirical_cell <- ggplot()+
  geom_sf(data = dams_basins_aggr[dams_basins_aggr$dam_id==1 & 
                                    dams_basins_aggr$downstream_basin==0,], fill = 'lightblue', alpha=0.9)+
  geom_sf(data = dams_basins_aggr[dams_basins_aggr$dam_id==1 & 
                                    dams_basins_aggr$downstream_basin==1,], 
          fill='#FFD700', alpha=0.6)+
  geom_sf(data = grid, fill = NA, col = "firebrick", lwd = 0.1)+
  geom_sf(data = stemrivers_affected_dams[stemrivers_affected_dams$dam_id==1 & 
                                            stemrivers_affected_dams$downstream_segment==1 &
                                            stemrivers_affected_dams$affected_riv==1,], 
          lwd = 0.7, aes(color = 'Affected River'))+
  geom_sf(data = stemrivers_affected_dams[stemrivers_affected_dams$dam_id==1 & 
                                            stemrivers_affected_dams$downstream_segment==1 &
                                            stemrivers_affected_dams$affected_riv==0,], 
          lwd = 0.7, aes(color = 'Unaffected River'))+
  geom_sf(data = stemrivers_affected_dams[stemrivers_affected_dams$dam_id==1 & 
                                            stemrivers_affected_dams$downstream_segment==0,], 
          lwd = 0.7, aes(color = 'Upstream Segments'))+
  geom_sf(data = dams[dams$dam_id==1, ], size = 1, aes(color = 'Dams'), alpha = 0.8)+
  geom_sf(data = dams[dams$dam_id==1, ], size = 3, aes(color = 'Dams'), alpha = 0.6)+
  geom_sf(data = dams[dams$dam_id==1, ], size = 5, aes(color = 'Dams'), alpha = 0.3)+
  geom_sf(data = dams[dams$dam_id==1, ], size = 7, aes(color = 'Dams'), alpha = 0.1)+
  geom_sf(data = st_jitter(conflict_eg[conflict_eg$dist_rivers_m<=30000 & 
                                         conflict_eg$affected_riv==1 &
                                         conflict_eg$downstream_segment==1,], 0.02), 
          aes(color = 'Conflicts (Affected)'),    # Deep teal border
          fill = 'red',      # Turquoise fill
          size = 2.5,
          shape = 23,             # Circle with border and fill
          stroke = 0.6,
          alpha = 0.8)+
  geom_sf(data = st_jitter(conflict_eg[conflict_eg$dist_rivers_m<=30000 & 
                                         conflict_eg$affected_riv==1 &
                                         conflict_eg$downstream_segment==0,], 0.02), 
          aes(color = 'Conflicts (Upstream)'),    # Deep teal border
          fill = '#0492C2',      # Turquoise fill
          size = 2.5,
          shape = 23,             # Circle with border and fill
          stroke = 0.6,
          alpha = 0.8)+
  geom_sf(data = st_jitter(conflict_eg[conflict_eg$dist_rivers_m<=30000 & 
                                         conflict_eg$affected_riv==0,], 0.02), 
          aes(color = 'Conflicts (Unaffected)'),    # Deep teal border
          fill = 'orange',      # Turquoise fill
          size = 2.5,
          shape = 21,             # Circle with border and fill
          stroke = 0.6,
          alpha = 0.8)+
  coord_sf(xlim=c(13.5, 15.5), ylim=c(-10.5, -8.5))+
  scale_color_manual(name = NULL,
                     guide = "legend",
                     values = c("Dams" = "red",
                                'Affected River' = 'red',
                                'Unaffected River' = 'orange',
                                'Upstream Segments' = '#0492C2',
                                'Conflicts (Affected)' = 'black',
                                'Conflicts (Unaffected)' = 'black',
                                'Conflicts (Upstream)' = 'black')) +
  guides(color = guide_legend(override.aes = list(linetype = c(1,NA,NA,NA,NA,1,1),
                                                  shape = c(NA,23,21,23, 19, NA,NA),
                                                  fill = c(NA,'red','orange','#0492C2',NA,NA,NA))))+
  theme_void()+
  labs(x = 'Lon', y='Lat')

ggsave('Figure/map/map_empirical_cell.pdf', map_empirical_cell, device = 'pdf', width = 8, height = 5, units = 'in')
