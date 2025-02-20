rstudioapi::getActiveDocumentContext
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(fixest)
library(texreg)
library(tidyverse)
library(data.table)
library(sf)
library(parallel)
library(conleyreg)
library(haven)

sf_use_s2(FALSE)
rm(list=ls())
# Set threads
setDTthreads(10)

###################################### Preparation #####################################  

## load data
main_sample <- readRDS(file = "Datasets/main_sample.rds")

## control/FEs varibles
main_sample <- main_sample %>%
  mutate(lon = y,
         lat = x,
         timetrend = as.numeric(factor(time)))

## new dependent variables
main_sample <- main_sample %>%
  dplyr::mutate(across(c(num_peaceful_protest:num_output),
                       ~ ifelse(.>0, 1, 0),
                       .names = '{.col}_dummy'),
                across(c(num_peaceful_protest:num_output),
                       ~ (.-mean(., na.rm=T))/sd(., na.rm=T),
                       .names = '{.col}_std'))

main_sample <- main_sample %>%
  mutate(pdsi = drought/100) %>%
  mutate(severe_drought = ifelse(pdsi < -4, 1, 0)) %>%
  mutate(lnNpp = log(Npp+1),
         lnsoilms = log(soilmoisture+1),
         lnwaterdef = log(waterdef+1),
         waterdef_std = (waterdef-mean(waterdef, na.rm=T))/sd(waterdef, na.rm=T),
         soilms_std = (soilmoisture-mean(soilmoisture, na.rm=T))/sd(soilmoisture, na.rm=T),
         lngws = log(gws_mm)) %>%
  mutate(TempMax=TempMax/10,
         TempMin=TempMin/10) %>%
  mutate(hightemp = ifelse(TempMax>30|TempMin>30, 1, 0),
         lowtemp = ifelse(TempMax<17.78|TempMin<17.78, 1, 0)) %>%
  mutate(severetemp = ifelse(hightemp==1|lowtemp==1, 1, 0))

### Climate/GWSTOR_V2
main_sample <- main_sample %>%
  mutate(arid = ifelse(climates_f %in% c('BSh', 'Bsk', 'BWh', 'Bwk', 'AW'), 1, 0),
         low_grwater = ifelse(GWSTOR_V2 %in% c('L', 'LM'), 1, 0))

## relevel
main_sample <- main_sample %>%
  mutate(ring2_fct = ordered(ring2, levels = c('R50', 'R50_100', 'R100_150', 'Control')))


###################################### Define Variables #####################################  

indvar <- c('affected_riv*post2','affected_riv*period_label',
            'downstream_segment*post2',
            'affected_riv*ring2*post2')

dict = c('affected_riv' = 'Affected_Riv',
         'downstream_segment' = 'Downstream',
         'post2' = 'Post',
         'cropland_dummy' = 'Cropland',
         'ethnicfrac_cell_high' = 'HighEthnicFrac_Cell',
         'ethnicfrac_dam_high' = 'HighEthnicFrac_Dam',
         'factor(dam_id)' = 'Dam',
         'factor(cell)' = 'Cell',
         'factor(year)' = 'Year',
         'factor(month)' = 'Month',
         'factor(ADM0_NAME)'='Country',
         'timetrend' = 'Time Trend')

## baseline variables
depvar_baseline <- c('num_all', 'num_all_dummy')
controls_baseline <- c('log(rainfall+1)+TempMax+TempMin+log(PopCount+1)+num_mine',
                       'factor(climates_f):factor(year)+factor(Lithology):factor(year)+
                       factor(GWSTOR_V2):factor(year)+log(rainfall+1)+TempMax+TempMin+
                       num_mine:factor(year)+log(PopCount+1)')
fes_baseline <- c('factor(dam_id)+factor(cell)+factor(year)+factor(month)',
                  'factor(dam_id)+factor(cell)+factor(ADM0_NAME)^factor(year)+
         factor(ADM0_NAME)^factor(month)+factor(year)^factor(month)')


## mechanism variables
depvar_mech <- c('lnNpp','soilms_std','lnsoilms', 'pdsi','severe_drought', 
                 'waterdef_std','lnwaterdef','TempMax', 'TempMin', 'lngws',
                 'waterdef', 'soilmoisture', 'Npp','hightemp', 'lowtemp', 'severetemp', 'gws_mm')
controls_mech <- c('factor(climates_f):factor(year)+factor(Lithology):factor(year)+
                    factor(GWSTOR_V2):factor(year)+log(dist_dams_m)+log(dist_rivers_m)',
                   'factor(climates_f):factor(year)+factor(Lithology):factor(year)+
                    factor(GWSTOR_V2):factor(year)+log(dist_dams_m)+log(dist_rivers_m)+log(rainfall+1)',
                   'factor(SUB_NAME):factor(year)+factor(climates_f):factor(year)+factor(Lithology):factor(year)+
                    factor(GWSTOR_V2):factor(year)+log(dist_dams_m)+log(dist_rivers_m)',
                   'factor(SUB_NAME):factor(year)+factor(climates_f):factor(year)+factor(Lithology):factor(year)+
                    factor(GWSTOR_V2):factor(year)+log(dist_dams_m)+log(dist_rivers_m)+log(rainfall+1)')
fes_mech <- c('factor(dam_id)+factor(cell)+factor(year)+factor(month)',
              'factor(dam_id)+factor(cell)+factor(year)^factor(month)',
              'factor(cell)+factor(dam_id)^factor(year)+factor(dam_id)^factor(month)+
              factor(year)^factor(month)',
              'factor(dam_id)^factor(year)+factor(dam_id)^factor(month)+factor(cell)+
              factor(ADM0_NAME)^factor(year)+factor(ADM0_NAME)^factor(month)+
              factor(year)^factor(month)')


## sample
subset1 <- paste0("~ dist_rivers_m <= 30000 & Treat==1 & paired == 1 & Dam_Year %in% c(2001:2020) & 
                            downstream_segment==1")
subset2 <- paste0("~ dist_rivers_m <= 30000 & Treat==1 & paired == 1 & Dam_Year %in% c(2001:2020) & 
                            downstream_segment==0")


###################################### Baseline Regression #####################################  

##### Table #####

for (i in seq_along(depvar_baseline)){
  ## formular
  fml1.1 <- paste(depvar_baseline[i], '~', indvar[1], '+', controls_baseline[1], '|', fes_baseline[1])
  fml1.2 <- paste(depvar_baseline[i], '~', indvar[1], '+', controls_baseline[1], '|', fes_baseline[2])
  fml1.3 <- paste(depvar_baseline[i], '~', indvar[1], '+', controls_baseline[2], '|', fes_baseline[2])
  
  ## regression
  result1.1 <- feols(eval(parse(text = fml1.1)),
                     data = main_sample, 
                     subset = as.formula(subset1),
                     #split = ~ ring2_fct,
                     cluster = ~ cell,
                     nthreads = 12)
  
  result1.2 <- feols(eval(parse(text = fml1.2)),
                     data = main_sample, 
                     subset = as.formula(subset1),
                     #split = ~ ring2_fct,
                     cluster = ~ cell,
                     nthreads = 12)
  
  result1.3 <- feols(eval(parse(text = fml1.3)),
                     data = main_sample, 
                     subset = as.formula(subset1),
                     #split = ~ ring2_fct,
                     cluster = ~ cell,
                     nthreads = 12)
  
  result1.4 <- feols(eval(parse(text = fml1.1)),
                     data = main_sample, 
                     subset = as.formula(subset2),
                     #split = ~ ring2_fct,
                     cluster = ~ cell,
                     nthreads = 12)
  
  result1.5 <- feols(eval(parse(text = fml1.2)),
                     data = main_sample, 
                     subset = as.formula(subset2),
                     #split = ~ ring2_fct,
                     cluster = ~ cell,
                     nthreads = 12)
  
  result1.6 <- feols(eval(parse(text = fml1.3)),
                     data = main_sample, 
                     subset = as.formula(subset2),
                     #split = ~ ring2_fct,
                     cluster = ~ cell,
                     nthreads = 12)
  
  ## Mean
  attach(main_sample)
  depvar <- get(depvar_baseline[i])
  conditions <- Treat==1 & paired == 1 & Dam_Year %in% c(2001:2020) & dist_rivers_m <= dist_rivers[d]
  
  mean1 <- round(mean(depvar[conditions & downstream_segment==1], na.rm=T), 3)
  mean2 <- round(mean(depvar[conditions & downstream_segment==0], na.rm=T), 3)
  
  ## regression table
  etable(result1.1, result1.2, result1.3, result1.4, result1.5, result1.6,
         title = 'The Effect of Hydropower Dams on the Incidence of Conflicts', 
         headers = list("Downstream"=3, "Upstream"=3),
         keep = '%affected_riv|post2|ring2|ethnicfrac_cell_high|ethnicfrac_dam_high',
         order = c("Post"), dict = dict,
         extralines = list('_^Mean of Dep. Var.'=c(mean1,mean1,mean1,
                                                   mean2,mean2,mean2),
                           '_Covariates' = c('Y','Y','Y','Y','Y','Y'),
                           '_Covariates*Year' = c('','','Y','','','Y')),
         fitstat= ~ n+r2, depvar = T, placement = 'h', tpt=T, adjustbox=1,
         page.width = "fit", tex = T, style.tex = style.tex("aer"), 
         digits = 3, digits.stats = 3,
         label = paste('tab',depvar_baseline[i], dist_rivers[d], sep='_'), replace = T,
         file = paste0('Latex Table/AEA_slides/', depvar_baseline[i], '_30000.tex'))
  
  ## Heterogeneity by Cropland
  result1.7 <- feols(eval(parse(text = fml1.3)),
                     data = main_sample, 
                     subset = as.formula(subset1),
                     split = ~ cropland_dummy,
                     cluster = ~ cell,
                     nthreads = 12)
  
  result1.8 <- feols(eval(parse(text = fml1.3)),
                     data = main_sample, 
                     subset = as.formula(subset2),
                     split = ~ cropland_dummy,
                     cluster = ~ cell,
                     nthreads = 12)
  
  ## Mean
  mean3 <- round(mean(depvar[conditions & downstream_segment==1 & cropland_dummy==0], na.rm=T), 3)
  mean4 <- round(mean(depvar[conditions & downstream_segment==1 & cropland_dummy==1], na.rm=T), 3)
  mean5 <- round(mean(depvar[conditions & downstream_segment==0 & cropland_dummy==0], na.rm=T), 3)
  mean6 <- round(mean(depvar[conditions & downstream_segment==0 & cropland_dummy==1], na.rm=T), 3)
  
  ## regression table
  etable(result1.7, result1.8,
         title = 'The Effect of Hydropower Dams on the Incidence of Conflicts by Cropand', 
         headers = list("Downstream"=2, "Upstream"=2),
         keep = '%affected_riv|post2|ring2|ethnicfrac_cell_high|ethnicfrac_dam_high',
         order = c("Post"), dict = dict,
         extralines = list('_^Mean of Dep. Var.'=c(mean3,mean4,mean5,mean6),
                           '_Covariates' = c('Y','Y','Y','Y')),
         fitstat= ~ n+r2, depvar = T, placement = 'h', tpt=T, adjustbox=1,
         page.width = "fit", tex = T, style.tex = style.tex("aer"), 
         digits = 3, digits.stats = 3,
         label = paste('tab_cropland',depvar_baseline[i], dist_rivers[d], sep='_'), replace = T,
         file = paste0('Latex Table/AEA_slides/', depvar_baseline[i], '_Cropland_30000.tex'))
  
}

rm(result1.1, result1.2, result1.3, result1.4, result1.5,
   result1.6, result1.7, result1.8)

## By types
depvar_types <- c('num_peaceful_protest_std','num_protest_with_intervention_std',
                  'num_excessive_force_against_protesters_std',
                  'num_violent_demonstration_std','num_armed_clash_std', 
                  'num_government_regains_territory_std',
                  'num_nonstate_actor_overtakes_territory_std', 'num_sexual_violence_std',
                  'num_abduction_and_forced_disappearance_std',
                  'num_mob_violence_std', 'num_attack_std', 
                  'num_looting_and_property_destruction_std',
                  'num_water_std')
depvar_types_label <- c('Peaceful protest','Protest with intervention',
                  'Excessive force against protesters',
                  'Violent demonstration','Armed clash', 
                  'Government regains territory',
                  'Nonstate actor overtakes territory', 'Sexual violence',
                  'Abduction and forced disappearance',
                  'Mob violence', 'Attack', 
                  'Looting and property destruction',
                  'Water-related')
tab <- list()
coef_table <- data.table()
tab_mean <- c()
for (i in seq_along(depvar_types)){
  
  fml1.3 <- paste(depvar_types[i], '~', indvar[1], '+', controls_baseline[1], '|', fes_baseline[2])
  
  ## Heterogeneity by Cropland
  result1.1 <- feols(eval(parse(text = fml1.3)),
                     data = main_sample, 
                     subset = as.formula(subset1),
                     #fsplit = ~ cropland_dummy,
                     cluster = ~ cell,
                     nthreads = 12)
  
  tab <- c(tab, list(result1.1))
  
  ## Mean
  attach(main_sample)
  depvar <- get(depvar_types[i])
  conditions <- Treat==1 & paired == 1 & Dam_Year %in% c(2001:2020) & dist_rivers_m <= 30000
  
  mean1 <- round(mean(depvar[conditions & downstream_segment==1], na.rm=T), 3)
  tab_mean <-c(tab_mean, mean1)
  
  ## coef. table
  result1_df <- cbind(data.frame(result1.1$coefficients), data.frame(result1.1$se))
  result1_df <- data.frame(result1_df[grepl('affected_riv:post2', 
                                            rownames(result1_df))==T,])
  names(result1_df) <- c('coef', 'se')
  result1_df <- result1_df %>%
    mutate(type=depvar_types_label[i])
  
  coef_table <- rbind(coef_table, result1_df)
}

## regression table
etable(tab,
       title = 'The Effect of Hydropower Dams on the Incidence of Conflicts by Types (Downstream)', 
       headers = list("Factor Conflicts"=9, "Output Conflicts"=3, 'Water'=1),
       keep = '%affected_riv|post2|ring2|ethnicfrac_cell_high|ethnicfrac_dam_high',
       order = c("Post"), dict = dict,
       extralines = list('_^Mean of Dep. Var.'= tab_mean,
                         '_Covariates' = c('Y','Y','Y','Y','Y','Y',
                                                'Y','Y','Y','Y','Y','Y','Y')),
       fitstat= ~ n+r2, depvar = T, placement = 'h', tpt=T, adjustbox=1,
       page.width = "fit", tex = T, style.tex = style.tex("aer"), 
       digits = 3, digits.stats = 3,
       label = 'tab_bytypes_std_30000', replace = T,
       file = paste0('Latex Table/AEA_slides/Bytypes_std_30000.tex'))


## coef. fig.
fig_types <- coef_table %>% ggplot()+ 
  geom_segment(aes(x = fct_reorder(factor(type), coef), 
                   xend = fct_reorder(factor(type), coef),
                   y=coef - 1.96*se, yend=coef + 1.96*se, group=1), 
               size=0.5, linewidth = 0.5, position=position_dodge(0.3),
               color = 'firebrick')+
  geom_point(aes(x = fct_reorder(factor(type), coef), y = coef, group=1), 
             position=position_dodge(0.3), color = 'firebrick') + 
  labs(x='Types', y='Monthly # of conflicts (std.)')+
  geom_hline(aes(yintercept=0), color="black", linetype = "dashed")+
  coord_flip()+
  theme_classic()+
  theme(legend.position = "bottom", 
        legent.text = element_text(size=rel(0.5)),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        axis.title=element_text(size=20),
        axis.text=element_text(size=15),
        strip.text.x = element_text(size = 20))

## Save
ggsave('Figure/event_study/AEA_slides/typesfig_30000.pdf', 
       fig_types, device = 'pdf', width = 8, height = 6, units = 'in')

  
  
##### Event Study #####

label_depvar <- c('# of Conflicts', 'Conflict Dummy')

### Baseline Results
for (i in seq_along(depvar_baseline)){
  ## formular
  fml1.1 <- paste(depvar_baseline[i], '~', indvar[2], '+', controls_baseline[1], '|', fes_baseline[2])
  
  ## regression
  result1.1 <- feols(eval(parse(text = fml1.1)),
                     data = main_sample, 
                     subset = as.formula(subset1),
                     #split = ~ ring2_fct,
                     cluster = ~ cell,
                     nthreads = 12)
  
  result1.2 <- feols(eval(parse(text = fml1.1)),
                     data = main_sample, 
                     subset = as.formula(subset2),
                     #split = ~ ring2_fct,
                     cluster = ~ cell,
                     nthreads = 12)
  
  # Downstream
  result1_df <- cbind(data.frame(result1.1$coefficients), data.frame(result1.1$se))
  result1_df <- data.frame(result1_df[grepl('affected_riv:period_label', 
                                            rownames(result1_df))==T,])
  names(result1_df) <- c('coef', 'se')
  result1_df$period <- str_sub(rownames(result1_df), nchar('affected_riv:period_label')+1,-1)
  result1_df[nrow(result1_df) + 1,] <- c(0, 0, '-1')
  result1_df$coef_id <- ifelse(result1_df$period=='-5+', 1,
                               ifelse(result1_df$period=='9+', 15, 
                                      as.numeric(result1_df$period)+6))
  result1_df$coef <- as.numeric(result1_df$coef)
  result1_df$se <- as.numeric(result1_df$se)

  result1_df <- result1_df %>%
    mutate(period_dummy = factor(ifelse(period<0, 0, 1))) %>%
    mutate(downstream = 'Downstream')
  
  # Upstream
  result2_df <- cbind(data.frame(result1.2$coefficients), data.frame(result1.2$se))
  result2_df <- data.frame(result2_df[grepl('affected_riv:period_label', 
                                            rownames(result2_df))==T,])
  names(result2_df) <- c('coef', 'se')
  result2_df$period <- str_sub(rownames(result2_df), nchar('affected_riv:period_label')+1,-1)
  result2_df[nrow(result2_df) + 1,] <- c(0, 0, '-1')
  result2_df$coef_id <- ifelse(result2_df$period=='-5+', 1,
                               ifelse(result2_df$period=='9+', 15, 
                                      as.numeric(result2_df$period)+6))
  result2_df$coef <- as.numeric(result2_df$coef)
  result2_df$se <- as.numeric(result2_df$se)
  result2_df <- result2_df %>%
    mutate(period_dummy = factor(ifelse(period<0, 0, 1))) %>%
    mutate(downstream = 'Upstream')
  
  es_table <- rbind(result1_df, result2_df)
  
  
  ## Graph
  es_plot <- es_table %>% ggplot()+ 
    geom_segment(aes(x = fct_reorder(factor(period), coef_id), 
                     xend = fct_reorder(factor(period), coef_id),
                     y=coef - 1.96*se, yend=coef + 1.96*se, group=period_dummy, color = period_dummy), 
                 size=0.5, linewidth = 0.5, position=position_dodge(0.3))+
    geom_point(aes(x = fct_reorder(factor(period), coef_id), y = coef, group=period_dummy, 
                   color = period_dummy), 
               position=position_dodge(0.3)) + 
    #geom_line(aes(x = fct_reorder(factor(period), coef_id), y = coef, group=1), 
    #          position=position_dodge(0.3), linetype = 'dashed', color = 'navy')+
    labs(x='Year since Dam Commissioned', y=label_depvar[i], color='Period')+
    geom_hline(aes(yintercept=0), color="black", linetype = "dashed")+
    facet_wrap(~downstream, ncol = 2, nrow=1) +
    #theme(panel.background = element_blank())+
    #ylim(-0.05, 0.045)+
    scale_color_manual(values = c('grey35','firebrick'),
                       labels = c('Pre', 'Post'))+
    theme_classic(base_size = 15)+
    theme(legend.position = "bottom", 
          legent.text = element_text(size=rel(0.5)),
          axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
          axis.title=element_text(size=20),
          axis.text=element_text(size=15),
          strip.text.x = element_text(size = 20))
  
  ## Save
  ggsave(paste('Figure/event_study/AEA_slides/', depvar_baseline[i],'_30000.pdf', sep = ''), 
         es_plot, device = 'pdf', width = 12, height = 6, units = 'in')
  
}













###################################### Water Mechanism  ########################################  

###### Water Deficit, Drought, Severe Drought ######
tab <- list()
tab_mean <- c()

for (i in c(11,4,5)){
  ## formular
  fml1.1 <- paste(depvar_mech[i], '~', indvar[1], '+', controls_mech[2], '|', fes_mech[3])
  
  ## regression
  result1.1 <- feols(eval(parse(text = fml1.1)),
                     data = main_sample, 
                     subset = as.formula(subset1),
                     #fsplit = ~ cropland_dummy,
                     cluster = ~ cell,
                     nthreads = 12)
  
  tab <- c(tab, list(result1.1))
  
  ## Mean
  attach(main_sample)
  depvar <- get(depvar_mech[i])
  conditions <- Treat==1 & paired == 1 & Dam_Year %in% c(2001:2020) & dist_rivers_m <= 30000
  
  mean1 <- round(mean(depvar[conditions & downstream_segment==1], na.rm=T), 3)
  tab_mean <- c(tab_mean, mean1)
  
}

##### Groundwater by cropland #####
### formular
fml1.1 <- paste(depvar_mech[10], '~', indvar[1], '+', controls_mech[2], '|', fes_mech[3])

### regression
result1.1 <- feols(eval(parse(text = fml1.1)),
                   data = main_sample, 
                   subset = as.formula(subset1),
                   fsplit = ~ cropland_dummy,
                   cluster = ~ cell,
                   nthreads = 12)

## Mean
attach(main_sample)
depvar <- get(depvar_mech[10])
conditions <- Treat==1 & paired == 1 & Dam_Year %in% c(2001:2020) & dist_rivers_m <= 30000

mean1 <- round(mean(depvar[conditions & downstream_segment==1], na.rm=T), 3)
mean2 <- round(mean(depvar[conditions & downstream_segment==1 & cropland_dummy==0], na.rm=T), 3)
mean3 <- round(mean(depvar[conditions & downstream_segment==1 & cropland_dummy==1], na.rm=T), 3)

tab_mean <- c(tab_mean, mean1, mean2, mean3)


##### Regression table #####
etable(tab, result1.1, 
       title = 'The Effect of Hydropower Dams on Downstream Water Cycle', 
       headers = list(" "=3, "All"=1, "Non-Cropland"=1, 'Cropland'=1),
       keep = '%affected_riv|post2|ring2|ethnicfrac_cell_high|ethnicfrac_dam_high',
       order = c("Post"), dict = dict,
       extralines = list('_^Mean of Dep. Var.'= tab_mean,
                         '_Covariates*Year' = c('Y','Y','Y','Y','Y','Y')),
       fitstat= ~ n+r2, depvar = T, placement = 'h', tpt=T, adjustbox=1,
       page.width = "fit", tex = T, style.tex = style.tex("aer"), 
       digits = 3, digits.stats = 3,
       label = 'tab_water_30000', replace = T,
       file = 'Latex Table/AEA_slides/water_30000.tex')


##### Event Study #####

depvar_label2 <- c('log(1+NPP)', 'Soil Moisture (std.)', 'log(1+Soil Moisture)',
                   'Palmer Drought Severity Index', 'Incidence of Severe Drought',
                   'Water Deficit (std.)', 'log(1+Water Deficit)', 'Temperature Max.',
                   'Temperature Min.', 'log(Groundwater Storage)', 'Water Deficit',
                   'Soil Moisture', 'NPP', 'High Temperature', 'Low Temperature',
                   'Non-mild Temperature', 'Groundwater Storage')


##### Water Deficit, PDSI, Severe Drought #####
for (i in c(11,4,5)){
  ## formular
  fml1.1 <- paste(depvar_mech[i], '~', indvar[2], '+', controls_mech[2], '|', fes_mech[4])
  
  subset1 <- paste0("~ dist_rivers_m <= 30000 & Treat==1 & paired == 1 & Dam_Year %in% c(2001:2020) & 
                            downstream_segment==1")
  
  ## regression
  result1.1 <- feols(eval(parse(text = fml1.1)),
                     data = main_sample, 
                     subset = as.formula(subset1),
                     #split = ~ ring2_fct,
                     cluster = ~ cell,
                     nthreads = 12)
  
  result1_df <- cbind(data.frame(result1.1$coefficients), data.frame(result1.1$se))
  result1_df <- data.frame(result1_df[grepl('affected_riv:period_label', 
                                            rownames(result1_df))==T,])
  result1_df <- data.frame(result1_df[grepl('affected_riv:period_label', 
                                            rownames(result1_df))==T,])
  names(result1_df) <- c('coef', 'se')
  result1_df$period <- str_sub(rownames(result1_df), nchar('affected_riv:period_label')+1,-1)
  result1_df[nrow(result1_df) + 1,] <- c(0, 0, '-1')
  result1_df$coef_id <- ifelse(result1_df$period=='-5+', 1,
                               ifelse(result1_df$period=='9+', 15, 
                                      as.numeric(result1_df$period)+6))
  result1_df$coef <- as.numeric(result1_df$coef)
  result1_df$se <- as.numeric(result1_df$se)
  
  result1_df <- result1_df %>%
    mutate(period_dummy = factor(ifelse(period<0, 0, 1))) 

  ## Graph
  es_plot <- result1_df %>% ggplot()+ 
    geom_segment(aes(x = fct_reorder(factor(period), coef_id), 
                     xend = fct_reorder(factor(period), coef_id),
                     y=coef - 1.96*se, yend=coef + 1.96*se, group=period_dummy, color = period_dummy), 
                 size=0.5, linewidth = 0.5, position=position_dodge(0.3))+
    geom_point(aes(x = fct_reorder(factor(period), coef_id), y = coef, group=period_dummy, 
                   color = period_dummy), 
               position=position_dodge(0.3)) + 
    #geom_line(aes(x = fct_reorder(factor(period), coef_id), y = coef, group=1), 
    #          position=position_dodge(0.3), linetype = 'dashed', color = 'navy')+
    labs(x='Year since Dam Commissioned', y=depvar_label2[i], color = 'Period')+
    geom_hline(aes(yintercept=0), color="black", linetype = "dashed")+
    #facet_wrap(~downstream, ncol = 2, nrow=1) +
    #theme(panel.background = element_blank())+
    #ylim(-0.05, 0.045)+
    scale_color_manual(values = c('grey35','firebrick'),
                       labels = c('Pre', 'Post'))+
    theme_classic(base_size = 15)+
    theme(legend.position = "bottom", 
          legent.text = element_text(size=rel(0.5)),
          axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
          axis.title=element_text(size=20),
          axis.text=element_text(size=15))
  
  ## Save
  ggsave(paste('Figure/event_study/AEA_slides/', depvar_mech[i],'_30000.pdf', sep = ''), 
         es_plot, device = 'pdf', width = 6, height = 6, units = 'in')
  
}



##### Groundwater Storage by Cropland #####

subset3 <- paste0("~ dist_rivers_m <= 30000 & Treat==1 & paired == 1 & Dam_Year %in% c(2003:2020) & 
                            downstream_segment==1")

## formular
fml1.1 <- paste(depvar_mech[10], '~', indvar[2], '+', controls_mech[2], '|', fes_mech[4])

## regression
result1.1 <- feols(eval(parse(text = fml1.1)),
                   data = main_sample, 
                   subset = as.formula(subset3),
                   #fsplit = ~ cropland_dummy,
                   cluster = ~ cell,
                   nthreads = 12)

etable(result1.1, keep = '%affected_riv')

# Non-Cropland
result1_df <- cbind(data.frame(result1.1$coefficients), data.frame(result1.1$se))
result1_df <- data.frame(result1_df[grepl('affected_riv:period_label', 
                                          rownames(result1_df))==T,])
result1_df <- data.frame(result1_df[grepl('affected_riv:period_label', 
                                          rownames(result1_df))==T,])
names(result1_df) <- c('coef', 'se')
result1_df$period <- str_sub(rownames(result1_df), nchar('affected_riv:period_label')+1,-1)
result1_df[nrow(result1_df) + 1,] <- c(0, 0, '-1')
result1_df$coef_id <- ifelse(result1_df$period=='-5+', 1,
                             ifelse(result1_df$period=='9+', 15, 
                                    as.numeric(result1_df$period)+6))
result1_df$coef <- as.numeric(result1_df$coef)
result1_df$se <- as.numeric(result1_df$se)

result1_df <- result1_df %>%
  mutate(period_dummy = factor(ifelse(period<0, 0, 1))) %>%
  mutate(cropland_dummy = 'Non-Cropland')

# Cropland
result2_df <- cbind(data.frame(result1.1[[2]]$coefficients), data.frame(result1.1[[2]]$se))
result2_df <- data.frame(result2_df[grepl('affected_riv:period_label', 
                                          rownames(result2_df))==T,])
names(result2_df) <- c('coef', 'se')
result2_df$period <- str_sub(rownames(result2_df), nchar('affected_riv:period_label')+1,-1)
result2_df[nrow(result2_df) + 1,] <- c(0, 0, '-1')
result2_df$coef_id <- ifelse(result2_df$period=='-5+', 1,
                             ifelse(result2_df$period=='9+', 15, 
                                    as.numeric(result2_df$period)+6))
result2_df$coef <- as.numeric(result2_df$coef)
result2_df$se <- as.numeric(result2_df$se)
result2_df <- result2_df %>%
  mutate(period_dummy = factor(ifelse(period<0, 0, 1))) %>%
  mutate(cropland_dummy = 'Cropland')

es_table <- rbind(result1_df, result2_df)


## Graph
es_plot <- result1_df %>% ggplot()+ 
  geom_segment(aes(x = fct_reorder(factor(period), coef_id), 
                   xend = fct_reorder(factor(period), coef_id),
                   y=coef - 1.96*se, yend=coef + 1.96*se, group=period_dummy, color = period_dummy), 
               size=0.5, linewidth = 0.5, position=position_dodge(0.3))+
  geom_point(aes(x = fct_reorder(factor(period), coef_id), y = coef, group=period_dummy, 
                 color = period_dummy), 
             position=position_dodge(0.3)) + 
  #geom_line(aes(x = fct_reorder(factor(period), coef_id), y = coef, group=1), 
  #          position=position_dodge(0.3), linetype = 'dashed', color = 'navy')+
  labs(x='Year since Dam Commissioned', y=depvar_label2[10], color='Period')+
  geom_hline(aes(yintercept=0), color="black", linetype = "dashed")+
  #facet_wrap(~cropland_dummy, ncol = 2, nrow=1) +
  #theme(panel.background = element_blank())+
  #ylim(-0.05, 0.045)+
  scale_color_manual(values = c('grey35','firebrick'),
                     labels = c('Pre', 'Post'))+
  theme_classic(base_size = 15)+
  theme(legend.position = "bottom", 
        legent.text = element_text(size=rel(0.5)),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        axis.title=element_text(size=20),
        axis.text=element_text(size=15))

## Save
ggsave(paste('Figure/event_study/AEA_slides/lngws_30000.pdf', sep = ''), 
       es_plot, device = 'pdf', width = 6, height = 6, units = 'in')





















###################################### Temperature Mechanism  ########################################  

###### TempMax, TempMin, hightemp, lowtemp, severetemp ######
tab <- list()
tab_mean <- c()

for (i in c(8:9, 14:16)){
  ## formular
  fml1.1 <- paste(depvar_mech[i], '~', indvar[1], '+', controls_mech[2], '|', fes_mech[4])
  
  ## regression
  result1.1 <- feols(eval(parse(text = fml1.1)),
                     data = main_sample, 
                     subset = as.formula(subset1),
                     #fsplit = ~ cropland_dummy,
                     cluster = ~ cell,
                     nthreads = 12)
  
  tab <- c(tab, list(result1.1))
  
  ## Mean
  attach(main_sample)
  depvar <- get(depvar_mech[i])
  conditions <- Treat==1 & paired == 1 & Dam_Year %in% c(2001:2020) & dist_rivers_m <= 30000
  
  mean1 <- round(mean(depvar[conditions & downstream_segment==1], na.rm=T), 3)
  tab_mean <- c(tab_mean, mean1)
}


##### Regression table #####
etable(tab, 
       title = 'The Effect of Hydropower Dams on Downstream Temperature', 
       #headers = list("Temprerature"=2, "Incidence of"=3),
       keep = '%affected_riv|post2|ring2|ethnicfrac_cell_high|ethnicfrac_dam_high',
       order = c("Post"), dict = dict,
       extralines = list('_^Mean of Dep. Var.'= tab_mean,
                         '_Covariates*Year' = c('Y','Y','Y','Y','Y')),
       fitstat= ~ n+r2, depvar = T, placement = 'h', tpt=T, adjustbox=1,
       page.width = "fit", tex = T, style.tex = style.tex("aer"), 
       digits = 3, digits.stats = 3,
       label = 'tab_temperature_30000', replace = T,
       file = 'Latex Table/AEA_slides/temperature_30000.tex')






###################################### Agricultural Mechanism  ########################################  

### change in cropland
main_sample_year <- main_sample %>%
  dplyr::select(dam_id:year, HYBAS_ID:num_ethnic_cell, Dam_Year:Sign_Month,
                ring:cropland_dummy, Npp:num_mine, rainfall_annual, lon, lat,
                lnNpp, arid:ring2_fct) %>%
  unique() %>%
  dplyr::mutate(post2 = ifelse(year>Dam_Year, 1, 0),
                period = as.numeric(year-Dam_Year)) %>%
  mutate(period_label = ifelse(as.numeric(period)>-5 & as.numeric(period)<9, 
                               period,
                               ifelse(as.numeric(period)>=9, '9+', '-5+'))) %>%
  mutate(period_label = relevel(as.factor(period_label), ref ='-1'))

library(modeest)

### switching cell
main_sample_switch <- main_sample %>%
  mutate(cropland_5yr_pre_dam = ifelse(period>-5 & period<0, cropland_dummy, NA),
         cropland_10yr_post_dam = ifelse(period>=0 & period<10, cropland_dummy, NA)) %>%
  group_by(dam_id, cell) %>% # Group again to calculate the mode
  mutate(cropland_5yr_pre_dam_mode = mfv(cropland_5yr_pre_dam, na.rm = TRUE)[1],
         cropland_10yr_post_dam_mode = mfv(cropland_10yr_post_dam, na.rm = TRUE)[1]) %>% # Calculate the mode
  ungroup()
  
main_sample_switch <- main_sample_switch %>%
  mutate(
    switching_direction = case_when(
      #post2==0 ~ NA,
      cropland_10yr_post_dam_mode == 1 & cropland_5yr_pre_dam_mode == 0 ~ 1,  # Non-cropland to cropland
      cropland_10yr_post_dam_mode == 0 & cropland_5yr_pre_dam_mode == 1 ~ -1, # Cropland to non-cropland
      cropland_10yr_post_dam_mode==cropland_5yr_pre_dam_mode ~ 0                                          # No change
    ))
  


### regression
subset1 <- paste0("~ dist_rivers_m <= 30000 & Treat==1 & paired == 1 & Dam_Year %in% c(2001:2020) & 
                            downstream_segment==1")
subset3 <- paste0("~ dist_rivers_m <= 30000 & Treat==1 & paired == 1 & Dam_Year %in% c(2001:2020) & 
                            downstream_segment==1 & year>=2001")

result1.1 <- feols(cropland_dummy ~ affected_riv*post2+log(PopCount+1)+log(rainfall_annual+1)+
                     factor(climates_f):factor(year)+factor(Lithology):factor(year)+
                     factor(GWSTOR_V2):factor(year)+log(dist_dams_m)+log(dist_rivers_m)|
                     factor(dam_id)+factor(cell)+factor(ADM0_NAME)^factor(year),
                   data = main_sample_year,
                   subset = as.formula(subset3),
                   cluster = ~cell)

fml1.1 <- paste(depvar_mech[12], '~', indvar[1], '+', controls_mech[2], '|', fes_mech[3])

result1.2 <- feols(eval(parse(text = fml1.1)),
                   data = main_sample, 
                   subset = as.formula(subset1),
                   fsplit = ~ cropland_dummy,
                   cluster = ~ cell,
                   nthreads = 12)

result1.3 <- feols(Npp ~ affected_riv*post2+log(PopCount+1)+log(rainfall_annual+1)+
                     factor(climates_f):factor(year)+factor(Lithology):factor(year)+
                     factor(GWSTOR_V2):factor(year)+log(dist_dams_m):factor(year)+
                     log(dist_rivers_m):factor(year)|
                     factor(dam_id)+factor(cell)+factor(year),
                   data = main_sample_year,
                   fsplit = ~cropland_dummy,
                   subset = as.formula(subset3),
                   cluster = ~cell)

## Mean
attach(main_sample_year)
conditions <- Treat==1 & paired == 1 & Dam_Year %in% c(2001:2020) & dist_rivers_m <= 30000 & downstream_segment==1
mean1 <- round(mean(cropland_dummy[conditions & year>=2001], na.rm=T), 3)
mean5 <- round(mean(Npp[conditions & year>=2001], na.rm=T), 3)
mean6 <- round(mean(Npp[conditions & year>=2001 & cropland_dummy==0], na.rm=T), 3)
mean7 <- round(mean(Npp[conditions & year>=2001 & cropland_dummy==1], na.rm=T), 3)
attach(main_sample)
mean2 <- round(mean(soilmoisture[conditions & downstream_segment==1], na.rm=T), 3)
mean3 <- round(mean(soilmoisture[conditions & downstream_segment==1 & cropland_dummy==0], na.rm=T), 3)
mean4 <- round(mean(soilmoisture[conditions & downstream_segment==1 & cropland_dummy==1], na.rm=T), 3)

tab_mean <- c(mean1, mean2, mean3, mean4, mean5, mean6, mean7)

etable(result1.1, result1.2, result1.3,
       title = 'The Effect of Hydropower Dams on Downstream Water Cycle', 
       headers = list('All'=1,"All"=1, "Non-Cropland"=1, 'Cropland'=1, 
                      "All"=1, "Non-Cropland"=1, 'Cropland'=1),
       keep = '%post2|ring2|ethnicfrac_cell_high|ethnicfrac_dam_high',
       order = c("Post"), dict = dict,
       extralines = list('_^Mean of Dep. Var.'= tab_mean,
                         '_Covariates*Year' = c('Y','Y','Y','Y','Y','Y','Y')),
       fitstat= ~ n+r2, depvar = T, placement = 'h', tpt=T, adjustbox=1,
       page.width = "fit", tex = T, style.tex = style.tex("aer"), 
       digits = 3, digits.stats = 3,
       label = 'tab_agric_30000', replace = T,
       file = 'Latex Table/AEA_slides/agric_30000.tex')





#### switching

subset1 <- paste0("~ dist_rivers_m <= 30000 & Treat==1 & paired == 1 & Dam_Year %in% c(2001:2020) & 
                            downstream_segment==1")

fml1.3 <- paste(depvar_baseline[1], '~', indvar[1], '+', controls_baseline[1], '|', fes_baseline[2])

## regression
result1.1 <- feols(eval(parse(text = fml1.3)),
                   data = main_sample_switch, 
                   subset = as.formula(subset1),
                   split = ~ switching_direction,
                   cluster = ~ cell,
                   nthreads = 12)

etable(result1.1, keep='%affected_riv|post2')


