## Purpose: Summarise structure and loss within PACs
## Project: Mature Forest Decline
## Upstream: spat_anal.R
## Downstream: pac_mods.R

pac_desc <- function(path) 
{
  library(tidyverse)
  library(terra)
  
  ## conifer raster
  con11 = rast(paste0(path,'/ss_con2011.tif'))
  
  ## pac shp
  pac <- vect(paste0(path, '/pac_sa.shp')) %>% 
    project(crs(con11))
  
  ## How much is in conifer? 85%
  pac.con <- mask(con11, pac)

  fpc <- freq(pac.con)
  fpc[2,'count'] / sum(fpc[,'count'])
  
  ## cover and height rasters
  cc11 <- rast(paste0(path, '/ss_cc2011.tif')) %>% 
    ## mask out non-conifer areas
    mask(con11)
  ht11 = rast(paste0(path, '/ss_ht2011_0.tif')) %>% 
    mask(con11)
  cc20 = rast(paste0(path, '/cc_2020.tif'))
  ## degraded cc
  cc_deg = cc11 - cc20
  
  ## Summarise
  pac.cc11 <- mask(cc11, pac)
  pac.cc20 <- mask(cc20, pac)
  pac.cc.deg <- mask(cc_deg, pac)
  pac.ht11 <- mask(ht11, pac)
  
  ## Note that heights are still in feet here!
  d <- data.frame(extent = rep(c('all', 'pac'), 8),
                  metric = rep(c(rep('cc', 4), 'cc_deg', 'cc_deg', 'ht_ft', 'ht_ft'), 2), 
                  year = rep(c(2011, 2011, 2020, 2020, 2020, 2020, 2011, 2011), 2), 
                  stat = c(rep('mean', 8), rep('median', 8)),
                  value = c(global(cc11, 'mean', na.rm = T)[1,1], 
                            global(pac.cc11, 'mean', na.rm = T)[1,1],
                            global(cc20, 'mean', na.rm = T)[1,1], 
                            global(pac.cc20, 'mean', na.rm = T)[1,1],
                            global(cc_deg, 'mean', na.rm = T)[1,1], 
                            global(pac.cc.deg, 'mean', na.rm = T)[1,1],
                            global(ht11, 'mean', na.rm = T)[1,1],
                            global(pac.ht11, 'mean', na.rm = T)[1,1]
                            ))
  

  ## percentages in classes were calculated in spat_anal.R
  
  write_csv(d, 'results/pac_desc.csv')
  
  ## Let's also sample values using random non-pac areas
  nonpac = vect(paste0(path,'/nonpac_areas.shp'))
  
  d2 <- extract(c(cc11, ht11, cc20, cc_deg), nonpac, fun = 'mean', na.rm = T) %>% 
    dplyr::select('cc11' = 2, 'ht11' = 3, 'cc20' = 4, 'cc_deg' = 5) %>% 
    mutate(class = 'nonpac')
  d2.1 <- extract(c(cc11, ht11, cc20, cc_deg), pac, fun = 'mean', na.rm = T) %>% 
    dplyr::select('cc11' = 2, 'ht11' = 3, 'cc20' = 4, 'cc_deg' = 5) %>% 
    mutate(class = 'pac')
  
  ## save for modeling and plotting
  bind_rows(d2, d2.1) %>% 
    write_csv("results/pac_sample_data.csv")
  
}
