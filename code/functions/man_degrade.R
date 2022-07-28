## Purpose: Quantify forest degradation 2011-2019 in managed areas
## Project: Mature Forest Decline
## Upstream: dashboard.R
## Downstream: plot_wifri.R

man_degrade = function(path) 
{
  library(tidyverse)
  library(terra)
  library(sf)

  ## Read in first cc layer for projection
  cc11 = rast(paste0(path, '/cc_2011.tif'))
  
  ## Read in SSierra Scenes and creates study area
  sa = vect(paste0(path, '/ss_scenes.shp')) %>% 
    project(crs(cc11))
  
  ## Bring in management data
  man = read_sf(paste0(path, '/man_simple12_19.shp')) %>% 
    st_transform(crs = st_crs(cc11))
  
  
  ## Function to calculate forest and old forest area
  f1 = function(x, low, high, oldr = NULL) {
    tmp = classify(x, rbind(c(0,low,0), #below thrsholds
                            c(low,high+1,1), #within range
                            c(high+1, 101, 0))) #above
    if(!is.null(oldr)) {tmp = tmp * oldr}
    freq(tmp) %>% 
      as.data.frame() %>% 
      filter(value == 1) %>% 
      pull(count) * 900 / 10000
  }
  
  ## Bring in height
  ht11 = rast('local/ss_ht2011.tif') %>%
    mask(cc11) #only conifer (previously mased in cc_subtract)
  
  
  ## convert meters to feet for height
  ht_ft = 30 * 3.28
  
  ## define mature forests
  ## default is right = T, which means interval is 'closed' on the right (does not include the last value)
  ## in this case ht_ft is not included in the first interval, but is in the second; thus 1 indicates >= ht_ft
  ht_old = classify(ht11, 
                    rbind(c(0,ht_ft,0), c(ht_ft,200,1)))
  
  ## Everything above 40% is potentially old; will split later
  
  cc_old = classify(cc11, 
                    rbind(c(0,40,0), c(40,101,1)))
  
  old = ht_old * cc_old
  
  
  ## moderate density mature
  old_mm = classify(cc11, 
                    rbind(c(0,40,0), c(40,61,1), c(61,101,0))) * old
  
  ## high density mature
  old_hm = classify(cc11, 
                   rbind(c(0,61,0), c(61,101,1))) * old
  
  ## Calculate areas
  for_ha = f1(cc11, low = 25, high = 100)
  old_ha = f1(cc11, low = 40, high = 100, oldr = old)
  mm_ha = f1(cc11, low = 40, high = 60, oldr = old)
  hm_ha = f1(cc11, low = 61, high = 100, oldr = old)
  
  ## Set up dataframe to populate
  d = data.frame(year = 2011, 
                 for_ha = for_ha, 
                 mature_ha = old_ha,
                 mmature_ha = mm_ha,
                 hmature_ha = hm_ha,
                 for_loss = NA, 
                 mature_loss = NA,
                 mmature_loss = NA,
                 hmature_loss = NA,
                 for_mloss = NA, 
                 mature_mloss = NA,
                 mmature_mloss = NA,
                 hmature_mloss = NA)
  
  ## clean up RAM
  gc()
  
  ## Years to iterate through (2020 management data is incomplete)
  years = 2012:2019
  
  ## Loop through each year and subtract cc loss
  ## Takes about a half hour
  for(year in years) {
    
    print(year)
    
    ## Previous & current year canopy cover
    cc_prev = rast(paste0(path, '/cc_', year-1, '.tif'))
    cc = rast(paste0(path, '/cc_', year, '.tif'))
    
    ## Change
    mmi = cc_prev - cc
    
    ## Subset management area
    man = filter(man, year == {{year}})
    mmi_m = mask(mmi, vect(man)) 
    cc_prev_m = mask(cc_prev, vect(man))
    cc_m = mask(cc, vect(man))
    
    ## Calculate current area in each category
    fa_new = f1(cc, low = 25, high = 100)
    ma_new = f1(cc, low = 40, high = 100, oldr = old)
    mma_new = f1(cc, low = 40, high = 60, oldr = old)
    hma_new = f1(cc, low = 61, high = 100, oldr = old)
    
    ## Get area lost 
    fa_lost = d[d$year == year - 1, "for_ha"] - fa_new
    ma_lost = d[d$year == year - 1, "mature_ha"] - ma_new
    mma_lost = d[d$year == year - 1, "mmature_ha"] - mma_new
    hma_lost = d[d$year == year - 1, "hmature_ha"] - hma_new
    fa_ml = f1(cc_prev_m, low = 25, high = 100) - f1(cc_m, low = 25, high = 100)
    ma_ml = f1(cc_prev_m, low = 40, high = 100, oldr = old) - f1(cc_m, low = 40, high = 100, oldr = old)
    mma_ml = f1(cc_prev_m, low = 40, high = 60, oldr = old) - f1(cc_m, low = 40, high = 60, oldr = old)
    hma_ml = f1(cc_prev_m, low = 61, high = 100, oldr = old) - f1(cc_m, low = 61, high = 100, oldr = old)
    
    ## some issues with zero values
    if(length(ma_ml) == 0) {ma_ml = 0} 
    if(length(mma_ml) == 0) {mma_ml = 0}
    if(length(hma_ml) == 0) {hma_ml = 0}
    
    d = bind_rows(d, data.frame(year = year, 
                                for_ha = fa_new, 
                                mature_ha = ma_new,
                                mmature_ha = mma_new,
                                hmature_ha = hma_new,
                                for_loss = fa_lost, 
                                mature_loss = ma_lost,
                                mmature_loss = mma_lost,
                                hmature_loss = hma_lost,
                                for_mloss = fa_ml, 
                                mature_mloss = ma_ml,
                                mmature_mloss = mma_ml,
                                hmature_mloss = hma_ml))
    print(d)
    
    ## clean house
    gc()
    
  }
  
  write_csv(d, 'results/ann_chg_man.csv')
  

}
