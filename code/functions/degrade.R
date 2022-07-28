## Purpose: Quantify forest degradation 2011-2020
## Project: Mature Forest Decline
## Upstream: dashboard.R
## Downstream: plot_deg.R; spat_anal.R; plot_wifri.R

degrade = function(path #user defined path to spatial files
                    ) 
{
  library(tidyverse)
  library(terra)
  library(sf)
  
  ## Read in first cc layer for projection
  cc11 = rast(paste0(path, '/cc_2011.tif'))
  
  ## Read in SSierra Scenes and creates study area
  sa = vect(paste0(path, '/ss_scenes.shp')) %>% 
    project(crs(cc11))
  
  ## Bring in fire perimeter data
  pers = vect(paste0(path, '/firep11_20_2.shp')) %>% 
    project(crs(sa)) %>% 
    buffer(width = 0) %>%
    crop(sa)
  rm(sa)
  pers.sf = st_as_sf(pers) %>% 
    group_by(year = YEAR_) %>% 
    summarise()
  rm(pers)
  
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
  
  ## Function to calculate area within the reference FRI
  f2 = function(x, low, high, oldr = NULL, cc_year = 2011) {
    tmp = classify(x, rbind(c(0,low,0), #below thrsholds
                            c(low,high+1,1), #within range
                            c(high+1, 101, 0))) #above
    if(!is.null(oldr)) {tmp = tmp * oldr}
    ## pull ref fri condition as of Jan 1 of the next year
    wi_year = cc_year + 1
    ## bring in binary within (1) rfri or greater than (0)
    rfri = rast(
      paste0(path, "/ss_wi_2mnrfri_",
             wi_year, ".tif")) %>% 
      project(tmp, method = 'near')
    ## Get where within forest and rfri
    tmp_wi = tmp * rfri
    
    freq(tmp_wi) %>% 
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
  
  ## Everything above 40% is potentially mature; will split later
  
  cc_old = classify(cc11, 
                    rbind(c(0,40,0), c(40,101,1)))
  
  old = ht_old * cc_old
  
  
  ## moderate density mature
  old_mm = classify(cc11, 
                    rbind(c(0,40,0), c(40,61,1), c(61,101,0))) * old
  
  ## high density mature
  old_hm = classify(cc11, 
                   rbind(c(0,61,0), c(61,101,1))) * old

  
  ## Save for later 
  writeRaster(old, paste0(path,'/mature11.tif'), overwrite = T)
  writeRaster(old_mm, paste0(path, '/mmature11.tif'), overwrite = T)
  writeRaster(old_hm, paste0(path, '/hmature11.tif'), overwrite = T)
  
  ## Need a big sample because of many NAs
  ds = spatSample(cc11_for, 50000, method = "regular", 
                  as.points = T, na.rm = T)
  names(ds) = "cc2011"
  ds_old = spatSample(cc11_old, 500000, method = "regular", 
                      as.points = T, na.rm = T)
  names(ds_old) = "cc2011_old"
  
  
  samples11 = extract(cc11, ds) %>% 
    rename(conifer = 2) %>%
    mutate(year = 2011) %>% 
    pivot_longer(cols = conifer, names_to = "class", values_to = "cc")
  
  samples11_old = extract(cc11, ds_old) %>% 
    rename(mature = 2) %>%
    mutate(year = 2011) %>% 
    pivot_longer(cols = mature, names_to = "class", values_to = "cc")
  
  ## Start sample dataframe
  d2 = bind_rows(samples11, samples11_old)
  
  ## Calculate areas
  for_ha = f1(cc11, low = 25, high = 100)
  old_ha = f1(cc11, low = 40, high = 100, oldr = old)
  mm_ha = f1(cc11, low = 40, high = 60, oldr = old)
  hm_ha = f1(cc11, low = 61, high = 100, oldr = old)
  
  for_rfri = f2(cc11, low = 25, high = 100, cc_year = 2011)
  old_rfri = f2(cc11, low = 40, high = 100, oldr = old, cc_year = 2011)
  mm_rfri = f2(cc11, low = 40, high = 60, oldr = old, cc_year = 2011)
  hm_rfri = f2(cc11, low = 61, high = 100, oldr = old, cc_year = 2011)
  
  ## Set up dataframe to populate
  d = data.frame(year = 2011, 
                 for_ha = for_ha, 
                 mature_ha = old_ha,
                 mmature_ha = mm_ha,
                 hmature_ha = hm_ha,
                 for_rfri = for_rfri,
                 mature_rfri = old_rfri,
                 mmature_rfri = mm_rfri,
                 hmature_rfri = hm_rfri,
                 for_loss = NA, 
                 mature_loss = NA,
                 mmature_loss = NA,
                 hmature_loss = NA,
                 for_bloss = NA, 
                 mature_bloss = NA,
                 mmature_bloss = NA,
                 hmature_bloss = NA)
  
  ## clean up RAM
  gc()
  
  ## Years to iterate through
  years = 2012:2020
  
  ## Loop through each year and subtract cc loss
  ## Takes about an hour
  for(year in years) {
    
    print(year)
    
    ## Previous & current year canopy cover
    cc_prev = rast(paste0(path, '/cc_', year-1, '.tif'))
    cc = rast(paste0(path, '/cc_', year, '.tif'))
    
    ## Change
    mmi = cc_prev - cc
    
    ## Subset burned area
    burn = filter(pers.sf, year == {{year}})
    mmi_b = mask(mmi, vect(burn)) 
    cc_prev_b = mask(cc_prev, vect(burn))
    cc_b = mask(cc, vect(burn))
    
    ## Calculate current area in each category
    fa_new = f1(cc, low = 25, high = 100)
    ma_new = f1(cc, low = 40, high = 100, oldr = old)
    mma_new = f1(cc, low = 40, high = 60, oldr = old)
    hma_new = f1(cc, low = 61, high = 100, oldr = old)
    
    ## Get area within rfri
    for_rfri_new = f2(cc, low = 25, high = 100, cc_year = year)
    old_rfri_new = f2(cc, low = 40, high = 100, oldr = old, cc_year = year)
    mm_rfri_new = f2(cc, low = 40, high = 60, oldr = old, cc_year = year)
    hm_rfri_new = f2(cc, low = 61, high = 100, oldr = old, cc_year = year)
    
    ## Get area lost 
    fa_lost = d[d$year == year - 1, "for_ha"] - fa_new
    ma_lost = d[d$year == year - 1, "mature_ha"] - ma_new
    mma_lost = d[d$year == year - 1, "mmature_ha"] - mma_new
    hma_lost = d[d$year == year - 1, "hmature_ha"] - hma_new
    fa_bl = f1(cc_prev_b, low = 25, high = 100) - f1(cc_b, low = 25, high = 100)
    ma_bl = f1(cc_prev_b, low = 40, high = 100, oldr = old) - f1(cc_b, low = 40, high = 100, oldr = old)
    mma_bl = f1(cc_prev_b, low = 40, high = 60, oldr = old) - f1(cc_b, low = 40, high = 60, oldr = old)
    hma_bl = f1(cc_prev_b, low = 61, high = 100, oldr = old) - f1(cc_b, low = 61, high = 100, oldr = old)
    
    ## some issues with zero values
    if(length(ma_bl) == 0) {ma_bl = 0} 
    if(length(mma_bl) == 0) {mma_bl = 0}
    if(length(hma_bl) == 0) {hma_bl = 0}
    
    d = bind_rows(d, data.frame(year = year, 
                                for_ha = fa_new, 
                                mature_ha = ma_new,
                                mmature_ha = mma_new,
                                hmature_ha = hma_new,
                                for_rfri = for_rfri_new, 
                                mature_rfri = old_rfri_new,
                                mmature_rfri = mm_rfri_new,
                                hmature_rfri = hm_rfri_new,
                                for_loss = fa_lost, 
                                mature_loss = ma_lost,
                                mmature_loss = mma_lost,
                                hmature_loss = hma_lost,
                                for_bloss = fa_bl, 
                                mature_bloss = ma_bl,
                                mmature_bloss = mma_bl,
                                hmature_bloss = hma_bl))
    print(d)
    
    ## Save some samples
    samples = extract(cc, ds) %>% 
      rename(conifer = 2) %>%
      mutate(year = year) %>% 
      pivot_longer(cols = conifer, names_to = "class", values_to = "cc")
    
    samples_old = extract(cc, ds_old) %>% 
      rename(mature = 2) %>%
      mutate(year = year) %>% 
      pivot_longer(cols = mature, names_to = "class", values_to = "cc")
    
    d2 = bind_rows(d2, samples, samples_old)
    
    ## clean house
    gc()
    
  }
  
  write_csv(d, 'results/ann_chg.csv')
  write_csv(d2, 'results/samples.csv')
  
}
