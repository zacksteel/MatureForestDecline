## Purpose: Calculate spatial statistics of loss/degradation
## Project: Mature Forest Decline
## Upstream: degrade.R; 
## Downstream: loss_map.R; pac_desc.R

spat_anal = function(path)
{
  library(tidyverse)
  library(terra)
  library(sf)
  library(landscapemetrics)
  
  ## Get conifer raster and NA zeros
  con11 = rast(paste0(path, '/ss_con2011.tif'))
  con11[con11 == 0] = NA
  
  ## Get starting canopy cover & height
  ## Make sure mosaic_ss has been run w/ mmi mask of these rasters!
  cc11 <- rast(paste0(path,'/ss_cc2011.tif')) %>% 
    ## mask out non-conifer areas
    mask(con11)
  ht11 = rast(paste0(path, '/ss_ht2011.tif')) %>% 
    mask(con11)
  
  # cc20 = rast(paste0(path, '/results/cc_ss_2020.tif'))
  cc20 = rast(paste0(path, '/cc_2020.tif'))
  
  cc_deg = cc11 - cc20
  
  ## Create forest and old forest rasters
  ## convert meters to feet for height
  ht_ft = 30 * 3.28
  
  ## define mature forest classes
  ## default is right = T, which means interval is 'closed' on the right (does not include the last value)
  ## in this case ht_ft is not included in the first interval, but is in the second; thus 1 indicates >= ht_ft
  ht_old = classify(ht11, 
                    rbind(c(0,ht_ft,0), c(ht_ft,200,1)))
  
  ## Let's look at the mature sparse class
  cc11_sparse = classify(cc11,
                         rbind(c(0,25,0), c(25, 40, 1), c(40, 101, 0)))
  
  old11_sparse = ht_old * cc11_sparse
  
  ## Everything above 40% is potentially old; will split later
  cc11_old = classify(cc11, 
                    rbind(c(0,40,0), c(40,101,1)))
  
  old11 = ht_old * cc11_old
  
  
  ## moderate density mature
  old11_mm = classify(cc11, 
                    rbind(c(0,40,0), c(40,61,1), c(61,101,0))) * old11
  
  ## high density mature
  old11_hm = classify(cc11, 
                    rbind(c(0,61,0), c(61,101,1))) * old11
  
  
  ## all mature still in 2020
  old_still = classify(cc20, 
                     rbind(c(0,40,0), c(40,101,1))) * old11
  
  ## moderate density old in 2011 and still in 2020
  mm_still = classify(cc20, 
                      rbind(c(0,40,0), c(40,61,1), c(61,101,0))) * old11_mm
  
  ## including transitions
  old20_mm  = classify(cc20, 
                       rbind(c(0,40,0), c(40,61,1), c(61,101,0))) * old11

  ## what about the high-density class?
  hm_still = classify(cc20, 
             rbind(c(0,61,0), c(61,101,1))) * old11_hm
  
  old20_hm = classify(cc20, 
                      rbind(c(0,61,0), c(61,101,1))) * old11
  
  ## and just forest
  forest11 = classify(cc11, 
                      rbind(c(0,25,0), c(25,101,1)))
  
  forest20 = classify(cc20, 
                      rbind(c(0,25,0), c(25,101,1)))
  
  ## Bring in fire data and make burned/unburned raster
  pers = read_sf(paste0(path,'/firep11_20_2.shp')) %>% 
    filter(YEAR_ > 2011) %>% 
    st_transform(crs = 32611) %>% 
    vect() 

  pers.r = rasterize(pers, cc11, background = 0) %>% 
    mask(cc11)
  
  ## Get loss/degraded/transition patches
  ## value of 1 indicates areas of change
  loss = (forest20 - forest11) * -1
  deg = (old_still - old11) * -1
  deg_mm = (mm_still - old11_mm) * -1
  deg_hm = (hm_still - old11_hm) * -1
  tr_mm_lm = deg_mm * forest20 #where it's been degraded but still forest
  tr_hm_for = deg_hm * forest20
  tr_hm_mm = deg_hm * old20_mm #where its switched from high to moderate density
  tr_hm_lm = tr_hm_for - tr_hm_mm
  hm_loss = deg_hm - tr_hm_for #complete loss
  mm_loss = deg_mm - tr_mm_lm
  
  ## Calculate areas
  f1 = function(r) {
    out = freq(r) %>% 
      as.data.frame() %>% 
      mutate(ha = count * 0.09)
    return(out[out$value == 1,'ha'])
  }
  
  f1(old11_sparse)
  
  ## do the same for within PACs & FS lands
  pac <- vect(paste0(path, '/pac_sa.shp')) %>% 
    project(crs(cc11))
  fs <- read_sf(paste0(path,"/ownership15_1.shp")) %>% 
    filter(Own_Group == "USDA Forest Service") %>% 
    vect() %>% 
    project(crs(cc11))
  
  f2 <- function(r, m) {
    out = mask(r, m) %>% 
      freq() %>% 
      as.data.frame() %>% 
      mutate(ha = count * 0.09)
    return(out[out$value == 1,'ha'])
  }
  
  f2(old11_sparse, pac)
  
  
  d = data.frame(extent = 'full',
                 class = c('forest', 'forest', 
                           'mature', 'mature',
                           'mmature', 'mmature', 
                           'hmature', 'hmature',
                           'mm_lm', 'hm_mm', 'hm_lm', 'hm_for',
                           'mm_loss', 'hm_loss'),
                 year = c(rep(c(2011, 2020), 4),
                          rep(2020, 6)),
                 ha = c(f1(forest11), f1(forest20), 
                        f1(old11), f1(old_still),
                        f1(old11_mm), f1(old20_mm), 
                        f1(old11_hm), f1(old20_hm),
                        f1(tr_mm_lm), f1(tr_hm_mm), f1(tr_hm_lm), f1(tr_hm_for),
                        f1(mm_loss), f1(hm_loss))
                 )
  
  d2 <- data.frame(extent = 'pac',
                   class = c('forest', 'forest', 
                             'mature', 'mature',
                             'mmature', 'mmature', 
                             'hmature', 'hmature',
                             'mm_lm', 'hm_mm', 'hm_lm', 'hm_for',
                             'mm_loss', 'hm_loss'),
                   year = c(rep(c(2011, 2020), 4),
                            rep(2020, 6)),
                   ha = c(f2(forest11, pac), f2(forest20, pac), 
                          f2(old11, pac), f2(old_still, pac),
                          f2(old11_mm, pac), f2(old20_mm, pac), 
                          f2(old11_hm, pac), f2(old20_hm, pac),
                          f2(tr_mm_lm, pac), f2(tr_hm_mm, pac), f2(tr_hm_lm, pac), f2(tr_hm_for, pac),
                          f2(mm_loss, pac), f2(hm_loss, pac))
  )
  
  d3 <- data.frame(extent = 'fs',
                   class = c('forest', 'forest', 
                             'mature', 'mature',
                             'mmature', 'mmature', 
                             'hmature', 'hmature',
                             'mm_lm', 'hm_mm', 'hm_lm', 'hm_for',
                             'mm_loss', 'hm_loss'),
                   year = c(rep(c(2011, 2020), 4),
                            rep(2020, 6)),
                   ha = c(f2(forest11, fs), f2(forest20, fs), 
                          f2(old11, fs), f2(old_still, fs),
                          f2(old11_mm, fs), f2(old20_mm, fs), 
                          f2(old11_hm, fs), f2(old20_hm, fs),
                          f2(tr_mm_lm, fs), f2(tr_hm_mm, fs), f2(tr_hm_lm, fs), f2(tr_hm_for, fs),
                          f2(mm_loss, fs), f2(hm_loss, fs))
  )
  
  dd <- bind_rows(d, d3, d2)
  
  
  write_csv(dd, "results/loss_area_tab.csv")
  
  ## add in sparse mature
  sparse = data.frame(extent = c('full', 'pac', 'fs'),
                      class = rep('lmature', 3),
                      year = rep(2011, 3),
                      ha = c(f1(old11_sparse),
                             f2(old11_sparse, pac),
                             f2(old11_sparse, fs))
  )
  
  read_csv("results/loss_area_tab.csv") %>% 
    bind_rows(sparse) %>% 
    write_csv('results/loss_area_tab.csv')
  
  ## update w/ fire info
  ## 0 is no loss outside of fire areas, 10 no loss within fire areas
  ## 10 is loss outside fire areas, 11 is loss within 
  loss2 = pers.r * 10 + loss
  deg2 = pers.r * 10 + deg
  deg2_mm = pers.r * 10 + deg_mm
  deg2_hm = pers.r * 10 + deg_hm
  
  ## Calculate patch size and core area of loss/degrade areas
  patch_loss <- dplyr::bind_rows(
    lsm_p_area(loss2),
    lsm_p_core(loss2, edge_depth = 3)
  ) %>% 
    mutate(threshold = 'forest')
  
  patch_deg = dplyr::bind_rows(
    lsm_p_area(deg2),
    lsm_p_core(deg2, edge_depth = 3)
  ) %>% 
    mutate(threshold = 'mature')
  
  patch_deg_mm = dplyr::bind_rows(
    lsm_p_area(deg2_mm),
    lsm_p_core(deg2_mm, edge_depth = 3)
  ) %>% 
    mutate(threshold = 'mmature')
  
  patch_deg_hm = dplyr::bind_rows(
    lsm_p_area(deg2_hm),
    lsm_p_core(deg2_hm, edge_depth = 3)
  ) %>% 
    mutate(threshold = 'hmature')
  
  ## May want to show distributions of patch size and core area
  d <- bind_rows(patch_loss, patch_deg, patch_deg_mm, patch_deg_hm) %>% 
    mutate(fate = case_when(class == 0 ~ "no loss - no fire",
                            class == 1 ~ 'loss - no fire',
                            class == 10 ~ 'no loss - fire',
                            class == 11 ~ 'loss - fire'))
  
  ## large file (84 MB), save in a more efficient format or outside of git repo
  # write_csv(d, "data/results/patch_tab.csv")

  
  ## Let's look at aggregation index
  library(tictoc)
  tic()
  f11_ai = lsm_c_ai(forest11) %>% 
    mutate(group = ifelse(class == 1, 'forest', 'other'),
           year = '2011')
  toc()
  f20_ai = lsm_c_ai(forest20) %>% 
    mutate(group = ifelse(class == 1, 'forest', 'other'),
           year = '2020')
  o11_ai = lsm_c_ai(old11) %>% 
    mutate(group = ifelse(class == 1, 'mature', 'other'),
           year = '2011')
  o20_ai = lsm_c_ai(old_still) %>% 
    mutate(group = ifelse(class == 1, 'mature', 'other'),
           year = '2020')
  mm11_ai = lsm_c_ai(old11_mm) %>% 
    mutate(group = ifelse(class == 1, 'mmature', 'other'),
           year = '2011')
  mm20_ai = lsm_c_ai(mm_still) %>% 
    mutate(group = ifelse(class == 1, 'mmature', 'other'),
           year = '2020')
  hm11_ai = lsm_c_ai(old11_hm) %>% 
    mutate(group = ifelse(class == 1, 'hmature', 'other'),
           year = '2011')
  hm20_ai = lsm_c_ai(hm_still) %>% 
    mutate(group = ifelse(class == 1, 'hmature', 'other'),
           year = '2020')
  
  ## also look at remnent forest/mature forest within and outside of fires
  pers = read_sf('local/firep11_20_2.shp') %>% 
    filter(YEAR_ > 2011) %>% 
    st_transform(crs = 32611) %>% 
    vect() 
  
  pers.r = rasterize(pers, cc11, background = 0) %>% 
    mask(cc11)
  
  rf20 = pers.r * 10 + forest20
  ro20 = pers.r * 10 + old_still
  rmm20 = pers.r * 10 + mm_still
  rhm20 = pers.r * 10 + hm_still
  

  rf20_ai = lsm_c_ai(rf20) %>% # forest: no fire = 87.9; fire = 74.6
    mutate(group = case_when(class == 0 ~ "other - no fire",
                             class == 1 ~ 'forest - no fire',
                             class == 10 ~ 'other - fire',
                             class == 11 ~ 'forest - fire'),
           year = '2020')
  ro20_ai = lsm_c_ai(ro20) %>% # old: no fire = 75.0; fire = 64.1
    mutate(group = case_when(class == 0 ~ "other - no fire",
                             class == 1 ~ 'mature - no fire',
                             class == 10 ~ 'other - fire',
                             class == 11 ~ 'mature - fire'),
           year = '2020')
  rmm20_ai = lsm_c_ai(rmm20) %>% 
    mutate(group = case_when(class == 0 ~ "other - no fire",
                             class == 1 ~ 'mmature - no fire',
                             class == 10 ~ 'other - fire',
                             class == 11 ~ 'mmature - fire'),
           year = '2020')
  rhm20_ai = lsm_c_ai(rhm20) %>% 
    mutate(group = case_when(class == 0 ~ "other - no fire",
                             class == 1 ~ 'hmature - no fire',
                             class == 10 ~ 'other - fire',
                             class == 11 ~ 'hmature - fire'),
           year = '2020')
  
  
  ## put together and save
  out = bind_rows(f11_ai, f20_ai, o11_ai, o20_ai, mm11_ai, hm11_ai, rf20_ai, ro20_ai, rmm20_ai, rhm20_ai) %>%
    arrange(metric, desc(year), group)
  write_csv(out, 'results/agg_i.csv')
  
}