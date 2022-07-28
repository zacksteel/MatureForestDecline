## Purpose: Plot change in forest within 2 mean fire return intervals
## Project: Mature Forest Decline
## Upstream: degrade.R
## Downstream: 

plot_wifri = function()
{
  library(tidyverse)
  library(cowplot)
  library(patchwork)
  library(colorspace)
  library(ggridges)
  
  ## Bring in canopy cover change data
  # d = read_csv('data/results/deg_ss/deg_ss.csv') 
  d = read_csv('results/ann_chg.csv') %>% 
    full_join(read_csv('results/ann_chg_man.csv'))
  
  d = mutate(d,
             for_p = for_ha / as.numeric(d[d$year == 2011, "for_ha"]) * 100,
             mat_p = mature_ha / as.numeric(d[d$year == 2011, "mature_ha"]) * 100,
             mm_p = mmature_ha / as.numeric(d[d$year == 2011, "mmature_ha"]) * 100,
             hm_p = hmature_ha / as.numeric(d[d$year == 2011, "hmature_ha"]) * 100,
             rfor_p = for_rfri / for_ha * 100,
             rmat_p = mature_rfri / mature_ha * 100,
             rmm_p = mmature_rfri / mmature_ha * 100,
             rhm_p = hmature_rfri / hmature_ha * 100,
             for_drloss = ifelse(is.na(for_mloss), for_loss - for_bloss,
                             for_loss - for_bloss - for_mloss),
             mat_drloss = ifelse(is.na(mature_mloss), mature_loss - mature_bloss,
                             mature_loss - mature_bloss - mature_mloss),
             mm_drloss = ifelse(is.na(mmature_mloss), mmature_loss - mmature_bloss,
                            mmature_loss - mmature_bloss - mmature_mloss),
             hm_drloss = ifelse(is.na(hmature_mloss), hmature_loss - hmature_bloss,
                            hmature_loss - hmature_bloss - hmature_mloss),
             ## make values relative to 2011
             for_bloss_p = for_bloss / as.numeric(d[d$year == 2011, 'for_ha']) * 100,
             mat_bloss_p = mature_bloss / as.numeric(d[d$year == 2011, 'mature_ha']) * 100,
             mm_bloss_p = mmature_bloss / as.numeric(d[d$year == 2011, 'mmature_ha']) * 100,
             hm_bloss_p = hmature_bloss / as.numeric(d[d$year == 2011, 'hmature_ha']) * 100,
             for_mloss_p = for_mloss / as.numeric(d[d$year == 2011, 'for_ha']) * 100,
             mat_mloss_p = mature_mloss / as.numeric(d[d$year == 2011, 'mature_ha']) * 100,
             mm_mloss_p = mmature_mloss / as.numeric(d[d$year == 2011, 'mmature_ha']) * 100,
             hm_mloss_p = hmature_mloss / as.numeric(d[d$year == 2011, 'hmature_ha']) * 100,
             for_drloss_p = for_drloss / as.numeric(d[d$year == 2011, 'for_ha']) * 100,
             mat_drloss_p = mat_drloss / as.numeric(d[d$year == 2011, 'mature_ha']) * 100,
             mm_drloss_p = mm_drloss / as.numeric(d[d$year == 2011, 'mmature_ha']) * 100,
             hm_drloss_p = hm_drloss / as.numeric(d[d$year == 2011, 'hmature_ha']) * 100)
  
  
  
  
  ## attributing loss
  d2 = dplyr::select(d, year, for_bloss_p, mat_bloss_p, 
                     mm_bloss_p, hm_bloss_p,  
                     for_mloss_p, mat_mloss_p, 
                     mm_mloss_p, hm_mloss_p, 
                     for_drloss_p, mat_drloss_p, mm_drloss_p, hm_drloss_p) %>% 
    pivot_longer(cols = for_bloss_p:hm_drloss_p) %>% 
    mutate(cover = str_split(name, pattern = "_", simplify = T)[,1],
           disturb = str_split(name, pattern = "_", simplify = T)[,2],
           label = case_when(cover == "for" ~ "All Conifer Forests",
                             cover == 'mat' ~ 'Mature Conifer Forests',
                             cover == "mm" ~ "Moderate Density Mature",
                             cover == 'hm' ~ 'High Density Mature'),
           label = fct_relevel(label, 'All Conifer Forests', 'Mature Conifer Forests', 'Moderate Density Mature'),
           disturb = fct_relevel(disturb, 'mloss', 'bloss', 'drloss'),
           chg = value * -1)
  
  
  ann_text1 <- data.frame(year = c(2013.25, 2016.5, 2020.25),
                          value = c(-3, -3, -3),
                          label = "All Conifer Forests",
                          disturb = NA,
                          lab = c('Rim Fire', 'Final Drought Year', 'Creek & Castle Fires')) 
  ann_text1$label = factor(ann_text1$label, levels = levels(d2$label))
  
  ## plot for major classes and a second for subclasses of mature
  p = ggplot(filter(d2, cover %in% c('for', 'mat')), 
             aes(x = year, y = chg, fill = disturb)) +
    facet_wrap(~ label, ncol = 1, scales = 'free_y') +
    geom_col(position = 'dodge', width = .75) +
    scale_fill_manual(breaks = c('mloss', 'bloss', 'drloss'),
                      values = c('grey10', "#660000", "#CC9966"),
                      name = NULL, #"Disturbance",
                      labels = c("Mech + Drought", "Fire + Drought", "Drought")) +
    scale_x_continuous(breaks = c(2012, 2014, 2016, 2018, 2020),
                       limits = c(2011, 2020.4)) +
    geom_vline(xintercept = c(2013, 2016.25, 2020), lty = 2.5) +
    theme_bw() +
    theme(legend.position = c(0.875, .25),
          # legend.direction = "horizontal",
          legend.justification = c(1,1),
          legend.background = element_rect(fill='transparent'),
          panel.grid.minor = element_blank(),
          text = element_text(size = 14)) +
    geom_text(data = ann_text1, aes(y = value, label = lab), angle = 90, size = 4) +
    ylab("Percent Area Changed") + xlab('Year')
  
  p.1 = ggplot(filter(d2, cover %in% c('mm', 'hm')), 
             aes(x = year, y = chg, fill = disturb)) +
    facet_wrap(~ label, ncol = 1, scales = 'free_y') +
    geom_col(position = 'dodge', width = .75) +
    scale_fill_manual(values = c('grey10', "#660000", "#CC9966"),
                      name = NULL, #"Disturbance",
                      labels = c("Mech + Drought", "Fire + Drought", "Drought")) +
    scale_x_continuous(breaks = c(2012, 2014, 2016, 2018, 2020),
                       limits = c(2011, 2020.4)) +
    theme_bw() +
    theme(legend.position = c(0.875, .25),
          legend.justification = c(1,1),
          legend.background = element_rect(fill='transparent'),
          panel.grid.minor = element_blank(),
          text = element_text(size = 14)) +
    ylab("Percent Area Changed") + xlab('Year')
  
  d3 = read_csv('results/ann_chg.csv') %>% 
    dplyr::select(year:hmature_rfri) %>% 
    pivot_longer(cols = for_ha:hmature_rfri) %>% 
    mutate(label = case_when(name %in% c("for_ha", "for_rfri") ~ "All Conifer Forests",
                             name %in% c('mature_ha', 'mature_rfri') ~ 'Mature Conifer Forests',
                             name %in% c("mmature_ha", 'mmature_rfri') ~ "Moderate Density Mature",
                             name %in% c('hmature_ha', 'hmature_rfri') ~ 'High Density Mature'),
                      label = fct_relevel(label, 'All Conifer Forests', 'Mature Conifer Forests', 'Moderate Density Mature'),
           condition = ifelse(name %in% c('for_ha', 'mature_ha', 'mmature_ha', 'hmature_ha'), 'total', 'within'))
  
  ann_text2 <- data.frame(year = c(2013.25, 2016.5, 2020.25),
                         value = 750,
                         label = "All Conifer Forests",
                         condition = NA,
                         lab = c('Rim Fire', 'Final Drought Year', 'Creek & Castle Fires'))
  ann_text2$label = factor(ann_text2$label, levels = levels(d3$label))
  
  p2 = ggplot(filter(d3, label %in% c('All Conifer Forests', 'Mature Conifer Forests')), 
              aes(x = year, y = value/1000, fill = condition)) +
    facet_wrap(~ label, ncol = 1, scales = 'free_y') +
    geom_density_line(stat = 'identity', lwd = 2, color = "transparent") +
    scale_fill_manual(name = NULL,
                      values = c("grey70", "grey30"),
                      labels = c("Total", "Recently Burned")) +
    scale_x_continuous(breaks = c(2012, 2014, 2016, 2018, 2020),
                       limits = c(2011, 2020.4)) +
    geom_vline(xintercept = c(2013, 2016.25, 2020), lty = 2.5) +
    theme_bw() +
    theme(legend.position = c(0.9, .45),
          legend.justification = c(1,1),
          legend.background = element_rect(fill='transparent'),
          panel.grid.minor = element_blank(),
          text = element_text(size = 14)) +
    geom_text(data = ann_text2, aes(y = value, label = lab), angle = 90, size = 4) +
    ylab("Area (1k Ha)") + xlab("Year")
  
  p2.1 = ggplot(filter(d3, label %in% c('Moderate Density Mature', 'High Density Mature')), 
              aes(x = year, y = value/1000, fill = condition)) +
    facet_wrap(~ label, ncol = 1, scales = 'free_y') +
    geom_density_line(stat = 'identity', lwd = 2, color = "transparent") +
    scale_fill_manual(name = NULL,
                      values = c("grey70", "grey30"),
                      labels = c("Total", "Recently Burned")) +
    scale_x_continuous(breaks = c(2012, 2014, 2016, 2018, 2020),
                       limits = c(2011, 2020.4)) +
    theme_bw() +
    theme(legend.position = c(0.9, .45),
          # legend.direction = "horizontal",
          legend.justification = c(1,1),
          legend.background = element_rect(fill='transparent'),
          panel.grid.minor = element_blank(),
          text = element_text(size = 14)) +
    ylab("Area (1k Ha)") + xlab("Year")

  
  pp = p / p2 +
    plot_annotation(tag_levels = 'a', tag_suffix = ')')
  
  save_plot(filename = "figures/area_fri.png",
            pp,
            base_height = 10, base_width = 8)
  
  pp.1 = p.1 / p2.1 +
    plot_annotation(tag_levels = 'a', tag_suffix = ')')
  
  save_plot(filename = "figures/area_fri_sup.png",
            pp.1,
            base_height = 10, base_width = 8)
  
}
