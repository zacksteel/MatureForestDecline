## Purpose: Plot change in cover and percent of 2011
## Project: Mature Forest Decline
## Upstream: degrade.R
## Downstream:

plot_chg = function() 
{
  library(tidyverse)
  library(ggdist)
  library(colorspace)
  library(patchwork)
  library(cowplot)
  
  mature_ht_cut = 30
  
  ## Bring in canopy cover change data
  d = read_csv('results/ann_chg.csv') %>% 
  # d = read_csv('data/results/deg_ss/deg_ss.csv') %>% 
    dplyr::select(year, for_ha:hmature_ha)
  
  ## Get 2011 values
  orig = filter(d, year == 2011) %>% 
    dplyr::select(for_orig = for_ha, mat_orig = mature_ha, mm_orig = mmature_ha, hm_orig = hmature_ha)
  
  d2 = merge(d, orig) %>% 
    mutate(for_p = for_ha / for_orig * 100,
           m_p = mature_ha / mat_orig * 100,
           mm_p = mmature_ha / mm_orig * 100,
           hm_p = hmature_ha / hm_orig * 100) %>% 
    dplyr::select(year, for_p:hm_p) %>% 
    pivot_longer(cols = for_p:hm_p,
                 names_to = 'cover', values_to = 'Percent') %>% 
    mutate(year = as.integer(year),
           col = case_when(cover == 'for_p' ~ 'All Conifer Forests',
                           cover == 'm_p' ~ 'Mature Conifer Forests',
                           cover == 'mm_p' ~ 'Moderate Density Mature',
                           cover == 'hm_p' ~ 'High Density Mature'),
           col = fct_relevel(col, 'All Conifer Forests', 'Mature Conifer Forests', 'Moderate Density Mature'),
           class2 = case_when(cover %in% c('m_p', 'mm_p', 'hm_p') ~ "Mature Conifer Forests",
                              TRUE ~ "All Conifer Forests"),
           class3 = case_when(cover %in% c('for_p', 'm_p') ~ 'first',
                              TRUE ~ 'second'))
  
  
  p <- ggplot(d2[d2$class3 == "first",], 
              aes(x = year, y = Percent, color = class2, group = col)) +
    geom_hline(yintercept = 100, lty = 1, lwd = 1, color = "grey60") +
    geom_line(lwd = 2.5, alpha = .8) +
    geom_line(data = d2[d2$class3 == "second",], lwd = 1.5, alpha = .8, lty = 2) +
    scale_size_manual(values = c(2.5, 1.5), guide = NULL) +
    scale_color_manual(values = c("#B4DCAB", "#004616")) +
    scale_x_continuous(breaks = c(2012, 2014, 2016, 2018, 2020)) +
    coord_cartesian(xlim = c(2010.5, 2020.5)) +
    scale_y_continuous(breaks = c(125, 100, 75, 50, 25, 0),
                       labels = c(25, 0, -25, -50, -75, -100),
                       limits = c(0, 135)) +
    ylab('Percent area change') + xlab('Year') +
    annotate('text', x = c(2018.5, 2018.5), y = c(26, 113),
             label = c("High Density", #\n(> 60% initial cover)", 
                       'Moderate Density'), #\n(40-60% initial cover)'),
             angle = c(-5, -9), size = 3.5) +
    theme_bw() +
    theme(legend.position = c(0, 0),
          # legend.direction = "horizontal",
          legend.justification = c(0,0),
          legend.key.width = unit(1.5, 'cm'),
          legend.spacing.y = unit(-.5, 'cm'),
          panel.grid.minor = element_blank(),
          legend.background = element_blank(),
          text = element_text(size = 14)) +
    guides(colour = guide_legend(override.aes = list(alpha = 1)))

  # save_plot(filename = 'figures/parea_chg.png',
  #           p,
  #           base_height = 4, base_width = 6)
  
  ## Looking at it a different way with everything above 40 cc and tall trees = mature
  d3 = read_csv('results/samples.csv') 
  
  ## Try splitting
  p0.1 = ggplot(d3[d3$class == "conifer",], 
                aes(y = cc, x = year, fill = stat(y < 25))) +
    stat_halfeye(.width = .5, alpha = .8,
                 show_point = F, show_interval = F) +
    scale_fill_manual(values = c("#B4DCAB", "grey60"),
                      guide = "none") +
    geom_hline(yintercept= 25, lty = 2, lwd = 1, color = "#B4DCAB") +
    scale_y_continuous(breaks = c(0, 25, 40, 60, 80)) +
    scale_x_continuous(breaks = c(2012, 2014, 2016, 2018, 2020)) +
    coord_cartesian(xlim = c(2010.5, 2020.5)) +
    annotate('text', x = c(2010.5, 2010.6), y = c(30, 4),
             label = c("Forest", 
                       'Non-Forest'), size = 3.5) +
    theme_bw() +
    theme(panel.grid.minor = element_blank(),
          legend.background = element_blank(),
          text = element_text(size = 14)) +
    ylab("Canopy Cover") + xlab(NULL)
  
  p0.2 = ggplot(d3[d3$class == "mature",], 
                aes(y = cc, x = year, fill = stat(y < 40))) +
    stat_halfeye(.width = .5, alpha = .8, 
                 show_point = F, show_interval = F) +
    scale_fill_manual(values = c("#004616", "grey60"), 
                      guide = "none") +
    geom_hline(yintercept= 25, lty = 2, lwd = 1, color = "#B4DCAB") +
    geom_hline(yintercept = c(40, 60), lty = 2, lwd = 1, color = "#004616") +
    scale_y_continuous(breaks = c(0, 25, 40, 60, 80)) +
    scale_x_continuous(breaks = c(2012, 2014, 2016, 2018, 2020)) +
    coord_cartesian(xlim = c(2010.5, 2020.5)) +
    annotate('text', x = c(2010.5, 2010.5), y = c(50, 70),
             label = c("Moderate\nDensity", 
                       'High\nDensity'), 
             size = 3.5, lineheight = 0.6) +
    theme_bw() +
    theme(panel.grid.minor = element_blank(),
          legend.background = element_blank(),
          text = element_text(size = 14)) +
    ylab("Canopy Cover") + xlab(NULL)
  
  pp0 = p0.1 / p0.2 
  pp = pp0 / p +
    plot_layout(heights = c(1, 1, 2)) +
    plot_annotation(tag_levels = "a", tag_suffix = ")")
  
  save_plot("figures/chg_plot.png", pp,
            base_height = 8, base_width = 8)
  

}