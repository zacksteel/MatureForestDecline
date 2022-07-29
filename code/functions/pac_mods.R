## Purpose: Model differences between pac and non-pac areas
## Project: Mature Forest Decline
## Upstream: pac_desc.R
## Downstream:

pac_mods = function(path)
{
  library(tidyverse)
  library(brms)
  library(lme4)
  library(marginaleffects)
  library(modelsummary)
  library(ggdist)
  library(tidybayes)
  
  ## read in data
  d = read_csv('results/pac_sample_data.csv') %>% 
    mutate(rel_deg = (cc_deg) / cc11,
           ## a few values > 1 (1%) likely do to raster malalignment
           rel_deg = ifelse(rel_deg > 0.98, 0.98, rel_deg),
           ## Beta distribution requires response greater than 0
           cc20 = cc20 + 0.01,
           cc_deg = cc_deg + 0.01,
           cc_deg = cc_deg / 100,
           rel_deg = rel_deg + 0.01,
           ## standardize continuous predictors
           cc11_s = (cc11 - mean(cc11)) / sd(cc11),
           ht11_s = (ht11 - mean(ht11)) / sd(ht11))
  
  ## decline relative to what we started with
  m_rd = brm(rel_deg ~ class, data = d, family = 'Beta')
  m_rd2 = brm(rel_deg ~ cc11_s + ht11_s, data = d, family = 'Beta')
  
  save(m_rd, m_rd2, d, file = 'models/mods.RData')
  
  ## meter values for ht figure; back transform standardization
  ht_ft = c(10, 20, 30) * 3.28
  ht_breaks = ht_ft %>% 
    sapply(function(x) (x - mean(d$ht11)) / sd(d$ht11))
  
  ## plot marginal effects
  plot_cap(m_rd2, condition = 'ht11_s', draw = F) %>% 
    ggplot(aes(x = condition1, y = predicted, ymin = conf.low, ymax = conf.high)) +
    geom_ribbon(alpha = .5, color = NA) +
    geom_line(size = 1.5) +
    theme_bw() +
    scale_y_continuous(limits = c(.15, .7), breaks = seq(0, .7, by = .1), labels = seq(0, 70, by = 10)) +
    scale_x_continuous(limits = c(-3.5, 2.5), breaks = ht_breaks, labels = c(10, 20, 30)) +
    ylab('Predicted Relative Decline (%)') + xlab('2011 Mean Large Tree Height (m)') -> p0
  
  cc_breaks = seq(0, 60, by = 20) %>% 
    sapply(function(x) (x - mean(d$cc11)) / sd(d$cc11))
    
  plot_cap(m_rd2, condition = 'cc11_s', draw = F) %>% 
    ggplot(aes(x = condition1, y = predicted, ymin = conf.low, ymax = conf.high)) +
    geom_ribbon(alpha = .5, color = NA) +
    geom_line(size = 1.5) +
    theme_bw() +
    scale_y_continuous(limits = c(.15, .7), breaks = seq(0, .7, by = .1), labels = seq(0, 70, by = 10)) +
    scale_x_continuous(breaks = cc_breaks, labels = seq(0, 60, by = 20)) +
    ylab('Predicted Relative Decline (%)') + xlab('2011 Canopy Cover (%)') -> p
  
  preds2 = predictions(m_rd2,
                       newdata = datagrid(cc11_s = c(0, max(d$cc11_s))))
  
  d %>%
    modelr::data_grid(class) %>%
    add_epred_draws(m_rd) %>%
    mutate(lab = ifelse(class == 'nonpac', 'Non-PAC', 'PAC')) %>%
    ggplot(aes(x = lab, y = .epred)) +
    stat_eye(point_size = 1.5) +
    theme_bw() +
    scale_y_continuous(limits = c(.15, .7), breaks = seq(0, .7, by = .1), labels = seq(0, 70, by = 10)) +
    ylab('Predicted Relative Decline (%)') + xlab(NULL) -> p1
  
  
  library(patchwork)
  library(cowplot)
  p1 + (p + ylab(NULL)) + (p0 + ylab(NULL)) + 
    plot_layout(widths = c(.3, .6, .6), nrow = 1) +
    plot_annotation(tag_levels = 'a', tag_suffix = ')') -> pp
  
  save_plot(filename = 'figures/pac_me.png', plot = pp, base_height = 4, base_width = 9)
  
  ## summarise model coeficients
  tidybayes::summarise_draws(m_rd2)
  brms::posterior_summary(m_rd2) %>% 
    as.data.frame() %>% 
    mutate_all(round,4) %>% 
    rownames_to_column(var = 'Variable') %>% 
    mutate(model = 'Structure',
           Variable = case_when(Variable != 'phi' ~ substring(Variable, 3))) %>% 
    filter(Variable %in% c('Intercept', 'cc11_s', 'ht11_s', 'phi')) -> ct
  
  brms::posterior_summary(m_rd) %>% 
    as.data.frame() %>% 
    mutate_all(round,4) %>% 
    rownames_to_column(var = 'Variable') %>% 
    mutate(model = 'Class',
           Variable = case_when(Variable != 'phi' ~ substring(Variable, 3))) %>% 
    filter(Variable %in% c('Intercept', 'classpac', 'phi')) -> ct1
  
  bind_rows(ct,ct1) %>% 
    dplyr::select(Model = model, everything(.)) %>% 
    write_csv("results/mod_coefs.csv")
    
}
