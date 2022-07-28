#### Code dashboard for Steel et al. In Review  ####
#### Author - Zack Steel                        ####

## Set path to spatial files. I'm using a directory untracked by git called local.
path <- "local"

## Calculates change in canopy cover over time (also parses by burned and not)
source('code/functions/degrade.R')
degrade(path)

## Change within managed areas
source('code/functions/man_degrade.R')
man_degrade(path)

## Some additional spatial analysis (i.e., patch size & aggregation index calculations) and tabular outputs
source('code/functions/spat_anal.R')
spat_anal(path)

## Additional descriptive stats for PAC & non-PAC areas not calculated as part of spat_anal.R
source('code/functions/pac_desc.R')
pac_desc(path)

## Run pac and structure models and plot results
source('code/functions/pac_mods.R')
pac_mods()

## Plot change in canopy cover by class
source('code/functions/plot_chg.R')
plot_chg()

## Plot changes attributable to different disturbances & plot change in recent fire are
source('code/functions/plot_wifri.R')
plot_wifri()


