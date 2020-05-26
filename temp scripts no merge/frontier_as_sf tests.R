##  Testing script for checking run times of the frontier_as_sf

library(socialFrontiers)
library(tidyverse)

##  Setup data as vignette
data(london)

##  Filter to the borough of Barnet
barnet <-
  london %>%
  filter(substr(LSOAname, 1, 6) %in% 'Barnet')

y <- 'nonUK' # 'nonUK' # Number of foreign
n.trials <- 'totalPop' #total population (per zone?)

frontier_model <-
  frontier_detect(
    data = barnet,
    y = y, n.trials = n.trials)
###
##  Test of function speed -----

borders_sf <-
#  frontier_as_sf(frontier_model, silent = T) # 1.97 / 0.9 s
#  frontier_as_sf(frontier_model, silent = T, method = 'wrongOption') # Checking error messgae
#  frontier_as_sf(frontier_model, silent = T, method = 'preAllocate') # 1.99 / 1.26 seconds
  frontier_as_sf(frontier_model, silent = T, method = 'rbindlist') # 2 / 0.02 seconds



#tmap::qtm(barnet) +
  tmap::qtm(borders_sf, col = 'red')

