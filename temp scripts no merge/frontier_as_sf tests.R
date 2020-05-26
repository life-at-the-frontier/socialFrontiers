##  Testing script for checking run times of the frontier_as_sf
##  Based on the barnet vignette

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
  frontier_as_sf(frontier_model, silent = T, method = 'forLoop') # 1.97 / 0.9 s
#  frontier_as_sf(frontier_model, silent = T, method = 'wrongOption') # Checking error messgae
#  frontier_as_sf(frontier_model, silent = T, method = 'preAllocate') # 1.99 / 1.26 seconds
#  frontier_as_sf(frontier_model, silent = T, method = 'rbindlist') # 2 / 0.02 seconds

## Test of equivalence
borders_sf_forLoop <-
  frontier_as_sf(frontier_model, silent = T, method = 'forLoop')

borders_sf_rbindlist <-
  frontier_as_sf(frontier_model, silent = T, method = 'rbindlist')

## Check data : same data rows and same class
borders_sf_forLoop
borders_sf_rbindlist

borders_sf_forLoop %>% st_crs()
borders_sf_rbindlist %>% st_crs()

##  Different bounding box thoug
borders_sf_forLoop %>% st_bbox()
borders_sf_rbindlist %>% st_bbox()

st_bbox(borders_sf_rbindlist) <- borders_sf_forLoop %>% st_bbox()

class(borders_sf_forLoop)
class(borders_sf_rbindlist) #this has data.table in it

##  Example plot from vignette
barnet <-
  barnet %>%
  mutate(propNonUK = nonUK/totalPop)

tm_shape(barnet) +
  tm_fill(col= 'propNonUK') +
  qtm(borders_sf_rbindlist) # works
###

##  Known issue : tmap plot doesn't plot correctly -- seems to be a bounding box issue
##  only present in tmap plot mode u
library(tmap)
tmap_mode('view')

tm_shape(borders_sf_forLoop) +
  tm_lines()

tm_shape(borders_sf_rbindlist) +
  tm_lines()
##  Visuall check the same

##  tmap plot mode
tmap_mode('plot')

tm_shape(borders_sf_forLoop) +
  tm_lines()

##  Here's the issue
tm_shape(borders_sf_rbindlist) +
  tm_lines(col = 'red')





tm_shape(borders_sf_rbindlist) +
  tm_lines()

##  Solution:
str(borders_sf_forLoop)
str(borders_sf_rbindlist)


borders_sf_rbindlist$geometry <- sf::st_sfc( sapply( borders_sf_rbindlist$geometry, `[`) )
