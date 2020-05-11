##  Example code using London and working with the routine
##  We had previously made function called inla_frontier which made the
##  binomial ina routine more usable
##  delete global environment and do a clean and rebuild for this package

startMyProject()
library(INLA) ## for some reason I still need to call this EVEN though we have @import INLA in the function call
library(socialFrontiers)

##  Load the data from the package namespace using data()
data(london)# the london data should be in the name space

##  Filter it to barnet -- london is too big
barnet <-
  london %>%
  filter(substr(LSOAname, 1, 6) %in% 'Barnet')

##  For binomial_inla we need to specify y (counts) and trials as well as contingency
##  matrix W

y <- 'nonUK' # 'nonUK' # Number of foreign
n.trials <- 'totalPop' #total population (per zone?)

barnet_sp <- barnet %>% as('Spatial')

##  Now we just run

model <-
  frontier_detect(data = barnet, y = y, n.trials = n.trials)


class(model) # correct

##  Based on my example code which used inla_frontier to preclean for binomial_localisedINLA
# source('Source/summary_frontier_model.R')


