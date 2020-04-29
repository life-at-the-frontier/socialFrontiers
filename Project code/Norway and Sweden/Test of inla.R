##  Example code using London and working with the routine
##  We had previously made function called inla_frontier which made the
##  binomial ina routine more usable

library(INLA) ## for some reason I still need to call this EVEN though we have @import INLA in the function call

##  Load the data from the package namespace using data()
data(london)# the london data should be in the name space

##  Filter it to barnet -- london is too big
barnet <-
  london %>%
  filter(substr(LSOAname, 1, 6) %in% 'Barnet')

##  For binomial_inla we need to specify y (counts) and trials as well as contingency
##  matrix W

y <- barnet$nonUK # 'nonUK' # Number of foreign
n.trials <- barnet$totalPop #total population (per zone?)

barnet_sp <- barnet %>% as('Spatial')

W.nb <- spdep::poly2nb(barnet_sp,
                  queen = F, #more than just a single point touching to be neighbours
                  row.names = 1:nrow(barnet_sp)) #Sadly we have to convert the sf object to spatial dataframe using as() here

W <- spdep::nb2mat(W.nb, style = "B") # B = binary

##  Now we just run
model <-
  binomial_localisedINLA(y ~ 1, W = W, Ntrials = n.trials)


##  Based on my example code which used inla_frontier to preclean for binomial_localisedINLA
# source('Source/inla_frontier.R')
# source('Source/summary_frontier_model.R')


##  My old code function
##  Output:
##  list object of class: frontier_model
##

# inla_frontier <- function(y, data, n.trials, W.nb = NULL, ...){
#   ## Check format
#   data.class <- class(data)
#   if(!('SpatialPolygonsDataFrame' %in% data.class) & !('sf' %in% data.class)) stop ('Data not sf or SpatialPolygons')
#
#   if('sf' %in% class(data)){data2 <- data %>% as('Spatial')} else {data2 <- data}
#
#   ##  We do not touch data
#   ##  We work with data2 and other versions copied (for just keep track of things)
#
#   ##  Creating the contiguity matrix: if no W.nb is supplied make one
#   if(is.null(W.nb)){
#     W.nb <- poly2nb(data2,
#                     queen = F,#more than just a single point touching to be neighbours
#                     row.names = 1:nrow(data2)) #Sadly we have to convert the sf object to spatial dataframe using as() here
#   }
#
#   count.no.neighours <- sum(unlist(W.nb) == 0)
#   if(count.no.neighours > 0){warning( count.no.neighours %>% paste('zone(s) have no neighbours!'))}
#
#   W <- nb2mat(W.nb, style = "B") # B = binary
#
#
#   ##  INLA routine
#   source("Source/binomial.localisedINLA.R") ## This needs to be loaded
#
#   x <- proc.time()
#
#   mod.inla <- binomial.localisedINLA(
#     formula = data2[[y]] ~ 1,# y is variable name for number of foreigners
#     W = W,
#     Ntrials = data2[[n.trials]],# how many lived in zone
#     fix.rho = TRUE,
#     rho = 0.99
#   )
#
#   print(proc.time() - x)
#
#
#
#   ##  So the inla output is just a list not a proper class object..
#   mod.inla$W.frontiers <- mod.inla$W.estimated #extract estimated matrix
#   # So basically we are putting NAs where there was no neighbour in the original
#   # W matrix. Remainder are geographical neighbours with 0s and 1s denoting
#   # sig correlation or not
#   mod.inla$W.frontiers[W == 0] <- NA ## Put NAs where there was orginally not a border
#   mod.inla$W.frontiers[lower.tri(mod.inla$W.frontiers, diag = T)] <- NA ## gets rid of the upper part of the sym. matrix
#
#   class(mod.inla) <- 'frontier_model' #changes it's class allowing for custom routines
#
#   return(mod.inla)
# }
#



##  frontier_model <-
##  inla_frontier(data = tower.sf, y = 'nonUK', n.trials = 'totalPop')


