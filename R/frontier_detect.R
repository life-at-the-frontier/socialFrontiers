#' This is a wrapper for our frontier detection functions
#' goal: standardise input and outputs regardless of technique?
#' TODO: Check out how packages like matchit and synthpop deal with calling 
#' methods from other packages
#'  Note: orginally called inla_frontier in the project
#'  Returns list object of class: frontier_model
##
##' @import INLA


frontier_detect <-
  function(y, data, n.trials, W.nb = NULL, ...) {
    
    ## Initial check of format; data needs to be spatialpolygon or sf
    data.class <- class(data)
    
    if (!('SpatialPolygonsDataFrame' %in% data.class) &
        !('sf' %in% data.class))
      stop ('Data class not sf or SpatialPolygons')
    
    if ('sf' %in% class(data)) {
      data2 <- data %>% as('Spatial')
    } else {
      data2 <- data
    }
    
    ##  We do not touch data
    ##  We work with data2 and other versions copied (for just keep track of things)
    
    ##  Creating the contiguity matrix: if no W.nb is supplied make one
    if (is.null(W.nb)) {
      W.nb <- poly2nb(data2,
                      queen = F,
                      #more than just a single point touching to be neighbours
                      row.names = 1:nrow(data2)) #Sadly we have to convert the sf object to spatial dataframe using as() here
    }
    
    ## data check
    count.no.neighours <- sum(unlist(W.nb) == 0)
    if (count.no.neighours > 0) {
      warning(count.no.neighours %>% paste('zone(s) have no neighbours!'))
    }
    
    W <- nb2mat(W.nb, style = "B") # B = binary
    
    
    ##  INLA routine
#    source("Source/binomial.localisedINLA.R") ## This needs to be loaded
    
    x <- proc.time()
    
    mod.inla <- binomial_localisedINLA(
      formula = data2[[y]] ~ 1, ## y is variable name
      W = W,
      Ntrials = data2[[n.trials]], ##n.trials is the trial variable
      fix.rho = TRUE,
      rho = 0.99
    )
    
    print(proc.time() - x)
    
    
    
    ##  So the inla output is just a list not a proper class object..
    mod.inla$W.frontiers <-
      mod.inla$W.estimated #extract estimated matrix
    # So basically we are putting NAs where there was no neighbour in the original
    # W matrix. Remainder are geographical neighbours with 0s and 1s denoting
    # sig correlation or not
    mod.inla$W.frontiers[W == 0] <-
      NA ## Put NAs where there was orginally not a border
    mod.inla$W.frontiers[lower.tri(mod.inla$W.frontiers, diag = T)] <-
      NA ## gets rid of the upper part of the sym. matrix
    
    class(mod.inla) <-
      'frontier_model' #changes it's class allowing for custom routines
    
    return(mod.inla)
  }
