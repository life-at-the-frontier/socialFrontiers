#'@title frontier_as_sf
#' This is the function that converts a frontier_model object and extracts the
#' borders as an sf object
#'  Routine basically requires us to:
#'  1) Make an edge list of all bordering polygons with an indicator frontier
#'  if they are a frontier (and include phi from the binomial inla)
#'  2) From that edge list use st_intersect to extract ALL borders
#'  Issue current method uses do.call(rbind) which is inefficient as number of
#'  list items grows
#' Original code: frontier creation source line 80+ onwards
#'@import tidyverse
#'@import sf


frontier_as_sf <-
  function(frontier_model){

    ##  Check class
    data.class <- class(frontier_model)

    if (!('frontier_model' %in% data.class))
      stop ('Not a frontier_model object; please run frontier_detect()')


    ##  An edge list of bordering polygons with an indicator frontier if that border
    ##  is a frontier or not
    w.index0 <-
      which(frontier_model$W.frontiers == 0, arr.ind = T) %>% # finds non-NA values (which row and col) and arr.ind returns it as a matrix
      data.frame(frontier = T) # takes the table and turns it into data.frame and adds a row called frontier

    w.index1 <-
      which(frontier_model$W.frontiers == 1, arr.ind = T) %>%
      data.frame(frontier = F) # finds non-NA values (which row and col) and arr.ind returns it as a matrix

    w.index <- w.index0 %>% rbind(w.index1) # I want all the indicies for social frontiers and non-frontiers in


    ##  Filter out everything from the data except id and phi -- to minimise memory
    data.for.borders <-
      frontier_model$data %>%
      mutate(phi = frontier_model$phi[['Median']]) %>%
      select(id, phi)

    ##  Now to run the st_intersection in a forloop
    borders.sf <- list(NULL)

    x <- proc.time()

    for (i in 1:nrow(w.index)) {
      #i <- 1 # for testing
      zone1 <- w.index$col[i]
      zone2 <- w.index$row[i]

      borders.sf[[i]] <- data.for.borders[zone1,] %>% st_intersection(data.for.borders[zone2,]) # now we are intersecting polys to get borders
      #borders.sf$frontier[i] <- w.index$frontier[i]

      if(i %% 10 == 0){
        print(i)
      }

    }

    borders.sf <-
      do.call(rbind, borders.sf)
    print(proc.time() - x)

    ##  Add the frontier label
    borders.sf$frontier <-
      w.index$frontier

    ##  add a class frontier_sf to the data
    class(borders.sf) <- c(class(borders.sf), 'frontier_sf')

    return(borders.sf)
  }
