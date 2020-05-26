#'@title Convert frontier_model object into an sf object
#'
#'@description
#'Converts a frontier_model created by frontier_detect into an sf line object.
#'Default geometry are lines.
#'
#'@param frontier_model A object of calls frontier_model created by frontier_detect
#'@param convert2Line Boolean value indicating if we want to return borders
#'as line geometries. If FALSE then it will return all types of geometries created
#'by the st_intersects function used by the routine (see details). This includes points.
#'@param non_frontiers Boolean value indicating if non-frontier borders are also
#'returned. Default FALSE to speed up processing (see details).
#'@param silent Boolean. To print progress or not. Default FALSE
#'@param method character. Options for routines to extract borders. Note issues
#' with method = 'rbindlist'
#'
#'@details
#'This is strictly for 1) graphing purposes or 2) for spatial operations
#'Recommended to use edgelists for statistical analysis (wip)
#'
#'  Implementation basically requires us to:
#'  1) Make an edge list of all bordering polygons with an indicator frontier
#'  if they are a frontier (and include phi from the binomial inla)
#'  2) From that edge list use st_intersect to extract ALL borders
#'For step 2) we need a method to combine all the st_intersects from a list. By
#'default rbindlist from data.table is fastest. However the bounding box may
#'be incorrect. This is know to affect tmap's default map plotting behaviour but
#'not a substantial issue. forLoop calls do.call(rbind) which is far slower
#'
#'@import dplyr
#'@import sf
#'@importFrom data.table rbindlist

#'@export
frontier_as_sf <-
  function(frontier_model,
           convert2Line = T,
           non_frontiers = F,
           method = 'rbindlist', #method for controlling how we get the
           silent = F
           ) {


    ##  Check class
    data.class <- class(frontier_model)

    if (!('frontier_model' %in% data.class))
      stop ('Not a frontier_model object; please run frontier_detect()')

    ## Check method for extracting borders is valid
    validMethods <-
      c('forLoop', 'preAllocate', 'rbindlist')

    if(!(
      method %in% validMethods
    ))stop(
      paste('Not a valid method for extracting borders. Choose one of:',
            validMethods %>% paste(collapse = ', ')
    )
    )

    ##  An edge list of bordering polygons with an indicator frontier if that border
    ##  is a frontier or not
    egdelist_frontier <-
      which(frontier_model$W.frontiers == 0, arr.ind = T) %>% # finds non-NA values (which row and col) and arr.ind returns it as a matrix
      data.frame(frontier = T) # takes the table and turns it into data.frame and adds a row called frontier

    egdelist_nonfrontier <-
      which(frontier_model$W.frontiers == 1, arr.ind = T) %>%
      data.frame(frontier = F) # finds non-NA values (which row and col) and arr.ind returns it as a matrix

    if (non_frontiers == T) {
      edgelist_borders <-
        egdelist_frontier %>%
        rbind(egdelist_nonfrontier) # I want all the indicies for social frontiers and non-frontiers in
    } else{
      edgelist_borders <-
        egdelist_frontier
    }

    ##  Filter out everything from the data except id and phi -- to minimise memory
    data.for.borders <-
      frontier_model$data %>%
      mutate(phi = frontier_model$phi[['Median']]) %>%
      select(id, phi)


# Methods and timing
    x <- proc.time()

    borders_list <- list(NULL)


    for (i in 1:nrow(edgelist_borders)) {
      #i <- 1 # for testing
      zone1 <- edgelist_borders$col[i]
      zone2 <- edgelist_borders$row[i]

      borders_list[[i]] <-
        data.for.borders[zone1, ] %>%
        sf::st_intersection(data.for.borders[zone2, ]) %>%
        sf::st_cast('MULTILINESTRING')# now we are intersecting polys to get borders
      if (!silent & (i %% 10 == 0)) {
        print(i)
      }

    }

    print(proc.time() - x)


    y <- proc.time()

    print(
      method %>% paste('Using', . , collapse = ' ')
    )


    ##  method = forLoop
    if(method == 'forLoop'){
      borders_sf <-
        do.call(rbind, borders_list)
    }

    ## method = preAllocate
    if(method == 'preAllocate'){
      n <- length(borders_list)

      ## Create the data using the first and last item of the borders list
      borders_sf <-
        borders_list[[1]]
      borders_sf[n, ] <-
        borders_list[[n]]

      for (j in 1:n){
        borders_sf[j, ] <-
          borders_list[[j]]
      }

    }

    ## method = rbindlist from data.table
    if(method == 'rbindlist'){
      borders_sf <-
        data.table::rbindlist(borders_list)

      ##  we need to coerce back to sf
      borders_sf <-
        borders_sf %>%
        st_as_sf()
    }

    print(proc.time() - y)


    ##  Add the frontier label
    borders_sf$frontier <-
      edgelist_borders$frontier



    ##  Change to linefile if convert2Line is true
    if(convert2Line){
      borders_sf <-
        st_collection_extract(borders_sf, type = 'LINE') # lots more objects now but still has the frontier feature in the right place
    }


    ##  add a class frontier_sf to the data
    class(borders_sf) <- c('frontier_sf', class(borders_sf))

    return(borders_sf)
  }
