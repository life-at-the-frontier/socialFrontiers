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
#'@param edgelistOnly Boolean value indicating where or not to extract edgelist only
#'without sf geometry. Default FALSE to return sf geometry.
#'@param silent Boolean. To print progress or not. Default FALSE
#'
#'@details
#'This is strictly for 1) graphing purposes or 2) for spatial operations
#'Recommended to use edgelists for statistical analysis (wip)
#'
#'  Implementation basically requires us to:
#'  1) Make an edge list of all bordering polygons with an indicator frontier
#'  if they are a frontier (and include phi from the binomial inla)
#'  2) From that edge list use st_intersect to extract ALL borders
#'  Issue current method uses do.call(rbind) which is inefficient as number of
#'  list items grows
#' Original code: frontier creation source line 80+ onwards
#'
#'@import dplyr
#'@import sf
#'@import purrr

#'@export
frontier_as_sf <-
  function(frontier_model,
           convert2Line = T,
           non_frontiers = F,
           edgelistOnly = F,
           silent = F
           ) {
    ##  Check class
    data.class <- class(frontier_model)

    if (!('frontier_model' %in% data.class))
      stop ('Not a frontier_model object; please run frontier_detect()')


    ##  An edge list of bordering polygons with an indicator frontier if that border
    ##  is a frontier or not
    edgelist_frontier <-
      which(frontier_model$W.frontiers == 0, arr.ind = T) %>% # finds non-NA values (which row and col) and arr.ind returns it as a matrix
      data.frame(frontier = T) # takes the table and turns it into data.frame and adds a row called frontier

    edgelist_nonfrontier <-
      which(frontier_model$W.frontiers == 1, arr.ind = T) %>%
      data.frame(frontier = F) # finds non-NA values (which row and col) and arr.ind returns it as a matrix

    if (non_frontiers == T) {
      edgelist_borders <-
        edgelist_frontier %>%
        rbind(edgelist_nonfrontier) # I want all the indicies for social frontiers and non-frontiers in
    } else{
      edgelist_borders <-
        edgelist_frontier
    }

    ##  Filter out everything from the data except id and phi -- to minimise memory
    data.for.borders <-
      frontier_model$data %>%
      mutate(phi = frontier_model$phi[['Median']]) %>%
      select(id, phi)

    edgelist <-
      data.frame(
        id = data.for.borders$id[edgelist_borders$col],
        id.1 = data.for.borders$id[edgelist_borders$row],
        phi = data.for.borders$phi[edgelist_borders$col],
        phi.1 = data.for.borders$phi[edgelist_borders$row]
      )




    ## Edgelist only option to save time
    if (edgelistOnly == T) {
      return(edgelist)
    }



    # hotfix: try to use map2 in purrr to vectorise the forloop ---------------
    x <- proc.time()


    ## Get geometry of whole vector
    zones1 <- data.for.borders[edgelist_borders$col,] %>% st_geometry()
    zones2 <- data.for.borders[edgelist_borders$row,] %>% st_geometry()

    # use map2 to vectorise the st_intersection
    borders_geomlist <- purrr::map2(zones1, zones2, st_intersection)

    ## turn list into sfc
    border_sfc <- st_sfc(borders_geomlist)

    ##  add geom + crs to edlist
    st_geometry(edgelist) <- border_sfc
    st_crs(edgelist) <- st_crs(data.for.borders)

    ## rename
    borders.sf <- edgelist
    ## end time
    print(proc.time() - x)


    # end hotfix --------------------------------------------------------------


    ##  Now to run the st_intersection in a forloop
    # borders.sf <- list(NULL)
    #
    # x <- proc.time()
    #
    # for (i in 1:nrow(edgelist_borders)) {
    #   #i <- 1 # for testing
    #   zone1 <- edgelist_borders$col[i]
    #   zone2 <- edgelist_borders$row[i]
    #
    #   borders.sf[[i]] <-
    #     data.for.borders[zone1, ] %>% st_intersection(data.for.borders[zone2, ]) # now we are intersecting polys to get borders
    #   #borders.sf$frontier[i] <- edgelist_borders$frontier[i]
    #
    #   if (!silent & (i %% 10 == 0)) {
    #     print(i)
    #   }
    #
    # }
    #
    #
    # borders.sf <-
    #   do.call(rbind, borders.sf)
    # print(proc.time() - x)

    ##  Add the frontier label
    borders.sf$frontier <-
      edgelist_borders$frontier




    ##  Change to linefile if convert2Line is true
    if(convert2Line){
      borders.sf <-
        st_collection_extract(borders.sf, type = 'LINE')

      ## Fix multiple entries caused by geom_collections -- eg. multiple intersections
      borders.sf <-
        borders.sf %>%
        group_by(id, id.1, phi, phi.1) %>%
        summarise(
          hotfix = T
        ) %>%
        ungroup %>%
        select(-hotfix)
    }


    ##  add a class frontier_sf to the data
    class(borders.sf) <- c('frontier_sf', class(borders.sf))

    return(borders.sf)
  }
