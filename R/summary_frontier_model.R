#' @title Summary method for frontier_model objects
#'
#' @description
#' Common inspections of the object the number of frontiers vs non-frontiers
#' this uses a class method for summary.
#' Prints total number of borders and the number of frontiers vs non-frontiers
#'
#' @param object A frontier_model object created by frontier_detect()
#'

summary.frontier_model <- function(object, ...){


  ## Display the numbers of frontiers
  borders_tab <- tabulate(object$W.frontiers + 1) # so table will convert integer to
  #factor first so the quicker way is to use tabulate which will only display
  ##  counts of positive intergers

  n_borders <-
    sum(borders_tab)

  out <-
    list(
      n_borders = n_borders,
      borders_tab = borders_tab
    )

  class(out) <- c("summary.frontier_model", out)

  return(out)

}


