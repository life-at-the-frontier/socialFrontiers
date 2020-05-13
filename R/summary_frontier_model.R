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
  borders.tab <- tabulate(object$W.frontiers + 1) # so table will convert integer to
  #factor first so the quicker way is to use tabulate which will only display
  ##  coutners of positive intergers

  print('Total N. of borders')
  print(sum(borders.tab))
  print('N. of borders: frontier vs non-frontier')
  print(borders.tab)


}
