#'  Common inspections of the object include the number of frontiers vs non-frontiers
#' this uses a class method for summary -- summary.<class> happens if we do 
#' summary(<this_class>)
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
