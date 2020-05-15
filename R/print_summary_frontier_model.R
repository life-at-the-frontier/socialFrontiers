#' Print method for summary.frontier_model
#'
#' See summary.frontier_model

print.summary.frontier_model <- function(object, ...){
  print(
    paste('Total N. of borders:',
          object$n_borders)
  )

  print(
    'N. frontier / non-frontiers'
    )

  print(
    object$borders_tab
    )
  invisible()

}
