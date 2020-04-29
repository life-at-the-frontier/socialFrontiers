#' The very first function in the entire project that loads in where the
#'  data is and packages that we need

#' @import tidyverse
#' @import sf


##  Data repo base location (e.g. google drive)

startMyProject <-
  function(
  ){

  ## Calls up libraries
  pkgs <-
    c('tidyverse',
      'sf')

  sapply(pkgs, require, character.only = T)

  ##  Assign file paths out of this function to global variables using <<-
  dataBase_path <<-
    'C:/Users/mi1mz/Google Drive/Google Drive/NordF project/Data'

  }
