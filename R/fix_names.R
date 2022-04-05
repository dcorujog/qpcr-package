#' Fix names output from chainy
#'
#' @usage fix_names(x)
#'
#' @param x
#'

fix_names <- function(x) { # fix names from chainy output (remove plate well coordinates)
  name <- as.character(x)
  index <- unlist(gregexpr("..", x, fixed = T)) + 2
  indexstar <- unlist(gregexpr("**", x, fixed = T))
  if (indexstar == -1) {
    fixed_name <- substr(name, index, nchar(x))
  } else {
    fixed_name <- substr(name, index, indexstar[2] - 1)
  }
  fixed_name
}
