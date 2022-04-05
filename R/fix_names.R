#' Fix names
#'
#' Remove well position, dots and asterisk symbols from sample names of chainy output files
#'
#' @param x A character vector of sample names
#'
#' @return A character vector of fixed sample names
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
