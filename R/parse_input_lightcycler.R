#' parse_input_lightcycler
#'
#' Parses an input xml file exported from the lightcycler software with the qpcr
#' curve data.
#'
#' @param infile character, path to input file
#'
#' @return A data frame with the fluorescence signal values for every cycle (rows)
#' for each sample (columns).
#'
#' @export
#'
#' @examples
#'
#' @importFrom XML xmlParse
#' @importFrom XML xmlRoot
#' @importFrom XML xmlToList

parse_input_lightcycler <- function(infile){
  tryCatch({
    doc <- xmlParse(infile)
    d = xmlRoot(doc)
    dat <- xmlToList(d)

    N_CYCLES <- as.numeric(dat[[1]]$points$.attrs[['count']])

    parsed_list <- list()
    parsed_list['Cycles'] <- list(seq(1, N_CYCLES))

    for (i in 1:length(dat)) {
      foo <- extract_well_lightcycler(dat,i)
      parsed_list[foo$well] <- list(foo$fluo)
    }

    parsed_dat <- as.data.frame(parsed_list)

    return(parsed_dat)
  }, error = function(x) return(NULL))

}
