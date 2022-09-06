#' extract_well_lightcycler
#'
#' Helper function to extract single well data.
#'
#' @param dat fluorescence data for a given cycle.
#' @param i well number.
#'
#' @return A list with the well name and fluorescence signal.
#' @keywords internal
#'
extract_well_lightcycler <- function(dat, i) {
  num_cycles <- dat[[i]]$points$.attrs
  name <- dat[[i]]$.attrs[1]

  ## robust to detect both dot and comma as decimal marks
  fluo <- as.numeric(
    gsub(',', '.',
         as.character(as.vector(na.omit(sapply(dat[[i]]$points, "[", 3))))))

  to_return = list('well' = name, 'fluo' = fluo)

  return(to_return)
}
