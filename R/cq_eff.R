#' cq_eff
#'
#' Calculates Cq and efficiency values for every well.
#'
#' @param qpcr_raw a data frame qpcr raw data with a row for every cycle and a
#' column for every sample.
#' @param type type of method to use for calculating Ct values and efficiencies
#' among those available for [qpcR::pcrbatch]
#'
#' @return
#' @export
#'
#' @examples
#'
#' @import qpcR

cq_eff <- function(qpcr_raw, type = "Cy0") {

  cq_eff_tab <- pcrbatch(qpcr_raw,
                         methods = 'sigfit',
                         remove = 'none',
                         verbose = FALSE,
                         group = NULL, ## test
                         plot = FALSE,
                         type = type)

  cq_eff_tab <- as.data.frame(cq_eff_tab)
  rownames(cq_eff_tab) <- cq_eff_tab$Vars
  cq_eff_tab <- cq_eff_tab[,-1]

  cq_eff_tab <- clean_spawning_dataframe(cq_eff_tab)

  names <- colnames(cq_eff_tab)
  eff <- as.numeric(unlist(cq_eff_tab["sig.eff",]))
  cq <- as.numeric(unlist(cq_eff_tab["sig.Cy0",]))

  df <- data.frame(names = names,
                   cq = cq,
                   efficiency = eff)

  return(df)
}
