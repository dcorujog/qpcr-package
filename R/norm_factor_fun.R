#' Calculate normalization factor
#'
#' Calculate a normalization factor for each sample using the average of the dct values of all genes used for normalization.
#'
#' @param dct_data data.frame containing the qPCR data including dCt values as calculated by \code{\link{dct_apply}}
#' @param hkg a character vector containing the names of the targets used as normalization genes
#' @param exp_name experiment name (used as plot title and to name the saved file)
#'
#' @import ggplot2
#'
#' @return a named numeric vector with the normalization factor calculated for each sample. In addition, saves
#' a PDF containing a scatterplot to asses the correlation between the expression values of normalization genes (if using more than one)
#' @export
#'
#' @examples
norm_factor_fun <- function(dct_data, hkg, exp_name) { # calculate normalization factor for each sample and generate a scatterplot of housekeeping gene expression
  hkg_data <- dct_data[grep(paste(hkg, collapse = "|"), dct_data$primers),]
  hkg_centered <- aggregate(hkg_data$dct, by = hkg_data["primers"], FUN = function(x) x)
  hkg_centered <- t(hkg_centered[,2:ncol(hkg_centered)])
  colnames(hkg_centered) <- hkg
  rownames(hkg_centered) <- unique(hkg_data$sample)
  norm_factor <- apply(hkg_centered, 1, mean, na.rm = T)
  norm_factor
}
