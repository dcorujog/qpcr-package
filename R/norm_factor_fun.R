#' Calculate normalization factor
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

  # Scatter plot of HK gene expression values
  hkg_centered <- as.data.frame(hkg_centered)
  hkg_plot <- ggplot(hkg_centered, aes(x = get(hkg[1]), y = get(hkg[2]), label = rownames(hkg_centered))) +
    coord_cartesian(xlim = c(min(hkg_centered), max(hkg_centered)),
                    ylim = c(min(hkg_centered), max(hkg_centered))) +
    geom_abline(slope = 1, intercept = 0, color = "darkblue") +
    geom_point() +
    geom_text(hjust = 0, nudge_x = 0.05) +
    labs(x = names(hkg_centered)[1],
         y = names(hkg_centered)[2])

  ggsave(filename = paste0(exp_name, "_hkg_scatter.pdf"),
         plot = hkg_plot,
         device = cairo_pdf)

  norm_factor
}
