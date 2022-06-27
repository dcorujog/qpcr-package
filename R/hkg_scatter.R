#' Scatterplot of normalization genes
#'
#' @param dct_data
#' @param hkg
#'
#' @import ggplot2
#'
#' @return
#' @export
#'
#' @examples
hkg_scatter <- function(dct_data, hkg, exp_name) {

  hkg_data <- dct_data[grep(paste(hkg, collapse = "|"), dct_data$primers),]
  hkg_centered <- aggregate(hkg_data$dct, by = hkg_data["primers"], FUN = function(x) x)
  hkg_centered <- t(hkg_centered[,2:ncol(hkg_centered)])
  colnames(hkg_centered) <- hkg
  rownames(hkg_centered) <- unique(hkg_data$sample)
  hkg_centered <- as.data.frame(hkg_centered)

  if (length(hkg) > 1) { # Skip if using only one normalization gene
    hkg_plot <- ggplot(hkg_centered, aes(x = get(hkg[1]), y = get(hkg[2]), label = rownames(hkg_centered))) +
      coord_cartesian(xlim = c(min(hkg_centered), max(hkg_centered)),
                      ylim = c(min(hkg_centered), max(hkg_centered)),
                      clip = "off") +
      geom_abline(slope = 1, intercept = 0, color = "darkblue") +
      geom_point() +
      geom_text(hjust = 0, nudge_x = 0.05) +
      labs(x = names(hkg_centered)[1],
           y = names(hkg_centered)[2],
           title = exp_name)
    return(hkg_plot)
  }
}
