#' QC plot of Ct averages of technical replicates
#'
#' @param norm_data data frame with the normalized qPCR data as generated bu the \code{\link{qpcr_analysis}}
#' @param max_ct maximum Ct value considered for quantitative range, a horizontal red line will be drawn in the plot at this value (default = 35)
#' @param exp_name experiment name (used as plot title and to name the saved file)
#'
#' @import ggplot2
#' @import viridisLite
#'
#' @return a pdf file is saved with the corresponding plot
#' @export
#'
#' @examples
ct_avg_plot <- function(norm_data, max_ct = 35, exp_name = "exp_name") {
  ctavg <- ggplot(norm_data, aes(x = primers, y = ctaverage, color = primers, group = primers, size = ct_sd)) +
    geom_jitter(width = 0.2, alpha = 0.75) +
    geom_hline(yintercept = max_ct, color = "red") +
    #scale_color_viridis(option = "turbo", discrete = TRUE) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  pdf(paste0(exp_name, "_ct_avg_plot.pdf"))
  print(ctavg)
  dev.off()
}
