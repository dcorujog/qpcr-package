#' QC plot of primer efficiencies
#'
#' A dotplot for QC purposes with efficiency values for each sample and a color scale for the average Ct
#'
#' @param norm_data data frame with the normalized qPCR data as generated bu the \code{\link{qpcr_analysis}}
#' @param effrange a numeric vector with two elements, the lower and higher values for the efficiency range to be considered acceptable
#' @param exp_name experiment name (used as plot title and to name the saved file)
#'
#' @import ggplot2
#' @import viridisLite
#'
#' @return a pdf file is saved with the corresponding plot
#' @export
#'
#' @examples
eff_avg_plot <- function(norm_data, effrange = c(1.8, 2), exp_name = "exp_name") {
  effavg <- ggplot(norm_data, aes(x = primers, y = effaverage, color = ctaverage, group = primers)) +
    geom_point(alpha = 0.75) +
    geom_hline(yintercept = effrange, color = "red") +
    scale_color_viridis(option = "turbo")
  pdf(paste0(exp_name, "_eff_avg_plot.pdf"))
  print(effavg)
  dev.off()
}
