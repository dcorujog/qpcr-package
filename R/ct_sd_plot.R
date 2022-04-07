#' QC plot of Ct standard deviation of technical replicates
#'
#' @param norm_data data frame with the normalized qPCR data as generated bu the \code{\link{qpcr_analysis}}
#' @param max_sd maximum standard deviation value between technical replicates to be considered acceptable, a horizontal red line will be drawn in the plot at this value (default = sd(c(0, 0.5)))
#' @param max_ct maximum Ct value considered for quantitative range, a vertical red line will be drawn in the plot at this value (default = 35)
#' @param exp_name experiment name (used as plot title and to name the saved file)
#'
#' @import ggplot2
#' @import viridis
#'
#' @return a pdf file is saved with the corresponding plot
#' @export
#'
#' @examples
ct_sd_plot <- function(norm_data, max_sd = sd(c(0, 0.5)), max_ct = 35, exp_name = "exp_name") {
  ctsd <- ggplot(norm_data, aes(x = ctaverage, y = ct_sd, color = primers, fill = primers)) +
    geom_point(alpha = 0.8) +
    geom_hline(yintercept = max_sd, color = "red") +
    geom_vline(xintercept = max_ct, color = "red") +
    scale_color_viridis(option = "turbo", discrete = TRUE)
  ggsave(filename = paste0(exp_name, "_ct_sd_plot.pdf"),
         plot = ctsd,
         device = cairo_pdf)
}
