eff_avg_plot <- function(norm_data, effrange = c(1.8, 2), exp_name = "exp_name") {
  effavg <- ggplot(norm_data, aes(x = primers, y = effaverage, color = ctaverage, group = primers)) +
    geom_point(alpha = 0.75) +
    geom_hline(yintercept = effrange, color = "red") +
    scale_color_viridis(option = "turbo")
  ggsave(filename = paste0(exp_name, "_eff_avg_plot.pdf"),
         plot = effavg,
         device = cairo_pdf)
}
