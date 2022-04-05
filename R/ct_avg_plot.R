ct_avg_plot <- function(norm_data, max_ct = 35, exp_name = "exp_name") {
  ctavg <- ggplot(norm_data, aes(x = primers, y = ctaverage, color = primers, group = primers, size = ct_sd)) +
    geom_jitter(width = 0.2, alpha = 0.75) +
    geom_hline(yintercept = max_ct, color = "red") +
    scale_color_viridis(option = "turbo", discrete = TRUE) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  ggsave(filename = paste0(exp_name, "_ct_avg_plot.pdf"),
         plot = ctavg,
         device = cairo_pdf)
}
