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
