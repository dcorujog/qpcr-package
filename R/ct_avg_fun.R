ct_avg_fun <- function(ct_data) {
  ct_avg <- data.frame()
  for (sample in unique(ct_data$sample)) {
    sample_data <- ct_data[ct_data$sample == sample,]
    sample_name <- sample_data[,"sample"]
    avg_ct <- aggregate(sample_data$cq, by = sample_data["primers"], FUN = mean, na.rm = F)
    names(avg_ct) <- c("primers", "ctaverage")
    avg_eff <- aggregate(sample_data$efficiency, by = sample_data["primers"], FUN = mean, na.rm = F)
    names(avg_eff) <- c("primers", "effaverage")
    ct_sd <- aggregate(sample_data$cq, by = sample_data["primers"], FUN = sd, na.rm = F)
    names(ct_sd) <- c("primers", "ct_sd")
    avg_sample_data <- data.frame("sample" = rep(sample_data[1, "sample"], nrow(avg_ct)),
                                  "primers" = avg_ct$primers, "effaverage" = avg_eff$effaverage, "ctaverage" = avg_ct$ctaverage, "ct_sd" = ct_sd$ct_sd)
    ct_avg <- rbind(ct_avg, avg_sample_data)
  }
  ct_avg
}