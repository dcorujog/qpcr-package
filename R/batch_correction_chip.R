batch_correction_chip <- function(data, reference, value) {
  # Expects the following columns in the data:
  # "group" with your unique experimental groups
  # "biorep" with the unique experimental replicates (it is not necessary numeric)
  if (!"batch_cor" %in% names(data)) {
    data <- cbind(data, "batch_cor" = rep(0, length.out = nrow(data)))
  }
  for (i in unique(data$primers)) {
    for (j in unique(data$biorep)) {
      target_data <- data[data$primers == i & data$biorep == j, value]
      data[data$primers == i & data$biorep == j, "batch_cor"] <- target_data / mean(target_data, na.rm = T)
    }
    reference_avg <- mean(data[data$primers == i & data$group == reference, "batch_cor"], na.rm = T)
    data[data$primers == i, "batch_cor"] <- data[data$primers == i, "batch_cor"] / reference_avg
  }
  data
}
