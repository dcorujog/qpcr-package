dct_apply <- function (data, calibsample, eff) { # helper function to apply the dct function to each row
  ctcalib <- data[data$sample == calibsample, "ctaverage"]
  names(ctcalib) <- data[data$sample == calibsample, "primers"]
  dct_values <- apply(data, 1, dct, calib = ctcalib, eff.matrix = eff)
  new_data <- cbind(data, "dct" = dct_values)
  new_data
}
