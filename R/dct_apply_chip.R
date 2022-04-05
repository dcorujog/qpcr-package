dct_apply_chip <- function (data, ...) { # helper function to apply the dct function to each row
  ctcalib <- data[data$input, "ctaverage"]
  names(ctcalib) <- unique(data$primers)
  dct_values <- apply(data, 1, dct, calib = ctcalib, eff.matrix = eff)
  data$dct <- dct_values
  data
}
