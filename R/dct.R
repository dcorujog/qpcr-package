dct <- function(x, calib, eff.matrix) { # takes a row of data for a single sample, the ct values
  # of the calibrator sample and the efficiency of each primer pair, computes the dct value
  ct <- as.numeric(x["ctaverage"])
  sample <- x["sample"]
  primers <- x["primers"]
  eff.primers <- eff.matrix[eff.matrix$primers == primers, "efficiency"]
  dct_value <- eff.primers^(calib[primers] - ct)
  dct_value
}
