dct_group_chip <- function(dct_data, ...) {
  dct_data <- NULL
  for (i in 1:length(unique(ct_avg$group))) {
    group <- unique(ct_avg$group)[i]
    data <- ct_avg[ct_avg$group == group,]
    new_data <- dct_apply_chip(data, eff)
    if (is.null(dct_data)) {
      dct_data <- new_data
    } else {
      dct_data <- rbind(dct_data, new_data)
    }
  }
  dct_data
}
