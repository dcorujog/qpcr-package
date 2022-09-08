clean_spawning_dataframe <- function(df) {
  void <-  rep(NA, nrow(df))

  for (i in 1:ncol(df)) {
    if (is.matrix(df[, i])) {
      df[, i] <- void
    }
  }

  return(df)
}
