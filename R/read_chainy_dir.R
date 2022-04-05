read_chainy_dir <- function(directory = "./chainy_output/") {
  file_list <- list.files(directory)
  print(paste0("Files found: ", file_list))
  chainy_output <- data.frame()
  for (file in file_list) {
    temp_data <- read.delim(paste(directory, file, sep = "/"))
    index <- unlist(gregexpr("_cq_eff", file, fixed = T)) - 1
    target <- rep(substr(file, 0, index), nrow(temp_data))
    target <- toupper(target) # comment if you don't want to get all primer names in uppercase
    temp_data <- cbind(temp_data, "primers" = target)
    chainy_output <- rbind(chainy_output, temp_data)
  }
  names(chainy_output) <- c("names", "cq", "efficiency", "primers")
  chainy_output
}
