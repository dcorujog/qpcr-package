#' Read data from chainy output
#'
#' A function to read chainy output files from a specified directory and format them as needed for input in \code{\link{qpcr_analysis}}.
#' Note that the file name is used as placeholder for the primers column, which works well if every file contains data for a single target.
#' Otherwise you will have to edit the data to properly annotate the primers before running the \code{\link{qpcr_analysis}} function.
#'
#' @param directory directory where the files to read are located
#'
#' @return a data frame with the qPCR data formatted as needed for \code{\link{qpcr_analysis}}
#' @export
#'
#' @examples
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
