#' qPCR analysis
#'
#' A function for the analysis of qPCR data using the "delta Ct" method.
#'
#' @param ct_data a data frame with the qPCR input data. It needs to have the following columns: names, cq, efficiency, primers, included.
#' @param design a data frame containing the experimental design, at least one expected column called "sample_name" which has to match
#' the sample names in \code{ct_data} and additional columns for each experimental condition relevant in the design.
#' @param calibsample a character string with the name of the sample to be used as reference for dct calculation. This sample will have an
#' expression value of 1 for all targets and the rest of the samples will have values expressing their relative expression to this sample.
#' @param hkg a character vector with the name of the "housekeeping" genes used for normalization.
#' @param exp_name a character string with the name of the experiment, which will be used to name the files and objects generated by the function
#' @param fix_names logical indicating whether the sample names need to be "fixed" (default = FALSE, only relevant if directly using files generated with chainy to remove well position, dots and asterisks from sample names).
#' @param exclude logical indicating whether the logical column "exclude" from the input should be used to filter the data frame (default = FALSE).
#' @param save_csv logical indicating whether to save the final analyzed data frame as a csv file.
#' @param qc_plots logical indicating whether to generate and save QC plots.
#' @param out_dir directory for saving all results and plots (default = working directory)
#'
#' @return a data frame with the analyzed qPCR data containing normalized dct values.
#' @export
#'
#' @examples
qpcr_analysis <- function( # MAIN FUNCTION
  ct_data,
  design,
  calibsample,
  hkg,
  exp_name = "Experiment",
  fix_names = FALSE,
  exclude = FALSE,
  save_csv = TRUE,
  qc_plots = TRUE,
  out_dir = getwd()
) {

  # Fix names if ncessary (chainy output), otherwise keep as is

  if (fix_names) {
    ct_data$sample <- sapply(as.character(ct_data$names), fix_names, USE.NAMES = F)
  } else {
    ct_data$sample <- ct_data$names
  }

  # Exclude samples if necessary

  if (exclude) {
    ct_data <- ct_data[ct_data$included == TRUE,]
  }

  # Average ct of technical replicates

  ct_avg <- ct_avg_fun(ct_data)

  # Calculate median efficieny and average ct per primer pair

  eff <- aggregate(ct_avg$effaverage, by = ct_avg["primers"], FUN = median, na.rm = T)
  names(eff)[2] <- "efficiency"

  # Calculate dct for the data

  dct_data <- dct_apply(ct_avg, calibsample, eff)

  # Normalization factor calculation

  norm_factor <- norm_factor_fun(dct_data, hkg, exp_name)

  # Normalize dct values

  norm_dct <- apply(dct_data, 1, function(x) as.numeric(x["dct"]) / norm_factor[as.character(x["sample"])])
  norm_data <- cbind(dct_data, "norm_dct" = norm_dct)

  if(save_csv) {
    write.csv(norm_data, file = paste0(exp_name, "_norm_data.csv"))
    write.csv(eff, paste0(exp_name, "_eff.csv"))
  }

  # Create QC plots and return results
  if (qc_plots) {
    qc_ctsd <- ct_sd_plot(norm_data, exp_name = exp_name)
    qc_ctavg <- ct_avg_plot(norm_data, exp_name = exp_name)
    qc_effavg <- eff_avg_plot(norm_data, exp_name = exp_name)
    qc_hkgscatter <- hkg_scatter(dct_data, hkg = hkg, exp_name = exp_name)
    if (!is.null(qc_hkgscatter)) {
    return(list("norm_data" = norm_data,
                "qc_ctsd" = qc_ctsd,
                "qc_ctavg" = qc_ctavg,
                "qc_effavg" = qc_effavg,
                "qc_hkgscatter" = qc_hkgscatter))
    } else {
      return(list("norm_data" = norm_data,
                  "qc_ctsd" = qc_ctsd,
                  "qc_ctavg" = qc_ctavg,
                  "qc_effavg" = qc_effavg))
    }
  } else {
    return(norm_data)
  }
}
