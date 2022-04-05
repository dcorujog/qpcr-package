qpcr_analysis <- function( # MAIN FUNCTION
  ct_data, # Data frame, expected columns: names, cq, efficiency, primers, included
  type = "rtqpcr", # Analysis type, rtqpcr or qchip
  design, # Design matrix, at least one expected column "sample_name" and additional columns for each exp condition, for chip at least one has to be named IP
  calibsample, # String, name of the reference sample for dct calculation, will be 1 for all genes and the rest of the samples will be relative to it
  hkg, # Character vector with the "housekeeping" genes for normalization
  exp_name, # String, experiment name, will be used for naming saved files
  fix_names = FALSE, # Logical, fix names from chainy output if TRUE
  exclude = FALSE, # Logical, exclude samples with included = FALSE
  save_csv = TRUE # Logical save a csv file with the normalized data
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
  print(eff)
  ct <- aggregate(ct_data$cq, by = ct_data["primers"], FUN = mean, na.rm = T)
  names(ct)[2] <- "cq"
  save(eff, file =  paste0(exp_name, "_eff.RData"))
  write.csv(eff, paste0(exp_name, "_eff.csv"))
  save(ct, file = paste0(exp_name, "_ct.RData"))
  write.csv(ct, paste0(exp_name, "_ct.csv"))


  if (type == "rtqpcr") {

    # Calculate dct for the data

    dct_data <- dct_apply(ct_avg, calibsample, eff)

    # Normalization factor calculation

    norm_factor <- norm_factor_fun(dct_data, hkg, exp_name)

    # Normalize dct values

    norm_dct <- apply(dct_data, 1, function(x) as.numeric(x["dct"]) / norm_factor[as.character(x["sample"])])
    norm_data <- cbind(dct_data, "norm_dct" = norm_dct)
    write.csv(norm_data, file = paste0(exp_name, "_norm_data.csv"))

    # Create QC plots
    ct_sd_plot(norm_data, exp_name = exp_name)
    ct_avg_plot(norm_data, exp_name = exp_name)
    eff_avg_plot(norm_data, exp_name = exp_name)

    return(norm_data)
  }
}
