# Configuration utility functions for kinfitr BIDS App
# Functions for generating and validating JSON configuration files

#' Detect Blood Data Sources
#'
#' @description Simple function to detect available blood data sources
#'
#' @param bids_dir Character string path to BIDS directory (optional)
#' @param derivatives_dir Character string path to derivatives directory
#' @return Character vector of available blood data sources
#' @export
detect_blood_sources <- function(bids_dir = NULL, derivatives_dir) {
  sources <- character(0)
  
  # Check bids_dir for _blood.tsv files
  if (!is.null(bids_dir) && dir.exists(bids_dir)) {
    blood_files <- list.files(bids_dir, pattern = "_blood\\.tsv$", recursive = TRUE)
    if (length(blood_files) > 0) {
      sources <- c(sources, "Raw: Generate input data from the raw blood data")
    }
  }
  
  # Check each folder in derivatives_dir
  if (dir.exists(derivatives_dir)) {
    deriv_folders <- list.dirs(derivatives_dir, recursive = FALSE)
    
    for (folder in deriv_folders) {
      folder_name <- basename(folder)
      
      # Check for _blood.tsv or _inputfunction.tsv files
      blood_files <- list.files(folder, pattern = "_blood\\.tsv$", recursive = TRUE)
      input_files <- list.files(folder, pattern = "_inputfunction\\.tsv$", recursive = TRUE)
      
      if (length(blood_files) > 0 || length(input_files) > 0) {
        # Look for dataset_description.json to determine subfolder structure
        dataset_desc_file <- file.path(folder, "dataset_description.json")
        
        if (file.exists(dataset_desc_file)) {
          # Check for subfolders with their own dataset_description.json
          subfolders <- list.dirs(folder, recursive = FALSE)
          derivative_sources <- character(0)
          
          for (subfolder in subfolders) {
            subfolder_desc <- file.path(subfolder, "dataset_description.json")
            if (file.exists(subfolder_desc)) {
              # Check if subfolder has blood/input data
              sub_blood_files <- list.files(subfolder, pattern = "_blood\\.tsv$", recursive = TRUE)
              sub_input_files <- list.files(subfolder, pattern = "_inputfunction\\.tsv$", recursive = TRUE)
              
              if (length(sub_blood_files) > 0 || length(sub_input_files) > 0) {
                subfolder_name <- basename(subfolder)
                derivative_sources <- c(derivative_sources, paste0("Derived: ", folder_name, "/", subfolder_name))
              }
            }
          }
          
          # If no subfolders with dataset_description.json found, use main folder
          if (length(derivative_sources) == 0) {
            sources <- c(sources, paste0("Derived: ", folder_name))
          } else {
            sources <- c(sources, derivative_sources)
          }
        } else {
          # No dataset_description.json but has blood/input files
          sources <- c(sources, paste0("Derived: ", folder_name))
        }
      }
    }
  }
  
  return(sources)
}
