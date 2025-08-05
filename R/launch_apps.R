#' Launch kinfitr Apps
#'
#' @description Launch either the region definition app, modelling app, or both sequentially
#'
#' @param bids_dir Character string path to the BIDS directory (default: NULL)
#' @param derivatives_dir Character string path to derivatives directory (default: bids_dir/derivatives if bids_dir provided)
#' @param kinfitr_output_foldername Character string name for kinfitr output folder within derivatives (default: "kinfitr")
#' @param subfolder Character string name for analysis subfolder (default: "Primary_Analysis")
#' @param config_file Character string path to existing config file (optional, for modelling app)
#' @param region_definition Logical, whether to launch region definition app (default: FALSE)
#' @param modelling Logical, whether to launch modelling app (default: FALSE)
#' 
#' @details 
#' This function provides a unified interface to launch either or both kinfitr applications:
#' - Region Definition App: For creating brain region definitions and generating combined TACs
#' - Modelling App: For configuring kinetic models and creating analysis configurations
#' 
#' If both region_definition and modelling are TRUE, the region definition app will launch first,
#' followed by the modelling app after the first app closes.
#' 
#' Parameter usage:
#' - bids_dir, derivatives_dir, kinfitr_output_foldername: used by region definition app
#' - bids_dir, derivatives_dir, subfolder, config_file: used by modelling app
#' 
#' @export
launch_apps <- function(bids_dir = NULL, 
                       derivatives_dir = NULL, 
                       kinfitr_output_foldername = "kinfitr",
                       subfolder = "Primary_Analysis",
                       config_file = NULL,
                       region_definition = FALSE,
                       modelling = FALSE) {
  
  # Validate that at least one app is selected
  if (!region_definition && !modelling) {
    stop("At least one of 'region_definition' or 'modelling' must be TRUE", call. = FALSE)
  }
  
  # Print configuration
  cat("Launching kinfitr apps with configuration:\n")
  if (!is.null(bids_dir)) {
    cat("  BIDS directory:", bids_dir, "\n")
  }
  if (!is.null(derivatives_dir)) {
    cat("  Derivatives directory:", derivatives_dir, "\n")
  } else if (!is.null(bids_dir)) {
    cat("  Derivatives directory:", file.path(bids_dir, "derivatives"), "(default)\n")
  }
  cat("  kinfitr output folder:", kinfitr_output_foldername, "\n")
  cat("  Analysis subfolder:", subfolder, "\n")
  if (!is.null(config_file)) {
    cat("  Config file:", config_file, "\n")
  }
  cat("  Launch region definition:", region_definition, "\n")
  cat("  Launch modelling:", modelling, "\n")
  cat("\n")
  
  # Launch region definition app first if requested
  if (region_definition) {
    cat("=== Launching Region Definition App ===\n")
    region_definition_app(
      bids_dir = bids_dir,
      derivatives_dir = derivatives_dir,
      kinfitr_output_foldername = kinfitr_output_foldername
    )
    
    if (modelling) {
      cat("Region definition app closed. Preparing to launch modelling app...\n")
      Sys.sleep(1)  # Brief pause between apps
    }
  }
  
  # Launch modelling app if requested
  if (modelling) {
    cat("=== Launching Modelling App ===\n")
    modelling_app(
      bids_dir = bids_dir,
      derivatives_dir = derivatives_dir,
      subfolder = subfolder,
      config_file = config_file
    )
  }
  
  cat("App launcher completed.\n")
}