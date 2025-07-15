launch_kinfitr_app <- function(bids_dir, derivatives_dir = NULL, subfolder = "Primary_Analysis", config_file = NULL, host = "0.0.0.0", port = 3838) {
  
  # Set default derivatives_dir if not provided
  if (is.null(derivatives_dir)) {
    derivatives_dir <- file.path(bids_dir, "derivatives", "kinfitr")
  }
  
  # Normalize paths to avoid double slashes
  bids_dir <- normalizePath(bids_dir, mustWork = TRUE)
  derivatives_dir <- normalizePath(derivatives_dir, mustWork = FALSE)
  
  # Validate that BIDS directory exists
  if (!dir.exists(bids_dir)) {
    stop(paste("BIDS directory does not exist:", bids_dir), call. = FALSE)
  }
  
  # Create derivatives directory if it doesn't exist
  if (!dir.exists(derivatives_dir)) {
    dir.create(derivatives_dir, recursive = TRUE)
    cat("Created derivatives directory:", derivatives_dir, "\n")
  }
  
  # Validate config file if provided
  if (!is.null(config_file) && !file.exists(config_file)) {
    stop(paste("Config file does not exist:", config_file), call. = FALSE)
  }
  
  # Print configuration
  cat("Starting kinfitr BIDS App with configuration:\n")
  cat("  BIDS directory:", bids_dir, "\n")
  cat("  Derivatives directory:", derivatives_dir, "\n")
  cat("  Subfolder:", subfolder, "\n")
  if (!is.null(config_file)) {
    cat("  Config file:", config_file, "\n")
  }
  cat("  Host:", host, "\n")
  cat("  Port:", port, "\n")
  
  # Create output directory if it doesn't exist
  output_dir <- file.path(derivatives_dir, subfolder)
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
    cat("Created output directory:", output_dir, "\n")
  }
  
  # Source the app with the configuration
  source("app.R", local = TRUE)
  
  # Launch the Shiny app
  library(shiny)
  runApp(shinyApp(ui = ui, server = server), host = host, port = port)
}