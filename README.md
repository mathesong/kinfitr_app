# kinfitr_app


Now you have a clean launch_kinfitr_app() function that accepts
  the BIDS directory and other arguments directly. The function will:

  1. Use default paths: bids_dir/derivatives/kinfitr/Primary_Analysis
  2. Accept an optional existing config file path
  3. Create the output directory if needed
  4. Launch the Shiny app with the specified configuration

  You can now call it from RStudio like:
  source("launch_app.R")
  launch_kinfitr_app(bids_dir = "/path/to/bids")

  Or with custom paths:
  launch_kinfitr_app(
    bids_dir = "/path/to/bids",
    derivatives_dir = "/custom/derivatives",
    subfolder = "custom_analysis",
    config_file = "/path/to/existing/config.json"
  )
