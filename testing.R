
bids_dir <- "/home/granville/Repositories/OpenNeuro/ds004869/"

source("launch_app.R")
launch_kinfitr_app(bids_dir)

# launch_kinfitr_app(
#     bids_dir = "/path/to/bids",
#     derivatives_dir = "/custom/derivatives",
#     subfolder = "custom_analysis",
#     config_file = "/path/to/existing/config.json"
# )
