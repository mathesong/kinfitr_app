roxygen2::roxygenise()
devtools::load_all()

bids_dir <- "/home/granville/Repositories/OpenNeuro/ds004869/"

region_definition_app(bids_dir)

run_app(bids_dir)

# launch_kinfitr_app(
#     bids_dir = "/path/to/bids",
#     derivatives_dir = "/custom/derivatives",
#     subfolder = "custom_analysis",
#     config_file = "/path/to/existing/config.json"
# )
