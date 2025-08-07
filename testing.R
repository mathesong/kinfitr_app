roxygen2::roxygenise()
devtools::document()
devtools::load_all()

bids_dir <- "/home/granville/Repositories/OpenNeuro/ds004869/"
blood_dir <- "/home/granville/Repositories/OpenNeuro/ds004869/derivatives/bloodstream/"

modelling_app(bids_dir, blood_dir = blood_dir)




modelling_app(bids_dir)


region_definition_app(bids_dir)



# launch_kinfitr_app(
#     bids_dir = "/path/to/bids",
#     derivatives_dir = "/custom/derivatives",
#     subfolder = "custom_analysis",
#     config_file = "/path/to/existing/config.json"
# )
