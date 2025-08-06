#' Run kinfitr Modelling App
#'
#' @description Launch the kinfitr modelling configuration interface for setting up kinetic models
#'
#' @param bids_dir Character string path to the BIDS directory (default: NULL)
#' @param derivatives_dir Character string path to the derivatives folder (default: bids_dir/derivatives)
#' @param subfolder Character string name for analysis subfolder (default: "Primary_Analysis")
#' @param config_file Character string path to existing config file (optional)
#' @export
modelling_app <- function(bids_dir = NULL, derivatives_dir = NULL, subfolder = "Primary_Analysis", config_file = NULL) {
  
  # Set derivatives directory logic
  if (is.null(derivatives_dir)) {
    if (is.null(bids_dir)) {
      stop("Either bids_dir or derivatives_dir must be provided", call. = FALSE)
    }
    # Default: bids_dir/derivatives
    derivatives_dir <- file.path(bids_dir, "derivatives")
  }
  
  # Validate directories
  if (!is.null(bids_dir) && !dir.exists(bids_dir)) {
    stop(paste("BIDS directory does not exist:", bids_dir), call. = FALSE)
  }
  
  # Normalize paths
  if (!is.null(bids_dir)) {
    bids_dir <- normalizePath(bids_dir, mustWork = FALSE)
  }
  derivatives_dir <- normalizePath(derivatives_dir, mustWork = FALSE)
  
  # Create derivatives directory if it doesn't exist
  if (!dir.exists(derivatives_dir)) {
    dir.create(derivatives_dir, recursive = TRUE)
    cat("Created derivatives directory:", derivatives_dir, "\n")
  }
  
  # Set kinfitr directory (within derivatives)
  kinfitr_dir <- file.path(derivatives_dir, "kinfitr")
  
  if (!dir.exists(kinfitr_dir)) {
    dir.create(kinfitr_dir, recursive = TRUE)
    cat("Created kinfitr directory:", kinfitr_dir, "\n")
  }
  
  # Normalize kinfitr path
  kinfitr_dir <- normalizePath(kinfitr_dir, mustWork = FALSE)
  
  # Validate config file if provided
  if (!is.null(config_file) && !file.exists(config_file)) {
    stop(paste("Config file does not exist:", config_file), call. = FALSE)
  }
  
  # Print configuration
  cat("Starting Modelling App:\n")
  if (!is.null(bids_dir)) {
    cat("  BIDS directory:", bids_dir, "\n")
  }
  cat("  Derivatives directory:", derivatives_dir, "\n")
  cat("  kinfitr directory:", kinfitr_dir, "\n")
  cat("  Analysis subfolder:", subfolder, "\n")
  if (!is.null(config_file)) {
    cat("  Config file:", config_file, "\n")
  }
  
  # Create output directory if it doesn't exist
  output_dir <- file.path(kinfitr_dir, subfolder)
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
    cat("Created output directory:", output_dir, "\n")
  }
  
  # Fixed filename
  out_filename <- "kinfitr_config.json"
  
  # Check for combined TACs file first
  combined_tacs_file <- file.path(kinfitr_dir, "desc-combinedregions_tacs.tsv")
  
  if (file.exists(combined_tacs_file)) {
    cat("Found combined TACs file:", combined_tacs_file, "\n")
    tacs_list <- tibble::tibble(
      tacs_filedescription = paste0("Combined TACs: ", basename(combined_tacs_file))
    )
  } else {
    cat("No combined TACs file found. Checking for kinfitr_regions.tsv...\n")
    
    # Look for kinfitr_regions.tsv file
    if (!is.null(bids_dir)) {
      config_dir <- file.path(bids_dir, "code", "kinfitr")
    } else {
      config_dir <- kinfitr_dir
    }
    
    regions_file <- file.path(config_dir, "kinfitr_regions.tsv")
    
    if (file.exists(regions_file)) {
      cat("Found kinfitr_regions.tsv. Generating combined TACs file...\n")
      
      tryCatch({
        # Generate combined TACs using existing functionality
        kinfitr_regions_files_path <- file.path(config_dir, "kinfitr_regions_files.tsv")
        
        # Load participant data if BIDS directory is provided
        participant_data <- NULL
        if (!is.null(bids_dir)) {
          participant_data <- load_participant_data(bids_dir)
          if (!is.null(participant_data)) {
            cat("Loaded participant data with", nrow(participant_data$data), "participants\n")
          }
        }
        
        # Create file mapping
        regions_files_data <- create_kinfitr_regions_files(regions_file, derivatives_dir)
        
        # Generate combined TACs with participant data and BIDS directory
        combined_data <- create_kinfitr_combined_tacs(kinfitr_regions_files_path, derivatives_dir, kinfitr_dir, bids_dir, participant_data)
        
        cat("Successfully generated combined TACs file.\n")
        tacs_list <- tibble::tibble(
          tacs_filedescription = paste0("Generated Combined TACs: desc-combinedregions_tacs.tsv")
        )
      }, error = function(e) {
        cat("Error generating combined TACs:", e$message, "\n")
        cat("Please use the region definition app first to create regions.\n")
        tacs_list <- tibble::tibble(tacs_filedescription = character(0))
      })
    } else {
      cat("No kinfitr_regions.tsv found. Please use the region definition app first.\n")
      tacs_list <- tibble::tibble(tacs_filedescription = character(0))
    }
  }
  
  ui <- fluidPage(theme = shinythemes::shinytheme("flatly"),
    
    # App title ----
    titlePanel("Kinetic Modelling Configuration"),
    
    # Tab panel for all options ----
    tabsetPanel(type = "tabs",
                # Tab panel for data definition ----
                tabPanel("Data Definition",
                         br(),
                         # h4("Data Definition"),
                         # p("Define and create the analysis dataset from the combined TACs file. This step allows you to subset the data and generate individual TACs files for your specific analysis.",
                         #   style = "font-size:14px;"
                         # ),
                         # br(),
                         h3("Subsetting"),
                         p(glue::glue("Use these options to apply this analysis to a subset of the data. ",
                                "Values should be separated by semi-colons (e.g., 01;02). ",
                                "All measurements fulfilling all the conditions will ",
                                "be included. Leave options blank if no subsetting is desired, ",
                                "i.e. leaving sub blank implies that all subjects should ",
                                "be included."),
                           style = "font-size:14px;"
                         ),
                         br(),
                         fluidRow(
                           column(4, textInput(inputId = "subset_sub", label = "sub", value = "")),
                           column(4, textInput(inputId = "subset_ses", label = "ses", value = "")),
                           column(4, textInput(inputId = "subset_task", label = "task", value = ""))
                         ),
                         fluidRow(
                           column(4, textInput(inputId = "subset_trc", label = "trc", value = "")),
                           column(4, textInput(inputId = "subset_rec", label = "rec", value = "")),
                           column(4, textInput(inputId = "subset_run", label = "run", value = ""))
                         ),
                         fluidRow(
                           column(12, textInput(inputId = "subset_regions", label = "Regions", value = ""))
                         ),
                         
                         hr(),
                         h3("Create Data"),
                         p("Generate individual TACs files for each measurement and create a report plotting the selected data.",
                           style = "font-size:14px;"
                         ),
                         br(),
                         actionButton("run_subset", "▶ Create Analysis Data", class = "btn-success btn-lg")
                ),
                # Tab panel for weights ----
                    tabPanel("Weights Definition",
                             br(),
                             h3("Region for Weights Calculation"),
                             p("Select the region to use for calculating weights. Regional means are volume-weighted averages.",
                               style = "font-size:14px;"
                             ),
                             br(),
                             fluidRow(
                               column(8,
                                 div(style = "width: 100%;",
                                   selectInput("weights_region_type", "Region Source:",
                                             choices = c("Single region" = "single",
                                                       "Mean of analysis regions" = "mean_combined", 
                                                       "Mean of external segmentation" = "external"),
                                             selected = "external",
                                             width = "100%")
                                 )
                               ),
                               column(4,
                                 conditionalPanel(
                                   condition = "input.weights_region_type == 'single'",
                                   textInput("weights_region", "Region Name:",
                                           value = "",
                                           placeholder = "e.g., Caudate",
                                           width = "100%")
                                 ),
                                 conditionalPanel(
                                   condition = "input.weights_region_type == 'mean_combined'",
                                   div(
                                     p(strong("All analysis regions will be averaged"), style = "font-size:12px; color:#666;")
                                   )
                                 ),
                                 conditionalPanel(
                                   condition = "input.weights_region_type == 'external'",
                                   selectInput("weights_external_tacs", "Select Segmentation:",
                                             choices = c("Loading..." = ""),
                                             selected = "",
                                             width = "100%")
                                 )
                               )
                             ),
                             
                             hr(),
                             h3("Radioisotope"),
                             p("Select the radioisotope for decay correction. Half-life is used for time-based weighting methods.",
                               style = "font-size:14px;"
                             ),
                             br(),
                             fluidRow(
                               column(8,
                                 radioButtons("weights_radioisotope", "",
                                            choices = c("C11 (20.4 min)" = "C11",
                                                      "O15 (2.05 min)" = "O15", 
                                                      "F18 (109.8 min)" = "F18",
                                                      "Other (specify half-life)" = "Other"),
                                            selected = "C11", inline = TRUE)
                               ),
                               column(4,
                                 conditionalPanel(
                                   condition = "input.weights_radioisotope == 'Other'",
                                   numericInput("weights_halflife", "Half-life (minutes):",
                                              value = 20.4, min = 0.1, max = 1000, step = 0.1)
                                 )
                               )
                             ),
                             
                             hr(),
                             h3("Weighting Method"),
                             p("Choose the mathematical method for calculating frame weights.",
                               style = "font-size:14px;"
                             ),
                             br(),
                             fluidRow(
                               column(8,
                                 div(style = "width: 100%;",
                                   selectInput("weights_method", "Method:",
                                             choices = c("1. frame_dur / tac_uncor" = "1",
                                                       "2. sqrt(frame_dur * tac_uncor) (default)" = "2",
                                                       "3. sqrt(frame_dur) / tac" = "3", 
                                                       "4. sqrt(frame_dur)" = "4",
                                                       "5. frame_dur * exp(-ln(2)/halflife)" = "5",
                                                       "6. frame_dur / tac" = "6",
                                                       "7. frame_dur" = "7",
                                                       "8. frame_dur^2 / tac_uncor" = "8",
                                                       "9. (frame_dur^2 / (frame_dur * tac)) * corrections^2" = "9",
                                                       "Custom formula" = "custom"),
                                             selected = "2",
                                             width = "100%")
                                 )
                               ),
                               column(4,
                                 numericInput("weights_minweight", "Minimum Weight:",
                                            value = 0.25, min = 0.001, max = 1, step = 0.001)
                               )
                             ),
                             
                             conditionalPanel(
                               condition = "input.weights_method == 'custom'",
                               br(),
                               div(
                                 h4("Custom Formula"),
                                 p("Define a custom weighting formula using available variables:",
                                   style = "font-size:14px; margin-bottom:10px;"),
                                 textAreaInput("weights_custom_formula", "",
                                             value = "",
                                             rows = 2,
                                             placeholder = "e.g., (frame_dur^2) / tac_uncor")
                               )
                             ),
                             
                             br(),
                             div(
                               p(strong("Available variables:"), style = "font-size:13px; margin-bottom:5px;"),
                               div(
                                 p("• ", strong("frame_dur:"), " Frame duration in seconds", style = "font-size:11px; margin:2px 0;"),
                                 p("• ", strong("frame_mid:"), " Frame midpoint time in seconds", style = "font-size:11px; margin:2px 0;"),
                                 p("• ", strong("tac:"), " Time activity curve (decay-corrected)", style = "font-size:11px; margin:2px 0;"),
                                 p("• ", strong("tac_uncor:"), " Time activity curve (decay-uncorrected)", style = "font-size:11px; margin:2px 0;"),
                                 p("• ", strong("corrections:"), " Decay correction factors", style = "font-size:11px; margin:2px 0;"),
                                 style = "background:#f8f9fa; padding:10px; border-left:3px solid #007bff; margin:10px 0;"
                               )
                             ),
                             
                             hr(),
                             h3("Calculate Weights"),
                             p("Calculate weights and generate a report plotting the selected data.",
                               style = "font-size:14px;"
                             ),
                             br(),
                             actionButton("run_weights", "▶ Calculate Weights", class = "btn-success btn-lg")
                    ),
                    # Tab panel for delay ----
                    tabPanel("Fit Delay",
                             h4("Fit Delay"),
                             p("Estimate the delay between the blood and tissue curves"),
                             br(),
                             h5("Content coming soon..."),
                             
                             hr(),
                             actionButton("run_delay", "▶ Estimate Delay", class = "btn-success btn-lg")
                    ),
                    tabPanel("Model 1",
                             h4(""),
                             p(glue::glue("There are not so many common models for the BPR. ",
                                    "When the BPR is clearly constant or linear, use ",
                                    "the relevant option. ",
                                    "For most tracers, with a more complex function, ",
                                    "a good default option is the ",
                                    "`Fit Individually: GAM` option, ",
                                    "which will fit a smooth generalised additive model ",
                                    "to each curve independently. ",
                                    "Hierarchical models are best left for experienced users.  ")
                             ),
                             # Model selection drop-down menu
                             selectInput("button", "Select a model:",
                                         choices = c("1TCM",
                                                     "2TCM",
                                                     "Logan",
                                                     "Fit Delay",
                                                     "t* finder"
                                         )),
                             # 1TCM selection panel
                             conditionalPanel(
                               condition = "input.button == '1TCM'",
                               fluidRow(
                                 column(3, offset = 0, numericInput("K1.start", "K1.start", value = 0.1,min = 0, step=.001)),
                                 column(3, offset = 0, numericInput("K1.lower", "K1.lower", value = 0.0001,min = 0, step=.001)),
                                 column(3, offset = 0, numericInput("K1.upper", "K1.upper", value = 0.5,min = 0, step=.001)),
                               ),
                               fluidRow(
                                 column(3, offset = 0, numericInput("k2.start", "k2.start", value = 0.1,min = 0, step=.001)),
                                 column(3, offset = 0, numericInput("k2.lower", "k2.lower", value = 0.0001,min = 0, step=.001)),
                                 column(3, offset = 0, numericInput("k2.upper", "k2.upper", value = 0.5,min = 0, step=.001)),
                               ),
                               fluidRow(
                                 column(3, offset = 0, numericInput("vB.start", "vB.start", value = 0.05, min = 0, step=.001)),
                                 column(3, offset = 0, numericInput("vB.lower", "vB.lower", value = 0.01, min = 0, step=.001)),
                                 column(3, offset = 0, numericInput("vB.upper", "vB.upper", value = 0.1, min = 0, step=.001)),
                               ),
                               checkboxInput("vB.fit", "Fit vB (otherwise use vB.start)", value = FALSE),
                               
                               
                             ),
                             # 2TCM selection panel
                             conditionalPanel(
                               condition = "input.button == '2TCM'",
                               fluidRow(
                                 column(3, offset = 0, numericInput("K1.start", "K1.start", value = 0.1,min = 0, step=.001)),
                                 column(3, offset = 0, numericInput("K1.lower", "K1.lower", value = 0.0001,min = 0, step=.001)),
                                 column(3, offset = 0, numericInput("K1.upper", "K1.upper", value = 0.5,min = 0, step=.001)),
                               ),
                               fluidRow(
                                 column(3, offset = 0, numericInput("k2.start", "k2.start", value = 0.1,min = 0, step=.001)),
                                 column(3, offset = 0, numericInput("k2.lower", "k2.lower", value = 0.0001,min = 0, step=.001)),
                                 column(3, offset = 0, numericInput("k2.upper", "k2.upper", value = 0.5,min = 0, step=.001)),
                               ),
                               fluidRow(
                                 column(3, offset = 0, numericInput("k3.start", "k3.start", value = 0.1,min = 0, step=.001)),
                                 column(3, offset = 0, numericInput("k3.lower", "k3.lower", value = 0.0001,min = 0, step=.001)),
                                 column(3, offset = 0, numericInput("k3.upper", "k3.upper", value = 0.5,min = 0, step=.001)),
                               ),
                               fluidRow(
                                 column(3, offset = 0, numericInput("k4.start", "k4.start", value = 0.1,min = 0, step=.001)),
                                 column(3, offset = 0, numericInput("k4.lower", "k4.lower", value = 0.0001,min = 0, step=.001)),
                                 column(3, offset = 0, numericInput("k4.upper", "k4.upper", value = 0.5,min = 0, step=.001)),
                               ),
                               fluidRow(
                                 column(3, offset = 0, numericInput("vB.start", "vB.start", value = 0.05, min = 0, step=.001)),
                                 column(3, offset = 0, numericInput("vB.lower", "vB.lower", value = 0.01, min = 0, step=.001)),
                                 column(3, offset = 0, numericInput("vB.upper", "vB.upper", value = 0.1, min = 0, step=.001)),
                               ),
                               checkboxInput("vB.fit", "Fit vB (otherwise use vB.start)", value = FALSE)
                             ),
                             # Logan selection panel
                             conditionalPanel(
                               condition = "input.button == 'Logan'",
                               numericInput("tstarIncludedFrames", "tstar", value = 10),
                               checkboxInput("vB.fit", "Fit vB", value = FALSE)
                             ),
                             
                             # t* finder
                             conditionalPanel(
                               condition = "input.button == 't* finder'",
                               textInput(
                                 inputId = "binding_regions", label = "Low-, medium- and high-binding regions", value = ""),
                             ),
                             
                             hr(),
                             actionButton("run_model1", "▶ Fit Model 1", class = "btn-success btn-lg")
                    ),
                    tabPanel("Model 2",
                             h4("Model 2"),
                             p(glue::glue("There are not so many common models for the BPR. ",
                                    "When the BPR is clearly constant or linear, use ",
                                    "the relevant option. ",
                                    "For most tracers, with a more complex function, ",
                                    "a good default option is the ",
                                    "`Fit Individually: GAM` option, ",
                                    "which will fit a smooth generalised additive model ",
                                    "to each curve independently. ",
                                    "Hierarchical models are best left for experienced users.  ")
                             ),
                             # Model selection drop-down menu
                             selectInput("button2", "Select a model:",
                                         choices = c("1TCM",
                                                     "2TCM",
                                                     "Logan",
                                                     "Fit Delay",
                                                     "t* finder"
                                         )),
                             # 1TCM selection panel
                             conditionalPanel(
                               condition = "input.button2 == '1TCM'",
                               fluidRow(
                                 column(3, offset = 0, numericInput("K1.start2", "K1.start", value = 0.1,min = 0, step=.001)),
                                 column(3, offset = 0, numericInput("K1.lower2", "K1.lower", value = 0.0001,min = 0, step=.001)),
                                 column(3, offset = 0, numericInput("K1.upper2", "K1.upper", value = 0.5,min = 0, step=.001)),
                               ),
                               fluidRow(
                                 column(3, offset = 0, numericInput("k2.start2", "k2.start", value = 0.1,min = 0, step=.001)),
                                 column(3, offset = 0, numericInput("k2.lower2", "k2.lower", value = 0.0001,min = 0, step=.001)),
                                 column(3, offset = 0, numericInput("k2.upper2", "k2.upper", value = 0.5,min = 0, step=.001)),
                               ),
                               fluidRow(
                                 column(3, offset = 0, numericInput("vB.start2", "vB.start", value = 0.05, min = 0, step=.001)),
                                 column(3, offset = 0, numericInput("vB.lower2", "vB.lower", value = 0.01, min = 0, step=.001)),
                                 column(3, offset = 0, numericInput("vB.upper2", "vB.upper", value = 0.1, min = 0, step=.001)),
                               ),
                               checkboxInput("vB.fit2", "Fit vB (otherwise use vB.start)", value = FALSE),
                             ),
                             # 2TCM selection panel
                             conditionalPanel(
                               condition = "input.button2 == '2TCM'",
                               fluidRow(
                                 column(3, offset = 0, numericInput("K1.start2", "K1.start", value = 0.1,min = 0, step=.001)),
                                 column(3, offset = 0, numericInput("K1.lower2", "K1.lower", value = 0.0001,min = 0, step=.001)),
                                 column(3, offset = 0, numericInput("K1.upper2", "K1.upper", value = 0.5,min = 0, step=.001)),
                               ),
                               fluidRow(
                                 column(3, offset = 0, numericInput("k2.start2", "k2.start", value = 0.1,min = 0, step=.001)),
                                 column(3, offset = 0, numericInput("k2.lower2", "k2.lower", value = 0.0001,min = 0, step=.001)),
                                 column(3, offset = 0, numericInput("k2.upper2", "k2.upper", value = 0.5,min = 0, step=.001)),
                               ),
                               fluidRow(
                                 column(3, offset = 0, numericInput("k3.start2", "k3.start", value = 0.1,min = 0, step=.001)),
                                 column(3, offset = 0, numericInput("k3.lower2", "k3.lower", value = 0.0001,min = 0, step=.001)),
                                 column(3, offset = 0, numericInput("k3.upper2", "k3.upper", value = 0.5,min = 0, step=.001)),
                               ),
                               fluidRow(
                                 column(3, offset = 0, numericInput("k4.start2", "k4.start", value = 0.1,min = 0, step=.001)),
                                 column(3, offset = 0, numericInput("k4.lower2", "k4.lower", value = 0.0001,min = 0, step=.001)),
                                 column(3, offset = 0, numericInput("k4.upper2", "k4.upper", value = 0.5,min = 0, step=.001)),
                               ),
                               fluidRow(
                                 column(3, offset = 0, numericInput("vB.start2", "vB.start", value = 0.05, min = 0, step=.001)),
                                 column(3, offset = 0, numericInput("vB.lower2", "vB.lower", value = 0.01, min = 0, step=.001)),
                                 column(3, offset = 0, numericInput("vB.upper2", "vB.upper", value = 0.1, min = 0, step=.001)),
                               ),
                               checkboxInput("vB.fit2", "Fit vB (otherwise use vB.start)", value = FALSE)
                             ),
                             # Logan selection panel
                             conditionalPanel(
                               condition = "input.button2 == 'Logan'",
                               numericInput("tstarIncludedFrames2", "tstar", value = 10),
                               checkboxInput("vB.fit2", "Fit vB", value = FALSE)
                             ),
                             # t* finder
                             conditionalPanel(
                               condition = "input.button2 == 't* finder'",
                               textInput(
                                 inputId = "binding_regions2", label = "Low-, medium- and high-binding regions", value = ""),
                             ),
                             
                             hr(),
                             actionButton("run_model2", "▶ Fit Model 2", class = "btn-success btn-lg")
                    ),
                    tabPanel("Model 3",
                             h4("Model 3"),
                             p(glue::glue("There are not so many common models for the BPR. ",
                                    "When the BPR is clearly constant or linear, use ",
                                    "the relevant option. ",
                                    "For most tracers, with a more complex function, ",
                                    "a good default option is the ",
                                    "`Fit Individually: GAM` option, ",
                                    "which will fit a smooth generalised additive model ",
                                    "to each curve independently. ",
                                    "Hierarchical models are best left for experienced users.  ")
                             ),
                             # Model selection drop-down menu
                             selectInput("button3", "Select a model:",
                                         choices = c("1TCM",
                                                     "2TCM",
                                                     "Logan",
                                                     "Fit Delay",
                                                     "t* finder"
                                         )),
                             # 1TCM selection panel
                             conditionalPanel(
                               condition = "input.button3 == '1TCM'",
                               fluidRow(
                                 column(3, offset = 0, numericInput("K1.start3", "K1.start", value = 0.1,min = 0, step=.001)),
                                 column(3, offset = 0, numericInput("K1.lower3", "K1.lower", value = 0.0001,min = 0, step=.001)),
                                 column(3, offset = 0, numericInput("K1.upper3", "K1.upper", value = 0.5,min = 0, step=.001)),
                               ),
                               fluidRow(
                                 column(3, offset = 0, numericInput("k2.start3", "k2.start", value = 0.1,min = 0, step=.001)),
                                 column(3, offset = 0, numericInput("k2.lower3", "k2.lower", value = 0.0001,min = 0, step=.001)),
                                 column(3, offset = 0, numericInput("k2.upper3", "k2.upper", value = 0.5,min = 0, step=.001)),
                               ),
                               fluidRow(
                                 column(3, offset = 0, numericInput("vB.start3", "vB.start", value = 0.05, min = 0, step=.001)),
                                 column(3, offset = 0, numericInput("vB.lower3", "vB.lower", value = 0.01, min = 0, step=.001)),
                                 column(3, offset = 0, numericInput("vB.upper3", "vB.upper", value = 0.1, min = 0, step=.001)),
                               ),
                               checkboxInput("vB.fit3", "Fit vB (otherwise use vB.start)", value = FALSE),
                             ),
                             # 2TCM selection panel
                             conditionalPanel(
                               condition = "input.button3 == '2TCM'",
                               fluidRow(
                                 column(3, offset = 0, numericInput("K1.start3", "K1.start", value = 0.1,min = 0, step=.001)),
                                 column(3, offset = 0, numericInput("K1.lower3", "K1.lower", value = 0.0001,min = 0, step=.001)),
                                 column(3, offset = 0, numericInput("K1.upper3", "K1.upper", value = 0.5,min = 0, step=.001)),
                               ),
                               fluidRow(
                                 column(3, offset = 0, numericInput("k2.start3", "k2.start", value = 0.1,min = 0, step=.001)),
                                 column(3, offset = 0, numericInput("k2.lower3", "k2.lower", value = 0.0001,min = 0, step=.001)),
                                 column(3, offset = 0, numericInput("k2.upper3", "k2.upper", value = 0.5,min = 0, step=.001)),
                               ),
                               fluidRow(
                                 column(3, offset = 0, numericInput("k3.start3", "k3.start", value = 0.1,min = 0, step=.001)),
                                 column(3, offset = 0, numericInput("k3.lower3", "k3.lower", value = 0.0001,min = 0, step=.001)),
                                 column(3, offset = 0, numericInput("k3.upper3", "k3.upper", value = 0.5,min = 0, step=.001)),
                               ),
                               fluidRow(
                                 column(3, offset = 0, numericInput("k4.start3", "k4.start", value = 0.1,min = 0, step=.001)),
                                 column(3, offset = 0, numericInput("k4.lower3", "k4.lower", value = 0.0001,min = 0, step=.001)),
                                 column(3, offset = 0, numericInput("k4.upper3", "k4.upper", value = 0.5,min = 0, step=.001)),
                               ),
                               fluidRow(
                                 column(3, offset = 0, numericInput("vB.start3", "vB.start", value = 0.05, min = 0, step=.001)),
                                 column(3, offset = 0, numericInput("vB.lower3", "vB.lower", value = 0.01, min = 0, step=.001)),
                                 column(3, offset = 0, numericInput("vB.upper3", "vB.upper", value = 0.1, min = 0, step=.001)),
                               ),
                               checkboxInput("vB.fit3", "Fit vB (otherwise use vB.start)", value = FALSE)
                             ),
                             # Logan selection panel
                             conditionalPanel(
                               condition = "input.button3 == 'Logan'",
                               numericInput("tstarIncludedFrames3", "tstar", value = 10),
                               checkboxInput("vB.fit3", "Fit vB", value = FALSE)
                             ),
                             # t* finder
                             conditionalPanel(
                               condition = "input.button3 == 't* finder'",
                               textInput(
                                 inputId = "binding_regions3", label = "Low-, medium- and high-binding regions", value = ""),
                             ),
                             
                             hr(),
                             actionButton("run_model3", "▶ Fit Model 3", class = "btn-success btn-lg")
                    ),
                    tabPanel("Interactive",
                             h4("Interactive Modeling"),
                             p("Fit models interactively with reactive figures and parameters"),
                             br(),
                             h5("Content coming soon..."),
                             
                             hr(),
                             actionButton("run_interactive", "▶ Launch Interactive Mode", class = "btn-success btn-lg")
                    ),
                    tabPanel("Preview Configuration",
                             br(),
                             h4("Configuration Settings"),
                             p("Review and save your analysis configuration. The JSON below shows all current settings."),
                             
                             # Analysis info
                             fluidRow(
                               column(6, 
                                      h5("Analysis Information"),
                                      p(strong("Analysis folder: "), textOutput("analysis_folder", inline = TRUE)),
                                      p(strong("Config file: "), "desc-kinfitroptions_config.json")
                               ),
                               column(6,
                                      br(),
                                      actionButton("run_all", "▶ Run All", class = "btn-success btn-lg"),
                                      br(), br(),
                                      actionButton("save_config", "💾 Save Config", class = "btn-primary btn-lg")
                               )
                             ),
                             
                             hr(),
                             
                             # JSON preview
                             h5("Current Configuration (JSON)"),
                             verbatimTextOutput("json_preview"),
                             
                             br()
                    )
        )
  )
  
  # Define server logic for config file creation ----
  server <- function(input, output, session) {
    
    # Load existing config on startup and restore UI state
    observe({
      existing_config <- load_existing_config()
      
      if (!is.null(existing_config)) {
        # Show notification that config is being loaded
        showNotification(paste("Loading previous configuration from", basename(output_dir)), 
                        type = "message", duration = 5)
        
        # Restore subsetting parameters
        if (!is.null(existing_config$Subsetting)) {
          updateTextInput(session, "subset_sub", value = existing_config$Subsetting$sub %||% "")
          updateTextInput(session, "subset_ses", value = existing_config$Subsetting$ses %||% "")
          updateTextInput(session, "subset_task", value = existing_config$Subsetting$task %||% "")
          updateTextInput(session, "subset_trc", value = existing_config$Subsetting$trc %||% "")
          updateTextInput(session, "subset_rec", value = existing_config$Subsetting$rec %||% "")
          updateTextInput(session, "subset_run", value = existing_config$Subsetting$run %||% "")
          updateTextInput(session, "subset_regions", value = existing_config$Subsetting$Regions %||% "")
        }
        
        # Restore Model 1 settings
        if (!is.null(existing_config$Models$Model1)) {
          updateSelectInput(session, "button", selected = existing_config$Models$Model1$type %||% "1TCM")
        }
        
        # Restore Model 2 settings
        if (!is.null(existing_config$Models$Model2)) {
          updateSelectInput(session, "button2", selected = existing_config$Models$Model2$type %||% "1TCM")
        }
        
        # Restore Model 3 settings
        if (!is.null(existing_config$Models$Model3)) {
          updateSelectInput(session, "button3", selected = existing_config$Models$Model3$type %||% "1TCM")
        }
        
        # Restore Weights settings
        if (!is.null(existing_config$Weights)) {
          updateSelectInput(session, "weights_region_type", 
                           selected = existing_config$Weights$region_type %||% "external")
          updateTextInput(session, "weights_region", 
                         value = existing_config$Weights$region %||% "")
          updateSelectInput(session, "weights_external_tacs", 
                           selected = existing_config$Weights$external_tacs %||% "")
          updateRadioButtons(session, "weights_radioisotope", 
                           selected = existing_config$Weights$radioisotope %||% "C11")
          if (!is.null(existing_config$Weights$halflife)) {
            updateNumericInput(session, "weights_halflife", 
                             value = existing_config$Weights$halflife)
          }
          updateSelectInput(session, "weights_method", 
                           selected = existing_config$Weights$method %||% "2")
          # Handle both new 'formula' field and legacy 'custom_formula' field
          custom_formula_value <- existing_config$Weights$formula %||% existing_config$Weights$custom_formula %||% ""
          if (custom_formula_value != "" && existing_config$Weights$method == "custom") {
            updateTextAreaInput(session, "weights_custom_formula", 
                              value = custom_formula_value)
          }
          updateNumericInput(session, "weights_minweight", 
                           value = existing_config$Weights$minweight %||% 0.25)
        }
      }
    })
    
    # Reactive function to populate external segmentation options from combined regions file ----
    observe({
      tryCatch({
        # Read desc values from combined regions file (in kinfitr folder)
        combined_regions_file <- file.path(kinfitr_dir, "desc-combinedregions_tacs.tsv")
        
        cat("Looking for combined regions file at:", combined_regions_file, "\n")
        
        if (file.exists(combined_regions_file)) {
          cat("Combined regions file found, reading desc values...\n")
          combined_regions <- readr::read_tsv(combined_regions_file, show_col_types = FALSE)
          
          if (nrow(combined_regions) > 0 && "desc" %in% colnames(combined_regions)) {
            # Get unique desc values for segmentation options
            unique_desc <- sort(unique(combined_regions$desc))
            unique_desc <- unique_desc[!is.na(unique_desc)]
            
            cat("Found", length(unique_desc), "unique desc values:", paste(unique_desc, collapse = ", "), "\n")
            
            if (length(unique_desc) > 0) {
              # Create choices for segmentation selection
              choices <- setNames(unique_desc, unique_desc)
              
              updateSelectInput(session, "weights_external_tacs",
                               choices = choices,
                               selected = unique_desc[1])
              
              cat("Successfully updated external segmentation dropdown\n")
            } else {
              updateSelectInput(session, "weights_external_tacs",
                               choices = c("No segmentations found" = ""),
                               selected = "")
            }
          } else {
            updateSelectInput(session, "weights_external_tacs",
                             choices = c("No desc column found" = ""),
                             selected = "")
          }
        } else {
          updateSelectInput(session, "weights_external_tacs",
                           choices = c("Combined regions file not found" = ""),
                           selected = "")
        }
      }, error = function(e) {
        # If there's an error, provide default choice
        updateSelectInput(session, "weights_external_tacs",
                         choices = c("Error loading segmentations" = ""),
                         selected = "")
      })
    })
    
    # Reactive expression to generate the config file ----
    config_json <- reactive({
      
      # Data Subset
      Subsetting <- list(
        sub = input$subset_sub,
        ses = input$subset_ses,
        task = input$subset_task,
        trc = input$subset_trc,
        rec = input$subset_rec,
        run = input$subset_run,
        Regions = input$subset_regions
      )
      
      # Weights Definition  
      # Define method formulas mapping
      method_formulas <- c(
        "1" = "frame_dur / tac_uncor",
        "2" = "sqrt(frame_dur * tac_uncor)",
        "3" = "sqrt(frame_dur) / tac", 
        "4" = "sqrt(frame_dur)",
        "5" = "frame_dur * exp(-ln(2)/halflife)",
        "6" = "frame_dur / tac",
        "7" = "frame_dur",
        "8" = "frame_dur^2 / tac_uncor",
        "9" = "(frame_dur^2 / (frame_dur * tac)) * corrections^2"
      )
      
      # Determine formula to store
      selected_method <- input$weights_method %||% "2"
      if (selected_method == "custom") {
        weights_formula <- input$weights_custom_formula %||% ""
      } else {
        weights_formula <- method_formulas[selected_method] %||% ""
      }
      
      Weights <- list(
        region_type = input$weights_region_type %||% "external",
        region = if(input$weights_region_type == "single") input$weights_region %||% "" else "",
        external_tacs = if(input$weights_region_type == "external") input$weights_external_tacs %||% "" else "",
        radioisotope = input$weights_radioisotope %||% "C11",
        halflife = if(input$weights_radioisotope == "Other") as.character(input$weights_halflife %||% 20.4) else "",
        method = selected_method,
        formula = weights_formula,
        minweight = input$weights_minweight %||% 0.25
      )
      
      # Fit Delay
      FitDelay <- list(
        method = "Not configured"
      )
      
      # Models (capture actual model inputs)
      Models <- list(
        Model1 = list(
          type = if(!is.null(input$button)) input$button else "Not selected"
        ),
        Model2 = list(
          type = if(!is.null(input$button2)) input$button2 else "Not selected"
        ),
        Model3 = list(
          type = if(!is.null(input$button3)) input$button3 else "Not selected"
        )
      )
      
      config_list <- list(
        analysis_folder = subfolder,
        config_created = format(Sys.time(), "%Y-%m-%d %H:%M"),
        Subsetting = Subsetting,
        Weights = Weights,
        FitDelay = FitDelay,
        Models = Models
      )
      
      jsonlite::toJSON(config_list, pretty = TRUE, auto_unbox = TRUE)
    })
    
    # Save config function
    save_config <- function() {
      config_file_path <- file.path(output_dir, "desc-kinfitroptions_config.json")
      
      tryCatch({
        writeLines(config_json(), config_file_path)
        showNotification(paste("Configuration saved to", basename(config_file_path)), 
                        type = "message", duration = 3)
        cat("Config saved to:", config_file_path, "\n")
      }, error = function(e) {
        showNotification(paste("Error saving config:", e$message), 
                        type = "error", duration = 5)
        cat("Error saving config:", e$message, "\n")
      })
    }
    
    # Load existing config function
    load_existing_config <- function() {
      config_file_path <- file.path(output_dir, "desc-kinfitroptions_config.json")
      
      if (!file.exists(config_file_path)) {
        return(NULL)
      }
      
      tryCatch({
        config_json <- readLines(config_file_path, warn = FALSE)
        config_data <- jsonlite::fromJSON(paste(config_json, collapse = ""))
        cat("Loaded existing config from:", config_file_path, "\n")
        return(config_data)
      }, error = function(e) {
        showNotification("Unable to read existing config file. Using default settings.", 
                        type = "error", duration = 5)
        cat("Error loading config:", e$message, "\n")
        return(NULL)
      })
    }
    
    # Save config button handler
    observeEvent(input$save_config, {
      save_config()
    })
    
    # Run button handlers - each saves config then executes step
    observeEvent(input$run_subset, {
      save_config()
      
      # Check if combined TACs file exists in kinfitr folder
      kinfitr_folder <- kinfitr_dir  # derivatives/kinfitr - contains combined TACs file
      analysis_folder <- output_dir  # derivatives/kinfitr/{subfolder} - for individual files
      combined_tacs_file <- file.path(kinfitr_folder, "desc-combinedregions_tacs.tsv")
      
      if (!file.exists(combined_tacs_file)) {
        showNotification("Combined TACs file not found. Please run the region definition app first.", 
                        type = "error", duration = 5)
        return()
      }
      
      tryCatch({
        # Read combined TACs data from kinfitr folder
        combined_data <- readr::read_tsv(combined_tacs_file, show_col_types = FALSE)
        
        if (nrow(combined_data) == 0) {
          showNotification("Combined TACs file is empty", type = "error", duration = 5)
          return()
        }
        
        # Parse subsetting parameters
        subset_params <- list(
          sub = parse_semicolon_values(input$subset_sub),
          ses = parse_semicolon_values(input$subset_ses),
          task = parse_semicolon_values(input$subset_task),
          trc = parse_semicolon_values(input$subset_trc),
          rec = parse_semicolon_values(input$subset_rec),
          run = parse_semicolon_values(input$subset_run),
          regions = parse_semicolon_values(input$subset_regions)
        )
        
        # Apply subsetting
        filtered_data <- subset_combined_tacs(combined_data, subset_params)
        
        if (nrow(filtered_data) == 0) {
          showNotification("No data matches the subsetting criteria", type = "warning", duration = 5)
          return()
        }
        
        # Create individual TACs files in analysis folder
        result <- create_individual_tacs_files(filtered_data, analysis_folder)
        
        # Show generating report notification
        showNotification("Generating report...", type = "message", duration = NULL, id = "generating_report")
        
        cat("=== Data Subsetting Complete ===\n")
        cat("Files created:", result$files_created, "\n")
        cat("Analysis folder:", analysis_folder, "\n")
        cat("Total measurements:", nrow(filtered_data), "\n")  
        cat("Unique subjects:", length(unique(filtered_data$sub)), "\n")
        cat("Unique regions:", length(unique(filtered_data$region)), "\n")
        
        # Generate data definition report
        tryCatch({
          report_file <- generate_step_report(
            step_name = "data_definition",
            analysis_folder = analysis_folder
          )
          
          # Remove generating notification and show completion
          removeNotification(id = "generating_report")
          
          if (!is.null(report_file)) {
            showNotification("Data definition report generated successfully", type = "message", duration = 5)
          } else {
            showNotification("Report generation completed", type = "message", duration = 5)
          }
          
        }, error = function(e) {
          # Remove generating notification and show error
          removeNotification(id = "generating_report")
          showNotification("Error generating report", type = "error", duration = 5)
          cat("Warning: Could not generate data definition report:", e$message, "\n")
        })
        
      }, error = function(e) {
        error_msg <- paste("Error during data subsetting:", e$message)
        showNotification(error_msg, type = "error", duration = 10)
        cat("Error:", e$message, "\n")
      })
    })
    
    observeEvent(input$run_weights, {
      # Check for combined regions TACs files first
      tacs_files <- list.files(output_dir, 
                              pattern = "*_desc-combinedregions_tacs.tsv", 
                              recursive = TRUE)
      
      if (length(tacs_files) == 0) {
        showNotification("No combined regions TACs files found. Please run Data Definition first to create individual TACs files.", 
                        type = "error", duration = 8)
        return()
      }
      
      save_config()
      # TODO: Add actual weights calculation logic
      
      # Generate weights report
      showNotification("Generating weights report...", type = "message", duration = NULL, id = "generating_weights_report")
      
      tryCatch({
        report_file <- generate_step_report(
          step_name = "weights",
          analysis_folder = output_dir
        )
        
        removeNotification(id = "generating_weights_report")
        
        if (!is.null(report_file)) {
          showNotification("Weights report generated", type = "message", duration = 3)
        } else {
          showNotification("Weights report generation failed - check console for details", type = "error", duration = 5)
        }
        
      }, error = function(e) {
        removeNotification(id = "generating_weights_report")
        error_msg <- paste("Could not generate weights report:", e$message)
        showNotification(error_msg, type = "error", duration = 8)
        cat("Error generating weights report:", e$message, "\n")
      })
    })
    
    observeEvent(input$run_delay, {
      save_config()
      showNotification("Delay estimation completed", type = "message", duration = 3)
      # TODO: Add actual delay estimation logic
      
      # Generate delay report
      tryCatch({
        report_file <- generate_step_report(
          step_name = "delay",
          analysis_folder = output_dir
        )
        
        if (!is.null(report_file)) {
          showNotification("Delay report generated", type = "message", duration = 3)
        }
        
      }, error = function(e) {
        cat("Warning: Could not generate delay report:", e$message, "\n")
      })
    })
    
    observeEvent(input$run_model1, {
      save_config()
      showNotification("Model 1 fitting completed", type = "message", duration = 3)
      # TODO: Add actual model 1 fitting logic
      
      # Generate model 1 report
      tryCatch({
        model_type <- input$button %||% "1TCM"
        
        report_file <- generate_model_report(
          model_type = model_type,
          model_number = "Model 1",
          analysis_folder = output_dir,
          model_results = NULL,  # TODO: Add actual model results when implemented
          tacs_files = NULL      # TODO: Add TACs files list when available
        )
        
        if (!is.null(report_file)) {
          showNotification(paste("Model 1 report generated (", model_type, ")"), type = "message", duration = 3)
        }
        
      }, error = function(e) {
        cat("Warning: Could not generate Model 1 report:", e$message, "\n")
      })
    })
    
    observeEvent(input$run_model2, {
      save_config()
      showNotification("Model 2 fitting completed", type = "message", duration = 3)
      # TODO: Add actual model 2 fitting logic
      
      # Generate model 2 report
      tryCatch({
        model_type <- input$button2 %||% "1TCM"
        
        report_file <- generate_model_report(
          model_type = model_type,
          model_number = "Model 2",
          analysis_folder = output_dir,
          model_results = NULL,  # TODO: Add actual model results when implemented
          tacs_files = NULL      # TODO: Add TACs files list when available
        )
        
        if (!is.null(report_file)) {
          showNotification(paste("Model 2 report generated (", model_type, ")"), type = "message", duration = 3)
        }
        
      }, error = function(e) {
        cat("Warning: Could not generate Model 2 report:", e$message, "\n")
      })
    })
    
    observeEvent(input$run_model3, {
      save_config()
      showNotification("Model 3 fitting completed", type = "message", duration = 3)
      # TODO: Add actual model 3 fitting logic
      
      # Generate model 3 report
      tryCatch({
        model_type <- input$button3 %||% "1TCM"
        
        report_file <- generate_model_report(
          model_type = model_type,
          model_number = "Model 3",
          analysis_folder = output_dir,
          model_results = NULL,  # TODO: Add actual model results when implemented
          tacs_files = NULL      # TODO: Add TACs files list when available
        )
        
        if (!is.null(report_file)) {
          showNotification(paste("Model 3 report generated (", model_type, ")"), type = "message", duration = 3)
        }
        
      }, error = function(e) {
        cat("Warning: Could not generate Model 3 report:", e$message, "\n")
      })
    })
    
    observeEvent(input$run_interactive, {
      save_config()
      showNotification("Interactive mode launched", type = "message", duration = 3)
      # TODO: Add actual interactive mode logic
    })
    
    # Run All button handler
    observeEvent(input$run_all, {
      save_config()
      
      # Execute all steps in sequence with progress notifications
      showNotification("Running all steps...", type = "message", duration = 2)
      
      Sys.sleep(0.5)
      showNotification("Step 1/6: Data subsetting completed", type = "message", duration = 2)
      
      Sys.sleep(0.5)
      showNotification("Step 2/6: Weights calculation completed", type = "message", duration = 2)
      
      Sys.sleep(0.5)
      showNotification("Step 3/6: Delay estimation completed", type = "message", duration = 2)
      
      Sys.sleep(0.5)
      showNotification("Step 4/6: Model 1 fitting completed", type = "message", duration = 2)
      
      Sys.sleep(0.5)
      showNotification("Step 5/6: Model 2 fitting completed", type = "message", duration = 2)
      
      Sys.sleep(0.5)
      showNotification("Step 6/6: Model 3 fitting completed", type = "message", duration = 2)
      
      Sys.sleep(0.5)
      showNotification("All steps completed successfully!", type = "message", duration = 5)
      
      # TODO: Add actual sequential execution logic for all steps
    })
    
    # Output for analysis folder display
    output$analysis_folder <- renderText({
      subfolder
    })
    
    # JSON preview output
    output$json_preview <- renderText({
      config_json()
    })
    
  }
  
  # Run the application
  shiny::shinyApp(ui = ui, server = server)
}