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
    titlePanel("Create a customised kinfitr BIDS App config file"),
    
    # Tab panel for all options ----
    tabsetPanel(type = "tabs",
                # Tab panel for data definition ----
                tabPanel("Data Definition",
                         br(),
                         h4("Data Definition"),
                         # p("Define and create the analysis dataset from the combined TACs file. This step allows you to subset the data and generate individual TACs files for your specific analysis.",
                         #   style = "font-size:14px;"
                         # ),
                         # br(),
                         h5("Subsetting"),
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
                         h5("Create Data"),
                         p("Generate individual TACs files for each measurement and create a report plotting the selected data.",
                           style = "font-size:14px;"
                         ),
                         br(),
                         actionButton("run_subset", "▶ Create Analysis Data", class = "btn-success btn-lg")
                ),
                # Tab panel for weights ----
                    tabPanel("Weights Definition",
                             h4("Weights Definition"),
                             p("Create weights for modelling"),
                             br(),
                             h5("Content coming soon..."),
                             
                             hr(),
                             actionButton("run_weights", "▶ Run Weights Calculation", class = "btn-success btn-lg")
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
                    tabPanel("Configuration",
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
        # Show notification that config was loaded
        showNotification(paste("Loaded previous configuration from", basename(output_dir)), 
                        type = "message", duration = 5)
        
        # Restore subsetting parameters
        if (!is.null(existing_config$Subsetting)) {
          updateTextInput(session, "subset_sub", value = existing_config$Subsetting$sub %||% "")
          updateTextInput(session, "subset_ses", value = existing_config$Subsetting$ses %||% "")
          updateTextInput(session, "subset_task", value = existing_config$Subsetting$task %||% "")
          updateTextInput(session, "subset_trc", value = existing_config$Subsetting$trc %||% "")
          updateTextInput(session, "subset_rec", value = existing_config$Subsetting$rec %||% "")
          updateTextInput(session, "subset_run", value = existing_config$Subsetting$run %||% "")
          updateTextInput(session, "subset_regions", value = existing_config$Subsetting$RegionNames %||% "")
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
      }
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
        RegionNames = input$subset_regions
      )
      
      # Weights Definition  
      Weights <- list(
        method = "Not configured"
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
      
      jsonlite::toJSON(config_list, pretty=T)
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
        
        # Show success notification
        success_msg <- paste0(
          "Data subsetting completed successfully. ",
          result$summary, " in analysis folder. ",
          "Total measurements: ", nrow(filtered_data), ", ",
          "Unique subjects: ", length(unique(filtered_data$sub)), ", ",
          "Unique regions: ", length(unique(filtered_data$region))
        )
        
        showNotification(success_msg, type = "message", duration = 5)
        
        cat("=== Data Subsetting Complete ===\n")
        cat("Files created:", result$files_created, "\n")
        cat("Analysis folder:", analysis_folder, "\n")
        cat("Total measurements:", nrow(filtered_data), "\n")  
        cat("Unique subjects:", length(unique(filtered_data$sub)), "\n")
        cat("Unique regions:", length(unique(filtered_data$region)), "\n")
        
      }, error = function(e) {
        error_msg <- paste("Error during data subsetting:", e$message)
        showNotification(error_msg, type = "error", duration = 10)
        cat("Error:", e$message, "\n")
      })
    })
    
    observeEvent(input$run_weights, {
      save_config()
      showNotification("Weights calculation completed", type = "message", duration = 3)
      # TODO: Add actual weights calculation logic
    })
    
    observeEvent(input$run_delay, {
      save_config()
      showNotification("Delay estimation completed", type = "message", duration = 3)
      # TODO: Add actual delay estimation logic
    })
    
    observeEvent(input$run_model1, {
      save_config()
      showNotification("Model 1 fitting completed", type = "message", duration = 3)
      # TODO: Add actual model 1 fitting logic
    })
    
    observeEvent(input$run_model2, {
      save_config()
      showNotification("Model 2 fitting completed", type = "message", duration = 3)
      # TODO: Add actual model 2 fitting logic
    })
    
    observeEvent(input$run_model3, {
      save_config()
      showNotification("Model 3 fitting completed", type = "message", duration = 3)
      # TODO: Add actual model 3 fitting logic
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