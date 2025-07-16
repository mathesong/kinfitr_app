#' Run Kinfitr App
#'
#' @description Launch the Kinfitr BIDS App configuration interface
#'
#' @param bids_dir Character string path to the BIDS directory (default: current directory)
#' @param derivatives_dir Character string path to the derivatives folder (default: bids_dir/derivatives)
#' @param subfolder Character string name for analysis subfolder (default: "Primary_Analysis")
#' @param config_file Character string path to existing config file (optional)
#' @export
run_app <- function(bids_dir = ".", derivatives_dir = NULL, subfolder = "Primary_Analysis", config_file = NULL) {
  
  # Set default derivatives_dir if not provided
  if (is.null(derivatives_dir)) {
    derivatives_dir <- file.path(bids_dir, "derivatives")
  }
  
  # Normalize paths to avoid double slashes
  bids_dir <- normalizePath(bids_dir, mustWork = FALSE)
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
  
  # Create kinfitr dir
  kinfitr_dir <- file.path(bids_dir, "derivatives")
  kinfitr_dir <- normalizePath(kinfitr_dir, mustWork = FALSE)
  
  if (!dir.exists(kinfitr_dir)) {
    dir.create(kinfitr_dir, recursive = TRUE)
    cat("Created kinfitr directory:", kinfitr_dir, "\n")
  }
  
  # Validate config file if provided
  if (!is.null(config_file) && !file.exists(config_file)) {
    stop(paste("Config file does not exist:", config_file), call. = FALSE)
  }
  
  # Print configuration
  cat("Starting kinfitr BIDS App with configuration:\n")
  cat("  BIDS directory:", bids_dir, "\n")
  cat("  Derivatives directory:", derivatives_dir, "\n")
  cat("  kinfitr subfolder:", subfolder, "\n")
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
  
  # Try to create TACs list, with error handling
  tacs_list <- tryCatch({
    create_tacs_list(kinfitr_dir)
  }, error = function(e) {
    cat("Warning: Could not create TACs list:", e$message, "\n")
    cat("Creating empty TACs list for now.\n")
    tibble::tibble(tacs_filedescription = character(0))
  })
  
  ui <- fluidPage(theme = shinythemes::shinytheme("flatly"),
    
    # App title ----
    titlePanel("Create a customised kinfitr BIDS App config file"),
    
    # Sidebar layout for subsetting ----
    sidebarLayout(
      
      # Sidebar panel for inputs ----
      sidebarPanel(
        
        h2("Data Subset"),
        p(glue::glue("Use these options to apply this config to a subset of the data. ",
               "Values should be separated by semi-colons. ",
               "All measurements fulfilling all the conditions will ",
               "be included. Leave options blank for no subsetting is desired, ",
               "i.e. leaving sub blank implies that all subjects should ",
               "be included."),
          style = "font-size:14px;"
        ),
        textInput(inputId = "subset_sub", label = "sub", value = ""),
        textInput(inputId = "subset_ses", label = "ses", value = ""),
        textInput(inputId = "subset_rec", label = "rec", value = ""),
        textInput(inputId = "subset_tracer", label = "Tracer Name", value = ""),
        textInput(inputId = "subset_tracer", label = "Region Names (separated by ;)", value = ""),
        
      ),
      
      # Main panel for modelling options ----
      mainPanel(
        
        h2("Modelling Selections"),
        p(glue::glue("Select the modelling approach to fit the Time Activity Curves",
               " (TACs). You can select among different methods, either ",
               "compartmental models (e.g., 1TCM, 2TCM), and graphical models",
               "(e.g., Logan) to fit your data. Data to be fitted are exepcted to",
               " be derived from a  "),
          style = "font-size:14px;"
        ),
        br(),
        
        # Tab panel for region config ----
        tabsetPanel(type = "tabs",
                    tabPanel("Region Configuration",
                             h4("Region Configuration"),
                             p("Define brain regions and segmentation parameters"),
                             br(),
                             selectInput("region_selection", 
                                        "Select Region Configuration:",
                                        choices = tacs_list$tacs_filedescription,
                                        selected = NULL)
                    ),
                    # Tab panel for weights ----
                    tabPanel("Weights Definition",
                             h4("Weights Definition"),
                             p("Create weights for modelling"),
                             br(),
                             h5("Content coming soon...")
                    ),
                    # Tab panel for delay ----
                    tabPanel("Fit Delay",
                             h4("Fit Delay"),
                             p("Estimate the delay between the blood and tissue curves"),
                             br(),
                             h5("Content coming soon...")
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
                             )
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
                             )
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
                             )
                    ),
                    tabPanel("Interactive",
                             h4("Interactive Modeling"),
                             p("Fit models interactively with reactive figures and parameters"),
                             br(),
                             h5("Content coming soon...")
                    ),
                    tabPanel("Download",)
        )
      )
    )
  )
  
  # Define server logic for config file creation ----
  server <- function(input, output) {
    
    # Reactive expression to generate the config file ----
    config_json <- reactive({
      
      Subsets <- list(
        sub = input$subset_sub,
        ses = input$subset_ses,
        rec = input$subset_rec,
        task = input$subset_task,
        run = input$subset_run,
        TracerName = input$subset_tracer,
        ModeOfAdministration = input$subset_modeadmin,
        InstitutionName = input$subset_institute,
        PharmaceuticalName = input$subset_pharmaceutical
      )
      
      ParentFraction <- list(
        Method = input$pf_model,
        set_ppf0 = input$pf_set_t0,
        starttime = as.numeric(input$pf_starttime),
        endtime  = as.numeric(input$pf_endtime),
        gam_k = input$pf_k,
        hgam_formula = input$pf_hgam_opt
      )
      
      BPR <- list(
        Method = input$bpr_model,
        starttime = as.numeric(input$bpr_starttime),
        endtime  = as.numeric(input$bpr_endtime),
        gam_k = as.numeric(input$bpr_k),
        hgam_formula = input$bpr_hgam_opt
      )
      
      AIF <- list(
        Method = input$aif_model,
        starttime = as.numeric(input$aif_starttime),
        endtime  = as.numeric(input$aif_endtime),
        expdecay_props = as.numeric(c(input$aif_expdecay_1,
                                      input$aif_expdecay_2)),
        inftime = as.numeric(stringr::str_split(input$aif_inftime, pattern = ";")[[1]]),
        spline_kb = input$aif_kb,
        spline_ka_m = input$aif_ka_m,
        spline_ka_a = input$aif_ka_a
      )
      
      WholeBlood <- list(
        Method = input$wb_model,
        dispcor = input$wb_dispcor,
        starttime = as.numeric(input$wb_starttime),
        endtime  = as.numeric(input$wb_endtime),
        spline_kb = input$wb_kb,
        spline_ka_m = input$wb_ka_m,
        spline_ka_a = input$wb_ka_a
      )
      
      config_list <- list(
        Subsets = Subsets,
        Model = list(
          ParentFraction = ParentFraction,
          BPR = BPR,
          AIF = AIF,
          WholeBlood = WholeBlood
        )
      )
      
      jsonlite::toJSON(config_list, pretty=T)
    })
    
    output$downloadData <- downloadHandler(
      filename = function() {
        generate_new_filename()
      },
      content = function(con) {
        writeLines(text = config_json(),
                   con = con)
      }
    )
    
    output$json_text <- renderText( { config_json() } )
    
  }
  
  # Run the application
  shiny::shinyApp(ui = ui, server = server)
}