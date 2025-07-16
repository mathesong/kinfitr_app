#' Find Folders Containing TACs Files
#'
#' @description Identifies directories that contain *_tacs.tsv files
#'
#' @param derivatives_folder Character string path to the derivatives folder
#' @return Character vector of directory paths containing *_tacs.tsv files
#' @export
find_tacs_folders <- function(derivatives_folder) {
  
  # Find all subdirectories in the derivatives folder
  subdirs <- list.dirs(derivatives_folder, recursive = FALSE, full.names = TRUE)
  
  # Function to check if a directory contains *_tacs.tsv files
  has_tacs_files <- function(dir_path) {
    tacs_files <- list.files(dir_path, pattern = "*_tacs\\.tsv$", recursive = TRUE)
    return(length(tacs_files) > 0)
  }
  
  # Filter directories that contain *_tacs.tsv files
  valid_dirs <- subdirs[purrr::map_lgl(subdirs, has_tacs_files)]
  
  if (length(valid_dirs) == 0) {
    stop("No directories with *_tacs.tsv files found in ", derivatives_folder)
  }
  
  return(valid_dirs)
}

#' Summarise TACs File Descriptions
#'
#' @description Process and summarize TACs files descriptions
#'
#' @param dir_path Character string path to directory containing *_tacs.tsv files
#' @return Data frame with region configurations from the directory
#' @export
summarise_tacs_descriptions <- function(dir_path) {
  
  # Get all *_tacs.tsv files in this directory
  tacs_files <- list.files(dir_path, pattern = "*_tacs\\.tsv$", 
                          recursive = TRUE, full.names = TRUE)
  
  if (length(tacs_files) == 0) {
    return(NULL)
  }

  parsed_files <- kinfitr::bids_parse_files(dir_path)
  
  # Unnest the filedata
  unnested_tacfiledata <- parsed_files %>%
    dplyr::select(filedata) %>% 
    tidyr::unnest(filedata) %>% 
    dplyr::filter(measurement=="tacs") %>% 
    dplyr::select(-path_absolute, -path, -extension,
                  -measurement) %>% 
    dplyr::distinct()
  
  create_bids_key_value_pairs(unnested_tacfiledata,
                              colnames(unnested_tacfiledata))
  
}

create_tacs_list <- function(derivatives_folder) {
  
  tacs_folders <- tibble::tibble(
    path = find_tacs_folders(derivatives_folder)) %>% 
    dplyr::mutate(foldername = basename(path)) %>% 
    dplyr::mutate(descriptions = purrr::map(path, summarise_tacs_descriptions)) %>% 
    tidyr::unnest(descriptions) %>% 
    dplyr::mutate(tacs_filedescription = paste0(foldername, ": ", description))
  
  return(tacs_folders)
}


#' Create Kinfitr Regions Configuration
#'
#' @description Function to scan derivatives folders for *_tacs.tsv files and 
#' generate region configuration file
#'
#' @param derivatives_folder Character string path to the derivatives folder
#' @return Data frame with region configurations
#' @export
create_kinfitr_regions <- function(derivatives_folder) {
  
  # Find folders containing TACs files
  valid_dirs <- find_tacs_folders(derivatives_folder)
  
  # Process all valid directories
  all_regions <- purrr::map_dfr(valid_dirs, summarise_tacs_files)
  
  # Remove any duplicate combinations
  unique_regions <- all_regions %>%
    dplyr::distinct(region_name, derivatives_folder, description, name)
  
  # Write to kinfitr_regions.tsv
  output_file <- file.path(derivatives_folder, "kinfitr_regions.tsv")
  utils::write.table(unique_regions, output_file, sep = "\t", row.names = FALSE, quote = FALSE)
  
  cat("Created kinfitr_regions.tsv with", nrow(unique_regions), "region configurations\n")
  cat("Output file:", output_file, "\n")
  
  return(unique_regions)
}

create_bids_key_value_pairs <- function(data, columns) {
  data %>%
    dplyr::mutate(
      key_value_pairs = apply(
        data[columns], 1,
        function(row) {
          # Create key-value pairs only for non-NA values
          pairs <- paste(columns, row, sep = "-")
          non_na_pairs <- pairs[!is.na(row)]
          paste(non_na_pairs, collapse = "_")
        }
      )
    ) %>% 
    dplyr::select(description=key_value_pairs)
}

#' Interpret BIDS Key-Value Pairs
#'
#' @description Parse key-value pair strings back into tibble columns
#'
#' @param key_value_strings Character vector of key-value pair strings
#' @return Tibble with parsed columns
#' @export
interpret_bids_key_value_pairs <- function(key_value_strings) {
  
  if (length(key_value_strings) == 0) {
    return(tibble::tibble())
  }
  
  # Parse each key-value string
  parsed_list <- purrr::map(key_value_strings, function(kv_string) {
    
    if (is.na(kv_string) || kv_string == "") {
      return(list())
    }
    
    # Split by underscore to get individual pairs
    pairs <- stringr::str_split(kv_string, "_")[[1]]
    
    # Parse each pair (format: "key-value")
    result <- list()
    for (pair in pairs) {
      if (stringr::str_detect(pair, "-")) {
        parts <- stringr::str_split(pair, "-", n = 2)[[1]]
        if (length(parts) == 2) {
          key <- parts[1]
          value <- parts[2]
          result[[key]] <- value
        }
      }
    }
    
    return(result)
  })
  
  # Get all unique column names
  all_columns <- unique(unlist(purrr::map(parsed_list, names)))
  
  if (length(all_columns) == 0) {
    return(tibble::tibble())
  }
  
  # Create tibble with all columns
  result_data <- purrr::map_dfc(all_columns, function(col) {
    values <- purrr::map_chr(parsed_list, function(row) {
      if (col %in% names(row)) {
        return(row[[col]])
      } else {
        return(NA_character_)
      }
    })
    
    # Create a named list for this column
    setNames(list(values), col)
  })
  
  return(result_data)
}

#' Region Definition App
#'
#' @description Launch a separate Shiny app for defining brain regions
#'
#' @param bids_dir Character string path to the BIDS directory
#' @param output_dir Character string path for output (default: bids_dir/code/kinfitr)
#' @export
region_definition_app <- function(bids_dir = ".", output_dir = NULL) {
  
  # Set default output directory
  if (is.null(output_dir)) {
    output_dir <- file.path(bids_dir, "code", "kinfitr")
  }
  
  # Validate BIDS directory
  if (!dir.exists(bids_dir)) {
    stop(paste("BIDS directory does not exist:", bids_dir), call. = FALSE)
  }
  
  # Create output directory if it doesn't exist
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
    cat("Created output directory:", output_dir, "\n")
  }
  
  # Set derivatives directory (same logic as main app)
  derivatives_dir <- file.path(bids_dir, "derivatives")
  
  # Normalize paths
  bids_dir <- normalizePath(bids_dir, mustWork = FALSE)
  derivatives_dir <- normalizePath(derivatives_dir, mustWork = FALSE)
  output_dir <- normalizePath(output_dir, mustWork = FALSE)
  
  # Create derivatives directory if it doesn't exist
  if (!dir.exists(derivatives_dir)) {
    dir.create(derivatives_dir, recursive = TRUE)
    cat("Created derivatives directory:", derivatives_dir, "\n")
  }
  
  # Print configuration
  cat("Starting Region Definition App with configuration:\n")
  cat("  BIDS directory:", bids_dir, "\n")
  cat("  Derivatives directory:", derivatives_dir, "\n")
  cat("  Output directory:", output_dir, "\n")
  
  # Initialize kinfitr_regions.tsv file
  regions_file <- file.path(output_dir, "kinfitr_regions.tsv")
  file_was_empty <- FALSE
  
  if (!file.exists(regions_file)) {
    # Create empty regions file with proper columns
    empty_regions <- tibble::tibble(
      RegionName = character(0),
      folder = character(0),
      description = character(0),
      ConstituentRegion = character(0)
    )
    utils::write.table(empty_regions, regions_file, sep = "\t", 
                      row.names = FALSE, quote = FALSE)
    file_was_empty <- TRUE
    cat("Created empty kinfitr_regions.tsv file:", regions_file, "\n")
  } else {
    # Check if existing file is empty
    existing_data <- utils::read.table(regions_file, sep = "\t", header = TRUE, stringsAsFactors = FALSE)
    file_was_empty <- nrow(existing_data) == 0
    cat("Found existing kinfitr_regions.tsv file:", regions_file, "\n")
  }
  
  # Try to create TACs list, with error handling
  tacs_list <- tryCatch({
    create_tacs_list(derivatives_dir)
  }, error = function(e) {
    cat("Warning: Could not create TACs list:", e$message, "\n")
    cat("Creating empty TACs list for now.\n")
    tibble::tibble(
      tacs_filedescription = character(0),
      path = character(0),
      foldername = character(0),
      description = character(0)
    )
  })
  
  # UI for region definition app
  ui <- fluidPage(
    theme = shinythemes::shinytheme("flatly"),
    
    titlePanel("Region Definition"),
    
    sidebarLayout(
      sidebarPanel(
        h3("TACs File Selection"),
        p("Select from available TACs files to define regions"),
        
        conditionalPanel(
          condition = "output.has_tacs_files",
          selectInput("selected_tacs", 
                     "Available TACs Files:",
                     choices = NULL,
                     selected = NULL),
          br(),
          actionButton("load_tacs", "Load Selected Regions", class = "btn-primary"),
          br(), br()
        ),
        
        conditionalPanel(
          condition = "!output.has_tacs_files",
          div(class = "alert alert-warning",
              h4("No TACs Files Found"),
              p("No TACs files were found in the derivatives directory:", 
                derivatives_dir),
              p("Please ensure TACs files are present and try again.")
          )
        ),
        
        hr(),
        
        h3("Region Definition"),
        textInput("region_name", "Region Name:", value = ""),
        uiOutput("region_name_error"),
        
        br(),
        actionButton("add_region", "Add Region", 
                    class = "btn-success"),
        br(), br(),
        actionButton("add_all_regions", "Add All Regions", 
                    class = "btn-info"),
        br(), br(),
        actionButton("remove_region", "Remove Region", 
                    class = "btn-danger"),
        br(), br(), br(), br(),
        actionButton("remove_all_regions", "Remove All Regions", 
                    class = "btn-danger"),
        
        hr(),
        actionButton("close_app", "Close App", 
                    class = "btn-secondary"),
        
        conditionalPanel(
          condition = "input.load_tacs > 0",
          hr(),
          h4("TACs File Details"),
          verbatimTextOutput("tacs_info_sidebar")
        )
      ),
      
      mainPanel(
        tabsetPanel(
          tabPanel("Available Regions",
                   br(),
                   conditionalPanel(
                     condition = "output.has_morph_data",
                     h4("Available Regions"),
                     p("Select regions with non-zero volume:"),
                     uiOutput("region_checkboxes")
                   ),
                   conditionalPanel(
                     condition = "!output.has_morph_data",
                     div(class = "alert alert-info",
                         h4("No Regions Available"),
                         p("Select TACs file (left) to see available regions.")
                     )
                   )
          ),
          tabPanel("Defined Regions",
                   br(),
                   h4("Currently Defined Regions"),
                   p("Regions saved to kinfitr_regions.tsv:"),
                   tableOutput("defined_regions_table")
          )
        )
      )
    )
  )
  
  # Server logic for region definition app
  server <- function(input, output, session) {
    
    # Reactive value to store defined regions
    defined_regions_data <- reactiveVal(NULL)
    
    # Track if file was originally empty
    originally_empty <- reactiveVal(file_was_empty)
    
    # Load defined regions on startup
    observe({
      regions_data <- tryCatch({
        utils::read.table(regions_file, sep = "\t", header = TRUE, stringsAsFactors = FALSE)
      }, error = function(e) {
        tibble::tibble(
          RegionName = character(0),
          folder = character(0),
          description = character(0),
          ConstituentRegion = character(0)
        )
      })
      defined_regions_data(regions_data)
    })
    
    # Clean up empty file on session end
    session$onSessionEnded(function() {
      # Read file directly instead of using reactive
      current_data <- tryCatch({
        utils::read.table(regions_file, sep = "\t", header = TRUE, stringsAsFactors = FALSE)
      }, error = function(e) {
        NULL
      })
      
      if (is.null(current_data) || nrow(current_data) == 0) {
        if (file.exists(regions_file)) {
          file.remove(regions_file)
          cat("Removed empty kinfitr_regions.tsv file on app close\n")
        }
      }
    })
    
    # Check if we have TACs files
    output$has_tacs_files <- reactive({
      nrow(tacs_list) > 0
    })
    outputOptions(output, "has_tacs_files", suspendWhenHidden = FALSE)
    
    # Check if we have morph data
    output$has_morph_data <- reactive({
      !is.null(morph_data()) && nrow(morph_data()) > 0
    })
    outputOptions(output, "has_morph_data", suspendWhenHidden = FALSE)
    
    # Generate checkboxes for regions
    output$region_checkboxes <- renderUI({
      morph_df <- morph_data()
      if (is.null(morph_df) || nrow(morph_df) == 0) {
        return(NULL)
      }
      
      # Create checkboxes for each region
      checkbox_list <- lapply(1:nrow(morph_df), function(i) {
        region_name <- morph_df$name[i]
        checkboxInput(
          inputId = paste0("region_", i),
          label = region_name,
          value = FALSE
        )
      })
      
      do.call(tagList, checkbox_list)
    })
    
    # Update TACs file choices
    observe({
      if (nrow(tacs_list) > 0) {
        updateSelectInput(session, "selected_tacs",
                         choices = setNames(tacs_list$tacs_filedescription,
                                          tacs_list$tacs_filedescription))
      }
    })
    
    # Reactive to store loaded TACs data
    loaded_tacs_data <- reactiveVal(NULL)
    
    # Reactive to store morph data
    morph_data <- reactiveVal(NULL)
    
    # Load selected TACs file
    observeEvent(input$load_tacs, {
      req(input$selected_tacs)
      
      # Find the selected TACs file info
      selected_info <- tacs_list[tacs_list$tacs_filedescription == input$selected_tacs, ]
      
      if (nrow(selected_info) > 0) {
        # Parse the key-value pairs back to get file details
        parsed_details <- interpret_bids_key_value_pairs(selected_info$description)
        
        # Create tibble from tacs_list and join to get actual file paths
        selected_tibble <- tibble::tibble(tacs_filedescription = input$selected_tacs)
        joined_data <- dplyr::inner_join(selected_tibble, tacs_list, by = "tacs_filedescription")
        
        if (nrow(joined_data) > 0) {
          # Get the first path and look for corresponding _morph.tsv file
          first_path <- joined_data$path[1]
          
          # Extract the description from the selected TACs option to match the right file
          tacs_parts <- stringr::str_split(input$selected_tacs, ": ", n = 2)[[1]]
          description_part <- if(length(tacs_parts) > 1) tacs_parts[2] else ""
          
          # Find _tacs.tsv files in this directory that match the description
          tacs_files <- list.files(first_path, pattern = "*_tacs\\.tsv$", 
                                  recursive = TRUE, full.names = TRUE)
          
          # Filter to find the file that matches the selected description
          matching_tacs_file <- NULL
          for (tacs_file in tacs_files) {
            if (stringr::str_detect(tacs_file, description_part)) {
              matching_tacs_file <- tacs_file
              break
            }
          }
          
          if (!is.null(matching_tacs_file)) {
            # Convert the matching _tacs.tsv file to _morph.tsv
            morph_file <- stringr::str_replace(matching_tacs_file, "_tacs\\.tsv$", "_morph.tsv")
            
            # Debug output to track which files are being used
            cat("Selected TACs description:", description_part, "\n")
            cat("Matching TACs file:", matching_tacs_file, "\n")
            cat("Corresponding morph file:", morph_file, "\n")
            
            # Try to read the morph file
            if (file.exists(morph_file)) {
              tryCatch({
                morph_df <- utils::read.table(morph_file, sep = "\t", header = TRUE, stringsAsFactors = FALSE)
                
                # Filter for non-zero volume-mm3 values
                if ("volume.mm3" %in% colnames(morph_df) && "name" %in% colnames(morph_df)) {
                  filtered_morph <- morph_df %>%
                    dplyr::filter(volume.mm3 != 0) %>%
                    dplyr::select(name) %>%
                    dplyr::arrange(name)
                  
                  morph_data(filtered_morph)
                  cat("Loaded", nrow(filtered_morph), "regions with non-zero volume from:", morph_file, "\n")
                } else {
                  cat("Warning: Expected columns 'name' and 'volume.mm3' not found in morph file\n")
                  morph_data(NULL)
                }
              }, error = function(e) {
                cat("Error reading morph file:", e$message, "\n")
                morph_data(NULL)
              })
            } else {
              cat("Morph file not found:", morph_file, "\n")
              morph_data(NULL)
            }
          } else {
            cat("No matching TACs file found for description:", description_part, "\n")
            morph_data(NULL)
          }
        }
        
        loaded_tacs_data(list(
          info = selected_info,
          details = parsed_details
        ))
      }
    })
    
    # Display TACs file information (only when TACs loaded)
    output$tacs_info <- renderText({
      req(input$load_tacs > 0)  # Only render when TACs loaded
      
      data <- loaded_tacs_data()
      if (is.null(data)) return("No file loaded")
      
      # info_text <- paste(
      #   "Selected File:", data$info$tacs_filedescription,
      #   "\nFolder:", data$info$foldername,
      #   "\nPath:", data$info$path,
      #   "\nDescription:", data$info$description,
      #   sep = ""
      # )
      
      return(info_text)
    })
    
    # Display TACs file information in sidebar
    output$tacs_info_sidebar <- renderText({
      data <- loaded_tacs_data()
      if (is.null(data)) return("No file loaded")
      
      info_text <- paste(
        "File:", data$info$tacs_filedescription,
        "\nFolder:", data$info$foldername,
        sep = ""
      )
      
      return(info_text)
    })
    
    # Display parsed data (only when TACs loaded)
    output$parsed_data <- DT::renderDataTable({
      req(input$load_tacs > 0)  # Only render when TACs loaded
      
      data <- loaded_tacs_data()
      if (is.null(data) || is.null(data$details)) {
        return(data.frame(Message = "No data to display"))
      }
      
      DT::datatable(data$details, options = list(scrollX = TRUE))
    })
    
    # Display available columns
    output$available_columns <- renderText({
      req(input$load_tacs > 0)  # Only render when TACs loaded
      
      data <- loaded_tacs_data()
      if (is.null(data) || is.null(data$details)) return("No columns available")
      
      cols <- colnames(data$details)
      paste("Available columns:\n", paste(cols, collapse = ", "))
    })
    
    # Display defined regions in a simple table
    output$defined_regions_table <- renderTable({
      regions_data <- defined_regions_data()
      if (is.null(regions_data) || nrow(regions_data) == 0) {
        return(data.frame(Message = "No regions defined yet."))
      }
      
      # Sort by RegionName, then description, then ConstituentRegion
      sorted_data <- regions_data %>%
        dplyr::arrange(RegionName, description, ConstituentRegion) %>%
        dplyr::mutate(RegionName = paste0("<b>", RegionName, "</b>"))
      
      return(sorted_data)
    }, sanitize.text.function = function(x) x)
    
    # Show error message for duplicate names or missing names for combined regions
    duplicate_error <- reactiveVal(FALSE)
    multiple_regions_error <- reactiveVal(FALSE)
    
    output$region_name_error <- renderUI({
      if (duplicate_error() && !multiple_regions_error()) {
        div(style = "color: red; font-size: 12px; margin-top: 5px;",
            "⚠ This region name already exists!")
      } else if (multiple_regions_error()) {
        div(style = "color: red; font-size: 12px; margin-top: 5px;",
            "⚠ Please choose a new name for combined regions")
      } else {
        NULL
      }
    })
    
    # Add region
    observeEvent(input$add_region, {
      # Get selected checkboxes first to check if only one is selected
      morph_df <- morph_data()
      if (is.null(morph_df) || nrow(morph_df) == 0) {
        showNotification("No regions available to add", type = "warning")
        return()
      }
      
      selected_regions <- c()
      for (i in 1:nrow(morph_df)) {
        checkbox_id <- paste0("region_", i)
        if (!is.null(input[[checkbox_id]]) && input[[checkbox_id]]) {
          selected_regions <- c(selected_regions, morph_df$name[i])
        }
      }
      
      if (length(selected_regions) == 0) {
        showNotification("Please select at least one region", type = "warning")
        return()
      }
      
      # Determine region name: use input if provided, otherwise use original name for single selection
      region_name_to_use <- if (input$region_name == "" && length(selected_regions) == 1) {
        selected_regions[1]
      } else if (input$region_name == "") {
        # Show red error text for multiple regions without name
        multiple_regions_error(TRUE)
        duplicate_error(FALSE)
        return()
      } else {
        input$region_name
      }
      
      # Check for duplicate region names
      current_data <- defined_regions_data()
      if (!is.null(current_data) && nrow(current_data) > 0 && region_name_to_use %in% current_data$RegionName) {
        duplicate_error(TRUE)
        multiple_regions_error(FALSE)
        return()
      }
      
      # Clear any previous errors
      duplicate_error(FALSE)
      multiple_regions_error(FALSE)
      
      if (is.null(input$selected_tacs) || input$selected_tacs == "") {
        showNotification("Please select a TACs file first", type = "warning")
        return()
      }
      
      # Parse the selected TACs to extract folder and description
      tacs_parts <- stringr::str_split(input$selected_tacs, ": ", n = 2)[[1]]
      folder_name <- tacs_parts[1]
      description_part <- if(length(tacs_parts) > 1) tacs_parts[2] else ""
      
      # Create new rows for the TSV
      new_rows <- tibble::tibble(
        RegionName = rep(region_name_to_use, length(selected_regions)),
        folder = rep(folder_name, length(selected_regions)),
        description = rep(description_part, length(selected_regions)),
        ConstituentRegion = selected_regions
      )
      
      # Add to existing data
      if (is.null(current_data) || nrow(current_data) == 0) {
        updated_data <- new_rows
      } else {
        updated_data <- dplyr::bind_rows(current_data, new_rows)
      }
      
      # Save to file
      utils::write.table(updated_data, regions_file, sep = "\t", 
                        row.names = FALSE, quote = FALSE)
      
      # Update reactive value - this triggers reactive updates
      defined_regions_data(updated_data)
      
      # Mark that file is no longer empty
      originally_empty(FALSE)
      
      # Clear region name input and any errors
      updateTextInput(session, "region_name", value = "")
      duplicate_error(FALSE)
      multiple_regions_error(FALSE)
      
      # Clear checkboxes
      for (i in 1:nrow(morph_df)) {
        checkbox_id <- paste0("region_", i)
        updateCheckboxInput(session, checkbox_id, value = FALSE)
      }
      
      showNotification(paste("Added", length(selected_regions), "constituent regions for:", region_name_to_use), type = "message")
    })
    
    # Add all regions
    observeEvent(input$add_all_regions, {
      if (is.null(input$selected_tacs) || input$selected_tacs == "") {
        showNotification("Please select a TACs file first", type = "warning")
        return()
      }
      
      morph_df <- morph_data()
      if (is.null(morph_df) || nrow(morph_df) == 0) {
        showNotification("No regions available to add", type = "warning")
        return()
      }
      
      # Parse the selected TACs to extract folder and description
      tacs_parts <- stringr::str_split(input$selected_tacs, ": ", n = 2)[[1]]
      folder_name <- tacs_parts[1]
      description_part <- if(length(tacs_parts) > 1) tacs_parts[2] else ""
      
      # Create rows for all regions using their original names
      new_rows <- tibble::tibble(
        RegionName = morph_df$name,
        folder = rep(folder_name, nrow(morph_df)),
        description = rep(description_part, nrow(morph_df)),
        ConstituentRegion = morph_df$name
      )
      
      # Add to existing data
      current_data <- defined_regions_data()
      if (is.null(current_data) || nrow(current_data) == 0) {
        updated_data <- new_rows
      } else {
        updated_data <- dplyr::bind_rows(current_data, new_rows)
      }
      
      # Save to file
      utils::write.table(updated_data, regions_file, sep = "\t", 
                        row.names = FALSE, quote = FALSE)
      
      # Update reactive value - this triggers reactive updates
      defined_regions_data(updated_data)
      
      # Mark that file is no longer empty
      originally_empty(FALSE)
      
      showNotification(paste("Added all", nrow(morph_df), "regions with original names"), type = "message")
    })
    
    # Remove region
    observeEvent(input$remove_region, {
      if (input$region_name == "") {
        showNotification("Please enter a region name to remove", type = "warning")
        return()
      }
      
      current_data <- defined_regions_data()
      if (is.null(current_data) || nrow(current_data) == 0) {
        showNotification("No regions defined to remove", type = "warning")
        return()
      }
      
      # Check if region exists
      if (!input$region_name %in% current_data$RegionName) {
        showNotification("Region name not found", type = "warning")
        return()
      }
      
      # Remove all rows with the specified region name
      updated_data <- current_data %>%
        dplyr::filter(RegionName != input$region_name)
      
      # Count removed rows
      removed_count <- nrow(current_data) - nrow(updated_data)
      
      # Save to file
      utils::write.table(updated_data, regions_file, sep = "\t", 
                        row.names = FALSE, quote = FALSE)
      
      # Update reactive value
      defined_regions_data(updated_data)
      
      # Clear region name input
      updateTextInput(session, "region_name", value = "")
      
      showNotification(paste("Removed", removed_count, "rows for region:", input$region_name), type = "message")
    })
    
    # Remove all regions
    observeEvent(input$remove_all_regions, {
      current_data <- defined_regions_data()
      if (is.null(current_data) || nrow(current_data) == 0) {
        showNotification("No regions defined to remove", type = "warning")
        return()
      }
      
      # Count total rows to be removed
      total_rows <- nrow(current_data)
      
      # Create empty data frame with proper structure
      empty_data <- tibble::tibble(
        RegionName = character(0),
        folder = character(0),
        description = character(0),
        ConstituentRegion = character(0)
      )
      
      # Save empty file
      utils::write.table(empty_data, regions_file, sep = "\t", 
                        row.names = FALSE, quote = FALSE)
      
      # Update reactive value
      defined_regions_data(empty_data)
      
      # Clear region name input
      updateTextInput(session, "region_name", value = "")
      
      showNotification(paste("Removed all", total_rows, "region definitions"), type = "message")
    })
    
    # Close app
    observeEvent(input$close_app, {
      stopApp()
    })
  }
  
  # Run the app
  shiny::shinyApp(ui = ui, server = server)
}
