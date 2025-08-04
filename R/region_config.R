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
  
  # Function to check if a directory contains *_tacs.tsv files (excluding combined files)
  has_tacs_files <- function(dir_path) {
    tacs_files <- list.files(dir_path, pattern = "*_tacs\\.tsv$", recursive = TRUE)
    # Exclude combined TACs files
    tacs_files <- tacs_files[!grepl("desc-combined_tacs\\.tsv$", tacs_files)]
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
  
  # Get all *_tacs.tsv files in this directory (excluding combined files)
  tacs_files <- list.files(dir_path, pattern = "*_tacs\\.tsv$", 
                          recursive = TRUE, full.names = TRUE)
  # Exclude combined TACs files
  tacs_files <- tacs_files[!grepl("desc-combined_tacs\\.tsv$", tacs_files)]
  
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
#' @param bids_dir Character string path to the BIDS directory (default: NULL)
#' @param derivatives_dir Character string path to derivatives directory (default: bids_dir/derivatives if bids_dir provided)
#' @param kinfitr_output_foldername Character string name for kinfitr output folder within derivatives (default: "kinfitr")
#' @details Config files (kinfitr_regions.tsv) are saved to:
#'   - bids_dir/code/kinfitr if bids_dir provided
#'   - derivatives_dir/kinfitr_output_foldername if no bids_dir
#' @export
region_definition_app <- function(bids_dir = NULL, derivatives_dir = NULL, kinfitr_output_foldername = "kinfitr") {
  
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
  
  # Set config directory (for kinfitr_regions.tsv)
  if (!is.null(bids_dir)) {
    # Standard BIDS structure: bids_dir/code/kinfitr
    config_dir <- file.path(bids_dir, "code", "kinfitr")
  } else {
    # No BIDS dir: use derivatives/kinfitr_output_foldername
    config_dir <- file.path(derivatives_dir, kinfitr_output_foldername)
  }
  
  # Create config directory if it doesn't exist
  if (!dir.exists(config_dir)) {
    dir.create(config_dir, recursive = TRUE)
    cat("Created config directory:", config_dir, "\n")
  }
  
  # Normalize paths
  if (!is.null(bids_dir)) {
    bids_dir <- normalizePath(bids_dir, mustWork = FALSE)
  }
  derivatives_dir <- normalizePath(derivatives_dir, mustWork = FALSE)
  config_dir <- normalizePath(config_dir, mustWork = FALSE)
  
  # Create derivatives directory if it doesn't exist
  if (!dir.exists(derivatives_dir)) {
    dir.create(derivatives_dir, recursive = TRUE)
    cat("Created derivatives directory:", derivatives_dir, "\n")
  }
  
  # Print configuration
  cat("Starting Region Definition App with configuration:\n")
  if (!is.null(bids_dir)) {
    cat("  BIDS directory:", bids_dir, "\n")
  }
  cat("  Derivatives directory:", derivatives_dir, "\n")
  cat("  Config directory:", config_dir, "\n")
  
  # Initialize kinfitr_regions.tsv file
  regions_file <- file.path(config_dir, "kinfitr_regions.tsv")
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
          br(), br(),
          actionButton("add_all_regions", "Add All Regions from TACs File", 
                      class = "btn-info"),
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
        actionButton("remove_region", "Remove Region", 
                    class = "btn-danger"),
        br(), br(), br(), br(),
        actionButton("remove_all_regions", "Remove All Regions", 
                    class = "btn-danger", style = "background-color: #8B0000; border-color: #8B0000;"),
        
        hr(),
        actionButton("generate_tacs", HTML("&#9654; Generate Combined TACs"), 
                    class = "btn-success", style = "font-weight: bold;"),
        
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
                     br(),
                     div(
                       div(style = "margin-bottom: 10px;", 
                           strong("Filter regions by name:")),
                       div(class = "input-group",
                           tags$input(id = "region_filter", type = "text", class = "form-control",
                                     placeholder = "e.g., Caudate", value = ""),
                           div(class = "input-group-append",
                               actionButton("apply_filter", "Filter", 
                                          class = "btn btn-primary btn-sm"))
                       )
                     ),
                     br(),
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
      full_morph_df <- morph_data()
      filtered_morph_df <- filtered_morph_data()
      
      if (is.null(full_morph_df) || nrow(full_morph_df) == 0) {
        return(NULL)
      }
      
      # Get currently selected regions from all data
      selected_region_names <- c()
      for (i in 1:nrow(full_morph_df)) {
        checkbox_id <- paste0("region_", i)
        if (!is.null(input[[checkbox_id]]) && input[[checkbox_id]]) {
          selected_region_names <- c(selected_region_names, full_morph_df$name[i])
        }
      }
      
      if (is.null(filtered_morph_df) || nrow(filtered_morph_df) == 0) {
        if (!is.null(input$region_filter) && input$region_filter != "") {
          if (length(selected_region_names) > 0) {
            # Show only selected regions when filter doesn't match anything new
            selected_checkboxes <- lapply(selected_region_names, function(region_name) {
              # Find the original index in full data
              orig_idx <- which(full_morph_df$name == region_name)
              checkbox_id <- paste0("region_", orig_idx)
              checkboxInput(
                inputId = checkbox_id,
                label = region_name,
                value = TRUE
              )
            })
            
            return(tagList(
              div(class = "alert alert-info", 
                  "No new regions match the filter '", input$region_filter, "'"),
              hr(),
              div(style = "background-color: #f8f9fa; padding: 10px; border-radius: 5px;",
                  h5("Currently Selected Regions:"),
                  do.call(tagList, selected_checkboxes))
            ))
          } else {
            return(div(class = "alert alert-info", 
                      "No regions match the filter '", input$region_filter, "'"))
          }
        }
        return(NULL)
      }
      
      # Separate filtered regions into unselected and selected
      filtered_names <- filtered_morph_df$name
      unselected_filtered_names <- setdiff(filtered_names, selected_region_names)
      
      # Count only unselected filtered regions for "Select all" button
      unselected_count <- length(unselected_filtered_names)
      
      # Create select all checkbox (only for unselected filtered regions)
      select_all_checkbox <- if (unselected_count > 0) {
        checkboxInput(
          inputId = "select_all_visible",
          label = paste0("Select all visible (", unselected_count, " unselected regions)"),
          value = FALSE
        )
      } else {
        div(class = "alert alert-success", "All visible regions are already selected!")
      }
      
      # Create checkboxes for UNSELECTED filtered regions only
      unselected_filtered_checkboxes <- if (length(unselected_filtered_names) > 0) {
        lapply(unselected_filtered_names, function(region_name) {
          # Find original index in full data
          orig_idx <- which(full_morph_df$name == region_name)
          checkbox_id <- paste0("region_", orig_idx)
          checkboxInput(
            inputId = checkbox_id,
            label = region_name,
            value = FALSE
          )
        })
      } else {
        NULL
      }
      
      # Create checkboxes for ALL selected regions (matching filter or not)
      selected_checkboxes <- if (length(selected_region_names) > 0) {
        lapply(selected_region_names, function(region_name) {
          # Find original index in full data
          orig_idx <- which(full_morph_df$name == region_name)
          checkbox_id <- paste0("region_", orig_idx)
          checkboxInput(
            inputId = checkbox_id,
            label = region_name,
            value = TRUE
          )
        })
      } else {
        NULL
      }
      
      # Build the result
      result_list <- list()
      
      # Add select all section
      result_list <- append(result_list, list(
        div(style = "border-bottom: 1px solid #ddd; padding-bottom: 10px; margin-bottom: 10px;",
            select_all_checkbox)
      ))
      
      # Add unselected filtered regions if they exist
      if (!is.null(unselected_filtered_checkboxes)) {
        result_list <- append(result_list, list(
          h5("Available Regions (matching filter):"),
          do.call(tagList, unselected_filtered_checkboxes)
        ))
      }
      
      # Add selected regions section if they exist
      if (!is.null(selected_checkboxes)) {
        result_list <- append(result_list, list(
          hr(),
          div(style = "background-color: #f8f9fa; padding: 10px; border-radius: 5px;",
              h5(paste0("Selected Regions (", length(selected_region_names), " total):")),
              do.call(tagList, selected_checkboxes))
        ))
      }
      
      do.call(tagList, result_list)
    })
    
    # Update TACs file choices
    observe({
      if (nrow(tacs_list) > 0) {
        updateSelectInput(session, "selected_tacs",
                         choices = setNames(tacs_list$tacs_filedescription,
                                          tacs_list$tacs_filedescription))
      }
    })
    
    # Handle filter button click
    observeEvent(input$apply_filter, {
      full_morph_df <- morph_data()
      if (is.null(full_morph_df) || nrow(full_morph_df) == 0) {
        return()
      }
      
      if (is.null(input$region_filter) || input$region_filter == "") {
        # No filter - show all regions
        filtered_morph_data(full_morph_df)
      } else {
        # Apply filter
        filtered_df <- full_morph_df %>%
          dplyr::filter(stringr::str_detect(stringr::str_to_lower(name), 
                                           stringr::str_to_lower(input$region_filter)))
        filtered_morph_data(filtered_df)
      }
    })
    
    # Reactive to store loaded TACs data
    loaded_tacs_data <- reactiveVal(NULL)
    
    # Reactive to store morph data
    morph_data <- reactiveVal(NULL)
    
    # Reactive value to store filtered morph data (updated manually via button)
    filtered_morph_data <- reactiveVal(NULL)
    
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
          
          # Find _tacs.tsv files in this directory that match the description (excluding combined files)
          tacs_files <- list.files(first_path, pattern = "*_tacs\\.tsv$", 
                                  recursive = TRUE, full.names = TRUE)
          # Exclude combined TACs files
          tacs_files <- tacs_files[!grepl("desc-combined_tacs\\.tsv$", tacs_files)]
          
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
                  filtered_morph_data(filtered_morph)  # Initialize filtered data with all data
                  cat("Loaded", nrow(filtered_morph), "regions with non-zero volume from:", morph_file, "\n")
                } else {
                  cat("Warning: Expected columns 'name' and 'volume.mm3' not found in morph file\n")
                  morph_data(NULL)
                  filtered_morph_data(NULL)
                }
              }, error = function(e) {
                cat("Error reading morph file:", e$message, "\n")
                morph_data(NULL)
                filtered_morph_data(NULL)
              })
            } else {
              cat("Morph file not found:", morph_file, "\n")
              morph_data(NULL)
              filtered_morph_data(NULL)
            }
          } else {
            cat("No matching TACs file found for description:", description_part, "\n")
            morph_data(NULL)
            filtered_morph_data(NULL)
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
      # Get selected checkboxes from ALL regions (both filtered and previously selected)
      full_morph_df <- morph_data()
      if (is.null(full_morph_df) || nrow(full_morph_df) == 0) {
        showNotification("No regions available to add", type = "warning")
        return()
      }
      
      selected_regions <- c()
      for (i in 1:nrow(full_morph_df)) {
        checkbox_id <- paste0("region_", i)
        if (!is.null(input[[checkbox_id]]) && input[[checkbox_id]]) {
          selected_regions <- c(selected_regions, full_morph_df$name[i])
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
      
      # Clear all checkboxes (both filtered and previously selected)
      for (i in 1:nrow(full_morph_df)) {
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
      
      morph_df <- morph_data()  # Use full data, not filtered
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
    
    # Handle select all visible checkbox
    observeEvent(input$select_all_visible, {
      filtered_morph_df <- filtered_morph_data()
      full_morph_df <- morph_data()
      
      if (is.null(filtered_morph_df) || nrow(filtered_morph_df) == 0 || 
          is.null(full_morph_df) || nrow(full_morph_df) == 0) {
        return()
      }
      
      # Get currently selected regions
      selected_region_names <- c()
      for (i in 1:nrow(full_morph_df)) {
        checkbox_id <- paste0("region_", i)
        if (!is.null(input[[checkbox_id]]) && input[[checkbox_id]]) {
          selected_region_names <- c(selected_region_names, full_morph_df$name[i])
        }
      }
      
      # Get unselected filtered regions
      filtered_names <- filtered_morph_df$name
      unselected_filtered_names <- setdiff(filtered_names, selected_region_names)
      
      # Update only the UNSELECTED filtered region checkboxes
      for (region_name in unselected_filtered_names) {
        orig_idx <- which(full_morph_df$name == region_name)
        checkbox_id <- paste0("region_", orig_idx)
        updateCheckboxInput(session, checkbox_id, value = input$select_all_visible)
      }
    })
    
    # Update select all checkbox based on individual selections
    observe({
      filtered_morph_df <- filtered_morph_data()
      full_morph_df <- morph_data()
      
      if (is.null(filtered_morph_df) || nrow(filtered_morph_df) == 0 || 
          is.null(full_morph_df) || nrow(full_morph_df) == 0) {
        return()
      }
      
      # Get currently selected regions
      selected_region_names <- c()
      for (i in 1:nrow(full_morph_df)) {
        checkbox_id <- paste0("region_", i)
        if (!is.null(input[[checkbox_id]]) && input[[checkbox_id]]) {
          selected_region_names <- c(selected_region_names, full_morph_df$name[i])
        }
      }
      
      # Get unselected filtered regions
      filtered_names <- filtered_morph_df$name
      unselected_filtered_names <- setdiff(filtered_names, selected_region_names)
      
      # Update select all checkbox state (should be checked only if no unselected filtered regions remain)
      all_unselected_filtered_selected <- length(unselected_filtered_names) == 0
      if (!is.null(input$select_all_visible) && input$select_all_visible != all_unselected_filtered_selected) {
        updateCheckboxInput(session, "select_all_visible", value = all_unselected_filtered_selected)
      }
    })
    
    # Generate Combined TACs
    observeEvent(input$generate_tacs, {
      # Check if there are defined regions
      current_data <- defined_regions_data()
      if (is.null(current_data) || nrow(current_data) == 0) {
        showNotification("No regions defined. Please add regions first.", 
                        type = "warning", duration = 5)
        return()
      }
      
      tryCatch({
        # Show processing notification that stays visible during processing
        processing_id <- showNotification(HTML("Generating combined TACs.<br>Please wait..."), 
                        type = "message", duration = NULL, id = "processing_tacs")
        
        # Step 1: Create file mapping
        kinfitr_regions_file <- regions_file
        derivatives_folder <- normalizePath(derivatives_dir, mustWork = FALSE)
        
        cat("Creating file mapping...\n")
        regions_files_data <- create_kinfitr_regions_files(kinfitr_regions_file, derivatives_folder)
        
        # Step 2: Process all regions
        kinfitr_regions_files_path <- file.path(config_dir, "kinfitr_regions_files.tsv")
        combined_output_folder <- file.path(derivatives_dir, kinfitr_output_foldername)
        
        cat("Processing all regions...\n")
        
        # Use consolidated TACs creation instead of separate files
        combined_data <- create_kinfitr_combined_tacs(kinfitr_regions_files_path, derivatives_folder, combined_output_folder)
        
        # Show success notification with summary  
        total_rows <- nrow(combined_data)
        total_regions <- length(unique(combined_data$region))
        total_subjects <- length(unique(combined_data$sub))
        
        success_msg <- paste0(
          "Successfully created consolidated TACs file. ",
          "Total rows: ", total_rows, ", ",
          "Regions: ", total_regions, ", ",
          "Subjects: ", total_subjects, ". ",
          "Output: desc-combined_tacs.tsv in ", combined_output_folder
        )
        
        showNotification(success_msg, type = "message", duration = 5)
        
        cat("=== Combined TACs Generation Complete ===\n")
        cat("Total rows:", total_rows, "\n")
        cat("Total regions:", total_regions, "\n")
        cat("Subjects processed:", total_subjects, "\n")
        cat("Output folder:", combined_output_folder, "\n")
        
        # Remove processing notification and show success
        removeNotification("processing_tacs")
        
        # Close app after 5 seconds to allow user to see success message
        later::later(function() {
          shiny::stopApp()
        }, delay = 5)
        
      }, error = function(e) {
        # Remove processing notification on error
        removeNotification("processing_tacs")
        
        error_msg <- paste("Error generating combined TACs:", e$message)
        showNotification(error_msg, type = "error", duration = 10)
        cat("Error:", e$message, "\n")
      })
    })
  }
  
  # Run the app
  shiny::shinyApp(ui = ui, server = server)
}
