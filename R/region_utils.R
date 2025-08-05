#' Combine Single Region TAC using Volume-Weighted Averaging
#'
#' @description Core function for volume-weighted averaging of constituent regions
#'
#' @param tacs_data TACs tibble in wide format (regions as columns)
#' @param morph_data Morph tibble with volume data
#' @param constituent_regions Vector of region names to combine
#' @param region_name Name for the combined region
#' @return Single tibble with combined TAC and total volume
#' @export
combine_single_region_tac <- function(tacs_data, morph_data, constituent_regions, region_name) {
  
  # Validate inputs with graceful handling
  if (is.null(tacs_data) || nrow(tacs_data) == 0) {
    warning(paste("TACs data is empty or NULL for", region_name, "- excluding from output"))
    return(tibble::tibble())
  }
  
  if (is.null(morph_data) || nrow(morph_data) == 0) {
    warning(paste("Morph data is empty or NULL for", region_name, "- excluding from output"))
    return(tibble::tibble())
  }
  
  if (length(constituent_regions) == 0) {
    warning(paste("No constituent regions provided for", region_name, "- excluding from output"))
    return(tibble::tibble())
  }
  
  # Check which constituent regions are available in both datasets
  # Note: readr::read_tsv preserves hyphens in column names, so no conversion needed
  available_in_tacs <- constituent_regions[constituent_regions %in% colnames(tacs_data)]
  available_in_morph <- constituent_regions[constituent_regions %in% morph_data$name]
  available_regions <- intersect(available_in_tacs, available_in_morph)
  
  if (length(available_regions) == 0) {
    # GRACEFUL: Return empty tibble instead of stopping
    warning(paste("No constituent regions found for", region_name, ":",
                  paste(constituent_regions, collapse = ", "), 
                  "- excluding from output"))
    return(tibble::tibble())
  }
  
  if (length(available_regions) < length(constituent_regions)) {
    missing_regions <- setdiff(constituent_regions, available_regions)
    # GRACEFUL: Warning instead of error
    warning(paste("Some constituent regions not found for", region_name, ":", 
                  paste(missing_regions, collapse = ", "), 
                  "- using available regions only"))
  }
  
  # Filter morph data for available regions
  # Note: readr::read_tsv preserves "volume-mm3" as is, no conversion to volume.mm3
  region_volumes <- morph_data %>%
    dplyr::filter(name %in% available_regions) %>%
    dplyr::select(name, `volume-mm3`)
  
  # Calculate total volume
  total_volume <- sum(region_volumes$`volume-mm3`)
  
  if (total_volume == 0) {
    # GRACEFUL: Return empty tibble instead of stopping
    warning(paste("Total volume is zero for", region_name, 
                  "- excluding from output"))
    return(tibble::tibble())
  }
  
  # Calculate volume fractions
  region_volumes <- region_volumes %>%
    dplyr::mutate(volume_fraction = `volume-mm3` / total_volume)
  
  # Apply volume weighting to TACs data
  time_cols <- c("frame_start", "frame_end")
  
  # Initialize result with time columns
  combined_tac <- tacs_data %>%
    dplyr::select(dplyr::all_of(time_cols))
  
  # Calculate volume-weighted TAC for each time frame
  weighted_tac_values <- rep(0, nrow(tacs_data))
  
  for (region in available_regions) {
    volume_frac <- region_volumes$volume_fraction[region_volumes$name == region]
    # Note: readr::read_tsv preserves hyphens in column names, no conversion needed
    region_tac <- tacs_data[[region]]
    weighted_tac_values <- weighted_tac_values + (region_tac * volume_frac)
  }
  
  # Add combined results
  combined_tac$name <- region_name
  combined_tac$TAC <- weighted_tac_values
  combined_tac$`volume-mm3` <- total_volume
  
  # Calculate frame duration and midpoint
  combined_tac$frame_dur <- combined_tac$frame_end - combined_tac$frame_start
  combined_tac$frame_mid <- combined_tac$frame_start + 0.5 * combined_tac$frame_dur
  
  return(combined_tac)
}

#' Create kinfitr Regions Files Mapping
#'
#' @description Creates file mapping TSV linking regions to their TACs/morph files
#'
#' @param kinfitr_regions_file Path to kinfitr_regions.tsv
#' @param derivatives_folder Base path to derivatives folder
#' @return Creates kinfitr_regions_files.tsv and returns the mapping data
#' @export
create_kinfitr_regions_files <- function(kinfitr_regions_file, derivatives_folder) {
  
  # Validate inputs
  if (!file.exists(kinfitr_regions_file)) {
    stop(paste("kinfitr_regions.tsv file not found:", kinfitr_regions_file))
  }
  
  if (!dir.exists(derivatives_folder)) {
    stop(paste("Derivatives folder not found:", derivatives_folder))
  }
  
  # Read kinfitr_regions.tsv
  regions_config <- readr::read_tsv(kinfitr_regions_file, show_col_types = FALSE)
  
  if (nrow(regions_config) == 0) {
    stop("kinfitr_regions.tsv is empty")
  }
  
  # Get unique folder/description combinations
  unique_configs <- regions_config %>%
    dplyr::select(folder, description) %>%
    dplyr::distinct()
  
  # Find corresponding files for each unique configuration
  file_mappings <- purrr::map_dfr(1:nrow(unique_configs), function(i) {
    folder_name <- unique_configs$folder[i]
    description <- unique_configs$description[i]
    
    # Search for files matching this configuration
    folder_path <- file.path(derivatives_folder, folder_name)
    
    if (!dir.exists(folder_path)) {
      warning(paste("Folder not found:", folder_path))
      return(tibble::tibble())
    }
    
    # Step 1: Find ALL TACs files (including hemi files, excluding combined files)
    all_tacs_files <- list.files(folder_path, pattern = "*_tacs\\.tsv$", 
                                recursive = TRUE, full.names = TRUE)
    # Exclude combined TACs files
    all_tacs_files <- all_tacs_files[!grepl("desc-combinedregions_tacs\\.tsv$", all_tacs_files)]
    
    if (length(all_tacs_files) == 0) {
      warning(paste("No TACs files found in", folder_path))
      return(tibble::tibble())
    }
    
    # Step 2: Parse all TACs files using BIDS parsing
    parsed_files <- tryCatch({
      kinfitr::bids_parse_files(folder_path)
    }, error = function(e) {
      warning(paste("Error parsing BIDS files in", folder_path, ":", e$message))
      return(NULL)
    })
    
    if (is.null(parsed_files) || length(parsed_files$filedata) == 0) {
      warning(paste("No valid BIDS files found in", folder_path))
      return(tibble::tibble())
    }
    
    # Step 3: Parse description from kinfitr_regions.tsv into key-value pairs
    description_attributes <- extract_bids_attributes_from_filename(description)
    
    # Step 4: Filter parsed files using inner_join to match all key-value pairs
    matched_files <- tryCatch({
      # Combine main-level and filedata-level BIDS attributes
      files_with_attrs <- parsed_files %>%
        tidyr::unnest(filedata) %>%
        dplyr::filter(measurement == "tacs") %>%
        # Select relevant BIDS attributes from both levels
        dplyr::select(sub, ses, trc, rec, task, run, desc, hemi, pvc, path)
      
      # Match files where ALL non-identifier attributes are identical
      # Individual identifiers to exclude from matching: sub, ses, trc, rec, task, run, pet
      identifier_cols <- c("sub", "ses", "trc", "rec", "task", "run", "pet")
      
      # Get non-identifier attributes from description
      desc_attrs <- description_attributes %>%
        dplyr::select(-any_of(identifier_cols))
      
      # Match on desc, hemi, and pvc attributes (with NA when not present)
      # Standardize description attributes to include hemi and pvc as NA if missing
      desc_standardized <- tibble::tibble(
        desc = description_attributes$desc,
        hemi = if("hemi" %in% names(description_attributes)) description_attributes$hemi else NA_character_,
        pvc = if("pvc" %in% names(description_attributes)) description_attributes$pvc else NA_character_
      )
      
      # Match files with identical desc, hemi, and pvc values (using coalesce for NA matching)
      matched_files_list <- files_with_attrs %>%
        dplyr::filter(
          desc == desc_standardized$desc,
          dplyr::coalesce(hemi, "NA") == dplyr::coalesce(desc_standardized$hemi, "NA"),
          dplyr::coalesce(pvc, "NA") == dplyr::coalesce(desc_standardized$pvc, "NA")
        )
      
      matched <- matched_files_list
      
      # Check if path column exists and select appropriately
      if ("path" %in% colnames(matched)) {
        matched <- matched %>% dplyr::select(filename = path, desc)
      } else {
        warning("Path column not found after join")
        return(tibble::tibble())
      }
      
      matched
    }, error = function(e) {
      warning(paste("Error filtering BIDS files:", e$message))
      return(tibble::tibble())
    })
    
    if (nrow(matched_files) == 0) {
      warning(paste("No files matched description", description, "in", folder_path))
      return(tibble::tibble())
    }
    
    # Step 4: Create file pairs for matched files
    file_pairs <- purrr::map_dfr(matched_files$filename, function(tacs_file) {
      # tacs_file contains relative path like "sub-01/ses-baseline/filename.tsv"
      tacs_relative_path <- as.character(tacs_file)
      
      # Generate corresponding morph filename by replacing the suffix
      morph_relative_path <- stringr::str_replace(tacs_relative_path, "_tacs\\.tsv$", "_morph.tsv")
      
      # Construct full paths to check if morph file exists
      morph_full_path <- file.path(folder_path, morph_relative_path)
      
      if (!file.exists(morph_full_path)) {
        warning(paste("Corresponding morph file not found:", morph_relative_path))
        return(tibble::tibble())
      }
      
      # Return relative paths from derivatives folder
      tibble::tibble(
        folder = folder_name,
        description = description,
        tacs_filename = file.path(folder_name, tacs_relative_path),
        morph_filename = file.path(folder_name, morph_relative_path)
      )
    })
    
    return(file_pairs)
  })
  
  if (nrow(file_mappings) == 0) {
    stop("No valid TACs/morph file pairs found")
  }
  
  # Join with original regions config
  regions_files <- regions_config %>%
    dplyr::left_join(file_mappings, by = c("folder", "description"))
  
  # Remove rows where files weren't found
  regions_files <- regions_files %>%
    dplyr::filter(!is.na(tacs_filename), !is.na(morph_filename))
  
  if (nrow(regions_files) == 0) {
    stop("No regions could be matched to valid file pairs")
  }
  
  # Write to output file
  output_dir <- dirname(kinfitr_regions_file)
  output_file <- file.path(output_dir, "kinfitr_regions_files.tsv")
  
  readr::write_tsv(regions_files, output_file)
  
  cat("Created kinfitr_regions_files.tsv with", nrow(regions_files), "region-file mappings\n")
  cat("Output file:", output_file, "\n")
  
  return(regions_files)
}

#' Combine Regions from Single TACs/Morph File Pair
#'
#' @description Process all region combinations for one TACs/morph file pair
#'
#' @param derivatives_folder Base path to derivatives folder
#' @param tacs_relative_path Relative path to TACs file from derivatives folder
#' @param morph_relative_path Relative path to morph file from derivatives folder
#' @param regions_for_files Filtered regions config for these specific files
#' @return Tibble with all combined regions for this file pair
#' @export
combine_regions_from_files <- function(derivatives_folder, tacs_relative_path, 
                                      morph_relative_path, regions_for_files) {
  
  # Construct full file paths
  tacs_full_path <- file.path(derivatives_folder, tacs_relative_path)
  morph_full_path <- file.path(derivatives_folder, morph_relative_path)
  
  # Validate file existence with graceful handling
  if (!file.exists(tacs_full_path)) {
    warning(paste("TACs file not found:", tacs_full_path, "- skipping"))
    return(tibble::tibble())
  }
  
  if (!file.exists(morph_full_path)) {
    warning(paste("Morph file not found:", morph_full_path, "- skipping"))
    return(tibble::tibble())
  }
  
  # Read data files with graceful error handling
  tacs_data <- tryCatch({
    readr::read_tsv(tacs_full_path, show_col_types = FALSE)
  }, error = function(e) {
    warning(paste("Error reading TACs file:", e$message, "- skipping"))
    return(NULL)
  })
  
  if (is.null(tacs_data)) {
    return(tibble::tibble())
  }
  
  morph_data <- tryCatch({
    readr::read_tsv(morph_full_path, show_col_types = FALSE)
  }, error = function(e) {
    warning(paste("Error reading morph file:", e$message, "- skipping"))
    return(NULL)
  })
  
  if (is.null(morph_data)) {
    return(tibble::tibble())
  }
  
  # Group regions by RegionName
  region_groups <- regions_for_files %>%
    dplyr::group_by(RegionName) %>%
    dplyr::summarise(
      ConstituentRegions = list(ConstituentRegion),
      .groups = "drop"
    )
  
  # Combine each region group
  combined_results <- purrr::map_dfr(1:nrow(region_groups), function(i) {
    region_name <- region_groups$RegionName[i]
    constituent_regions <- region_groups$ConstituentRegions[[i]]
    
    tryCatch({
      combine_single_region_tac(tacs_data, morph_data, constituent_regions, region_name)
    }, error = function(e) {
      warning(paste("Failed to combine region", region_name, ":", e$message))
      return(tibble::tibble())
    })
  })
  
  if (nrow(combined_results) == 0) {
    warning("No regions were successfully combined")
    return(tibble::tibble())
  }
  
  return(combined_results)
}

#' Process All kinfitr Regions Across All Files
#'
#' @description Orchestrate entire region combination process across all files
#'
#' @param kinfitr_regions_files_path Path to kinfitr_regions_files.tsv
#' @param derivatives_folder Base path to derivatives folder
#' @param output_folder Where to save combined TACs files
#' @return Processing summary tibble
#' @export
process_all_kinfitr_regions <- function(kinfitr_regions_files_path, derivatives_folder, output_folder) {
  
  # Validate inputs
  if (!file.exists(kinfitr_regions_files_path)) {
    stop(paste("kinfitr_regions_files.tsv not found:", kinfitr_regions_files_path))
  }
  
  if (!dir.exists(derivatives_folder)) {
    stop(paste("Derivatives folder not found:", derivatives_folder))
  }
  
  # Create output folder if it doesn't exist
  if (!dir.exists(output_folder)) {
    dir.create(output_folder, recursive = TRUE)
    cat("Created output folder:", output_folder, "\n")
  }
  
  # Read regions files mapping
  regions_files <- readr::read_tsv(kinfitr_regions_files_path, show_col_types = FALSE)
  
  if (nrow(regions_files) == 0) {
    stop("kinfitr_regions_files.tsv is empty")
  }
  
  # Group by unique TACs/morph file pairs
  file_groups <- regions_files %>%
    dplyr::group_by(tacs_filename, morph_filename) %>%
    dplyr::group_nest(.key = "regions_data")
  
  # Process each file pair
  processing_summary <- purrr::map_dfr(1:nrow(file_groups), function(i) {
    tacs_file <- file_groups$tacs_filename[i]
    morph_file <- file_groups$morph_filename[i]
    regions_data <- file_groups$regions_data[[i]]
    
    cat("Processing:", tacs_file, "\n")
    
    tryCatch({
      # Combine regions for this file pair
      combined_results <- combine_regions_from_files(
        derivatives_folder, tacs_file, morph_file, regions_data
      )
      
      if (nrow(combined_results) > 0) {
        # Generate output filename
        base_filename <- basename(tacs_file)
        output_filename <- stringr::str_replace(base_filename, "_tacs\\.tsv$", "_combined_tacs.tsv")
        output_path <- file.path(output_folder, output_filename)
        
        # Save combined results
        readr::write_tsv(combined_results, output_path)
        
        cat("  Saved:", output_filename, "with", nrow(combined_results), "time points\n")
        
        return(tibble::tibble(
          tacs_file = tacs_file,
          output_file = output_filename,
          regions_combined = length(unique(combined_results$name)),
          time_points = nrow(combined_results),
          status = "success"
        ))
      } else {
        warning(paste("No regions combined for", tacs_file))
        return(tibble::tibble(
          tacs_file = tacs_file,
          output_file = NA_character_,
          regions_combined = 0,
          time_points = 0,
          status = "no_regions"
        ))
      }
    }, error = function(e) {
      warning(paste("Error processing", tacs_file, ":", e$message))
      return(tibble::tibble(
        tacs_file = tacs_file,
        output_file = NA_character_,
        regions_combined = 0,
        time_points = 0,
        status = paste("error:", e$message)
      ))
    })
  })
  
  # Print summary
  successful_files <- sum(processing_summary$status == "success")
  total_files <- nrow(processing_summary)
  total_regions <- sum(processing_summary$regions_combined, na.rm = TRUE)
  
  cat("\n=== Processing Summary ===\n")
  cat("Files processed successfully:", successful_files, "/", total_files, "\n")
  cat("Total regions combined:", total_regions, "\n")
  cat("Output folder:", output_folder, "\n")
  
  return(processing_summary)
}

#' Extract BIDS Attributes from Filename
#'
#' @description Extract BIDS key-value pairs from filename following bloodstream pattern
#'
#' @param filename Filename to parse (can be full path or basename)
#' @return Tibble with BIDS attributes (sub, ses, trc, rec, task, run, desc, pet)
#' @export
extract_bids_attributes_from_filename <- function(filename) {
  # Parse filename to extract BIDS key-value pairs
  # Required: sub, ses, trc, rec, task, run, desc, pet
  # Optional: any other key-value pairs found in filename
  # IMPORTANT: Extract only VALUES, not key-value pairs (e.g., "01" not "sub-01")
  
  basename_file <- basename(filename)
  
  # Extract all key-value pairs from filename using regex
  # Pattern matches: key-value where key is letters and value is alphanumeric/hyphens
  matches <- stringr::str_extract_all(basename_file, "([a-zA-Z]+)-([a-zA-Z0-9]+)")[[1]]
  
  # Parse matches into key-value pairs
  attributes <- list()
  for (match in matches) {
    parts <- stringr::str_split(match, "-", n = 2)[[1]]
    if (length(parts) == 2) {
      key <- parts[1]
      value <- parts[2]
      attributes[[key]] <- value
    }
  }
  
  # Always include required attributes (set to NA if missing)
  result <- tibble::tibble(
    sub = attributes[["sub"]] %||% NA_character_,
    ses = attributes[["ses"]] %||% NA_character_,
    trc = attributes[["trc"]] %||% NA_character_,
    rec = attributes[["rec"]] %||% NA_character_,
    task = attributes[["task"]] %||% NA_character_,
    run = attributes[["run"]] %||% NA_character_,
    desc = attributes[["desc"]] %||% NA_character_
  )
  
  # Create pet column (following bloodstream pattern) - PET measurement identifier
  # Remove desc and measurement suffix to get the core PET identifier
  pet <- basename_file %>%
    stringr::str_remove("_desc-[^_]+.*$") %>%  # Remove desc and everything after
    stringr::str_remove("_tacs\\.tsv$")       # Remove measurement suffix if still there
  result$pet <- pet
  
  # Add any other attributes found (excluding the required ones already added)
  required_keys <- c("sub", "ses", "trc", "rec", "task", "run", "desc")
  other_attributes <- attributes[!names(attributes) %in% required_keys]
  
  for (key in names(other_attributes)) {
    result[[key]] <- other_attributes[[key]]
  }
  
  return(result)
}

#' Create Consolidated kinfitr Combined TACs Output
#'
#' @description Create single consolidated TSV with BIDS attributes and long-format TACs
#'
#' @param kinfitr_regions_files_path Path to kinfitr_regions_files.tsv
#' @param derivatives_folder Base path to derivatives folder
#' @param output_dir Where to save consolidated combined TACs file
#' @param bids_dir Path to BIDS directory (optional, for participant data and PET metadata)
#' @param participant_data Participant data loaded from BIDS directory (optional)
#' @return Tibble with all combined TACs data in long format with BIDS attributes
#' @export
create_kinfitr_combined_tacs <- function(kinfitr_regions_files_path, derivatives_folder, output_dir, bids_dir = NULL, participant_data = NULL) {
  
  # Validate inputs
  if (!file.exists(kinfitr_regions_files_path)) {
    stop(paste("kinfitr_regions_files.tsv not found:", kinfitr_regions_files_path))
  }
  
  if (!dir.exists(derivatives_folder)) {
    stop(paste("Derivatives folder not found:", derivatives_folder))
  }
  
  # Parse BIDS study data once at the beginning if BIDS directory is provided
  study_data <- NULL
  if (!is.null(bids_dir)) {
    tryCatch({
      study_data <- kinfitr::bids_parse_study(bids_dir)
      cat("Parsed BIDS study with", nrow(study_data), "measurements\n")
    }, error = function(e) {
      warning(paste("Error parsing BIDS study:", e$message))
    })
  }
  
  # Create output folder if it doesn't exist
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
    cat("Created output folder:", output_dir, "\n")
  }
  
  # Read regions files mapping
  regions_files <- readr::read_tsv(kinfitr_regions_files_path, show_col_types = FALSE)
  
  if (nrow(regions_files) == 0) {
    stop("kinfitr_regions_files.tsv is empty")
  }
  
  # Group by unique TACs/morph file pairs
  file_groups <- regions_files %>%
    dplyr::group_by(tacs_filename, morph_filename) %>%
    dplyr::group_nest(.key = "regions_data")
  
  # Process all file pairs and collect results
  all_combined_data <- purrr::map_dfr(1:nrow(file_groups), function(i) {
    tacs_file <- file_groups$tacs_filename[i]
    morph_file <- file_groups$morph_filename[i]
    regions_data <- file_groups$regions_data[[i]]
    
    cat("Processing:", tacs_file, "\n")
    
    # Combine regions for this file pair
    combined_results <- tryCatch({
      combine_regions_from_files(derivatives_folder, tacs_file, morph_file, regions_data)
    }, error = function(e) {
      warning(paste("Error processing", tacs_file, ":", e$message))
      return(tibble::tibble())
    })
    
    if (nrow(combined_results) == 0) {
      return(tibble::tibble())
    }
    
    # Extract BIDS attributes from filename
    bids_attributes <- extract_bids_attributes_from_filename(tacs_file)
    
    # Extract PET metadata using pre-parsed study data if available
    pet_metadata <- list(InjectedRadioactivity = NA_real_, InjectedRadioactivityUnits = NA_character_)
    if (!is.null(study_data) || !is.null(bids_dir)) {
      pet_metadata <- extract_pet_metadata(
        bids_dir, 
        bids_attributes$sub, 
        bids_attributes$ses, 
        bids_attributes$trc, 
        bids_attributes$rec, 
        bids_attributes$task, 
        bids_attributes$run,
        study_data  # Pass pre-parsed data for efficiency
      )
    }
    
    # Add BIDS attributes to each row and ensure they stay as character
    combined_results_with_bids <- combined_results %>%
      dplyr::mutate(
        sub = as.character(bids_attributes$sub),
        ses = as.character(bids_attributes$ses),
        trc = as.character(bids_attributes$trc),
        rec = as.character(bids_attributes$rec),
        task = as.character(bids_attributes$task),
        run = as.character(bids_attributes$run),
        desc = as.character(bids_attributes$desc),
        pet = as.character(bids_attributes$pet),
        InjectedRadioactivity = as.numeric(pet_metadata$InjectedRadioactivity),
        bodyweight = NA_real_  # Always include bodyweight column, initially NA
      )
    
    # Add participant data if available
    if (!is.null(participant_data) && !is.null(participant_data$data)) {
      # Check for weight column duplication (participant data might have "weight" which maps to bodyweight)
      participant_columns_to_add <- participant_data$data
      if ("weight" %in% colnames(participant_columns_to_add)) {
        # Use participant weight data for bodyweight column
        combined_results_with_bids <- combined_results_with_bids %>%
          dplyr::left_join(participant_columns_to_add, by = "sub") %>%
          dplyr::mutate(bodyweight = weight) %>%
          dplyr::select(-weight)  # Remove the participant weight column, keep bodyweight
      } else {
        # Keep our bodyweight column and add other participant data
        combined_results_with_bids <- combined_results_with_bids %>%
          dplyr::left_join(participant_columns_to_add, by = "sub")
      }
    }
    
    # Determine participant columns (excluding sub and weight if it was in original data)
    participant_columns <- if (!is.null(participant_data) && !is.null(participant_data$data)) {
      setdiff(colnames(participant_data$data), c("sub", "weight"))  # Exclude both sub and weight
    } else {
      character(0)
    }
    
    # Column order: sub, ses, trc, rec, task, run, desc, pet, InjectedRadioactivity, bodyweight, [participant_columns], region, volume_mm3, frame_*, TAC
    base_columns <- c("sub", "ses", "trc", "rec", "task", "run", "desc", "pet", "InjectedRadioactivity", "bodyweight")
    frame_columns <- c("frame_start", "frame_end", "frame_dur", "frame_mid")
    end_columns <- c("region", "volume_mm3", frame_columns, "TAC")
    
    column_order <- c(base_columns, participant_columns, end_columns)
    
    combined_results_with_bids <- combined_results_with_bids %>%
      dplyr::rename(region = name, volume_mm3 = `volume-mm3`) %>%
      dplyr::select(dplyr::all_of(column_order[column_order %in% colnames(.)]))  # Only select columns that exist
    
    return(combined_results_with_bids)
  })
  
  if (nrow(all_combined_data) == 0) {
    warning("No regions were successfully combined across all files")
    return(tibble::tibble())
  }
  
  # Save single consolidated TSV
  output_file <- file.path(output_dir, "desc-combinedregions_tacs.tsv")
  
  readr::write_tsv(all_combined_data, output_file)
  
  # Create JSON description file if participant data is available
  if (!is.null(participant_data)) {
    if ("InjectedRadioactivity" %in% colnames(all_combined_data)) {
      create_combined_tacs_json_description(
        participant_data$metadata, 
        "kBq",  # Always kBq since we standardize to this unit
        output_dir
      )
    }
  }
  
  cat("\n=== Consolidated Output Summary ===\n")
  cat("Total rows:", nrow(all_combined_data), "\n")
  cat("Total regions:", length(unique(all_combined_data$region)), "\n")
  cat("Unique subjects:", length(unique(all_combined_data$sub)), "\n")
  cat("Unique sessions:", length(unique(all_combined_data$ses)), "\n")
  if (!is.null(participant_data)) {
    cat("Participant data integrated: YES\n")
    cat("Participant columns:", paste(setdiff(colnames(participant_data$data), "sub"), collapse = ", "), "\n")
  } else {
    cat("Participant data integrated: NO\n")
  }
  cat("Output file:", output_file, "\n")
  
  return(all_combined_data)
}
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
    tacs_files <- tacs_files[!grepl("desc-combinedregions_tacs\\.tsv$", tacs_files)]
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
  tacs_files <- tacs_files[!grepl("desc-combinedregions_tacs\\.tsv$", tacs_files)]
  
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


#' Create kinfitr Regions Configuration
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
  readr::write_tsv(unique_regions, output_file)
  
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

#' Load Participant Data from BIDS Directory
#'
#' @description Load and process participants.tsv and participants.json files
#'
#' @param bids_dir Path to BIDS directory
#' @return List with participant data and metadata, or NULL if files don't exist
#' @export
load_participant_data <- function(bids_dir) {
  
  if (is.null(bids_dir) || !dir.exists(bids_dir)) {
    return(NULL)
  }
  
  participants_tsv <- file.path(bids_dir, "participants.tsv")
  participants_json <- file.path(bids_dir, "participants.json")
  
  # Check if participants.tsv exists
  if (!file.exists(participants_tsv)) {
    return(NULL)
  }
  
  # Load participants.tsv
  participants_data <- tryCatch({
    readr::read_tsv(participants_tsv, show_col_types = FALSE)
  }, error = function(e) {
    warning(paste("Error reading participants.tsv:", e$message))
    return(NULL)
  })
  
  if (is.null(participants_data) || nrow(participants_data) == 0) {
    return(NULL)
  }
  
  # Transform participant_id column to sub column
  if ("participant_id" %in% colnames(participants_data)) {
    participants_data <- participants_data %>%
      dplyr::mutate(sub = stringr::str_replace(participant_id, "^sub-", "")) %>%
      dplyr::select(-participant_id)
  } else {
    warning("participants.tsv does not contain participant_id column")
    return(NULL)
  }
  
  # Load participants.json if it exists
  participants_metadata <- NULL
  if (file.exists(participants_json)) {
    participants_metadata <- tryCatch({
      jsonlite::fromJSON(participants_json)
    }, error = function(e) {
      warning(paste("Error reading participants.json:", e$message))
      return(NULL)
    })
  }
  
  return(list(
    data = participants_data,
    metadata = participants_metadata
  ))
}

#' Extract PET Metadata from BIDS Directory using kinfitr
#'
#' @description Extract InjectedRadioactivity from matching PET measurements using kinfitr::bids_parse_study
#'
#' @param bids_dir Path to BIDS directory (can be NULL if study_data provided)
#' @param sub Subject ID (without 'sub-' prefix)
#' @param ses Session ID (without 'ses-' prefix, can be NA)
#' @param trc Tracer ID (can be NA)
#' @param rec Reconstruction ID (can be NA)
#' @param task Task ID (can be NA)  
#' @param run Run ID (can be NA)
#' @param study_data Pre-parsed BIDS study data (optional, for efficiency)
#' @return List with InjectedRadioactivity and units, or NA values if not found
#' @export
extract_pet_metadata <- function(bids_dir, sub, ses = NA, trc = NA, rec = NA, task = NA, run = NA, study_data = NULL) {
  
  if (is.na(sub)) {
    return(list(InjectedRadioactivity = NA_real_, InjectedRadioactivityUnits = NA_character_))
  }
  
  # Use pre-parsed study data if available, otherwise parse BIDS directory
  tryCatch({
    if (is.null(study_data)) {
      if (is.null(bids_dir) || !dir.exists(bids_dir)) {
        return(list(InjectedRadioactivity = NA_real_, InjectedRadioactivityUnits = NA_character_))
      }
      study_data <- kinfitr::bids_parse_study(bids_dir)
    }
    
    if (is.null(study_data) || nrow(study_data) == 0) {
      return(list(InjectedRadioactivity = NA_real_, InjectedRadioactivityUnits = NA_character_))
    }
    
    # Filter for matching measurement
    # Note: kinfitr uses different column names and may not have all BIDS entities
    matching_measurements <- study_data %>%
      dplyr::filter(sub == !!sub)
    
    # Filter by session if provided and column exists
    if (!is.na(ses) && ses != "" && "ses" %in% colnames(matching_measurements)) {
      matching_measurements <- matching_measurements %>%
        dplyr::filter(ses == !!ses)
    }
    
    # Filter by task if provided and column exists  
    if (!is.na(task) && task != "" && "task" %in% colnames(matching_measurements)) {
      matching_measurements <- matching_measurements %>%
        dplyr::filter(task == !!task)
    }
    
    # Filter by tracer if provided and column exists
    if (!is.na(trc) && trc != "" && "trc" %in% colnames(matching_measurements)) {
      matching_measurements <- matching_measurements %>%
        dplyr::filter(trc == !!trc)
    }
    
    # Filter by reconstruction if provided and column exists
    if (!is.na(rec) && rec != "" && "rec" %in% colnames(matching_measurements)) {
      matching_measurements <- matching_measurements %>%
        dplyr::filter(rec == !!rec)
    }
    
    # Filter by run if provided and column exists
    if (!is.na(run) && run != "" && "run" %in% colnames(matching_measurements)) {
      matching_measurements <- matching_measurements %>%
        dplyr::filter(run == !!run)
    }
    
    if (nrow(matching_measurements) == 0) {
      return(list(InjectedRadioactivity = NA_real_, InjectedRadioactivityUnits = NA_character_))
    }
    
    # Get the first matching measurement's PET info
    pet_info <- matching_measurements$petinfo[[1]]
    
    if (is.null(pet_info) || length(pet_info) == 0) {
      return(list(InjectedRadioactivity = NA_real_, InjectedRadioactivityUnits = NA_character_))
    }
    
    # Extract InjectedRadioactivity and units
    injected_radioactivity <- if ("InjectedRadioactivity" %in% names(pet_info)) {
      pet_info$InjectedRadioactivity
    } else {
      NA_real_
    }
    
    injected_radioactivity_units <- if ("InjectedRadioactivityUnits" %in% names(pet_info)) {
      pet_info$InjectedRadioactivityUnits
    } else {
      NA_character_
    }
    
    # Convert to kBq if units are different and radioactivity is not NA
    if (!is.na(injected_radioactivity) && !is.na(injected_radioactivity_units) && injected_radioactivity_units != "kBq") {
      tryCatch({
        injected_radioactivity <- kinfitr::unit_convert(injected_radioactivity, injected_radioactivity_units, "kBq")
        injected_radioactivity_units <- "kBq"
      }, error = function(e) {
        warning(paste("Could not convert injected radioactivity from", injected_radioactivity_units, "to kBq:", e$message))
      })
    }
    
    return(list(
      InjectedRadioactivity = injected_radioactivity,
      InjectedRadioactivityUnits = injected_radioactivity_units
    ))
    
  }, error = function(e) {
    warning(paste("Error parsing BIDS study for PET metadata:", e$message))
    return(list(InjectedRadioactivity = NA_real_, InjectedRadioactivityUnits = NA_character_))
  })
}

#' Create JSON Description File for Combined TACs
#'
#' @description Generate JSON metadata file describing columns in combined TACs file
#'
#' @param participants_metadata Participants metadata from participants.json (can be NULL)
#' @param injected_radioactivity_units Units for InjectedRadioactivity (can be NA)
#' @param output_dir Directory to save the JSON file
#' @return Path to created JSON file
#' @export
create_combined_tacs_json_description <- function(participants_metadata, injected_radioactivity_units, output_dir) {
  
  # Base column descriptions (following kinfitr BIDS pattern, excluding acq as it's deprecated)
  base_descriptions <- list(
    "sub" = list("Description" = "Subject identifier (numeric part only, without 'sub-' prefix)"),
    "ses" = list("Description" = "Session identifier (without 'ses-' prefix)"),
    "trc" = list("Description" = "Tracer identifier"),
    "rec" = list("Description" = "Reconstruction identifier"),
    "task" = list("Description" = "Task identifier"),
    "run" = list("Description" = "Run identifier"),
    "desc" = list("Description" = "Description identifier"),
    "pet" = list("Description" = "PET measurement identifier"),
    "bodyweight" = list("Description" = "Body weight of participant for SUV calculation", "Units" = "kg"),
    "region" = list("Description" = "Brain region name (combined from constituent regions)"),
    "volume_mm3" = list("Description" = "Total volume of combined regions", "Units" = "mm3"),
    "frame_start" = list("Description" = "Frame start time", "Units" = "seconds"),
    "frame_end" = list("Description" = "Frame end time", "Units" = "seconds"),
    "frame_dur" = list("Description" = "Frame duration", "Units" = "seconds"),
    "frame_mid" = list("Description" = "Frame midpoint time", "Units" = "seconds"),
    "TAC" = list("Description" = "Time Activity Curve value (volume-weighted average)")
  )
  
  # Add participant columns if available
  combined_descriptions <- base_descriptions
  if (!is.null(participants_metadata)) {
    # Add descriptions from participants.json (excluding participant_id)
    for (col_name in names(participants_metadata)) {
      if (col_name != "participant_id") {
        combined_descriptions[[col_name]] <- participants_metadata[[col_name]]
      }
    }
  }
  
  # Add InjectedRadioactivity description (standardized to kBq)
  injected_desc <- list(
    "Description" = "Injected radioactivity at time of injection (converted to kBq from original units if necessary)",
    "Units" = "kBq"
  )
  combined_descriptions[["InjectedRadioactivity"]] <- injected_desc
  
  # Write JSON file
  output_file <- file.path(output_dir, "desc-combinedregions_tacs.json")
  
  tryCatch({
    jsonlite::write_json(combined_descriptions, output_file, pretty = TRUE, auto_unbox = TRUE)
    cat("Created JSON description file:", output_file, "\n")
    return(output_file)
  }, error = function(e) {
    warning(paste("Error writing JSON description file:", e$message))
    return(NULL)
  })
}
