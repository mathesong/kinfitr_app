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
  
  # Convert region names to match TACs column naming (hyphens to dots)
  constituent_regions_tacs <- stringr::str_replace_all(constituent_regions, "-", ".")
  
  # Check which constituent regions are available in both datasets
  available_in_tacs <- constituent_regions[constituent_regions_tacs %in% colnames(tacs_data)]
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
  # Note: R converts "volume-mm3" to "volume.mm3" when reading TSV
  region_volumes <- morph_data %>%
    dplyr::filter(name %in% available_regions) %>%
    dplyr::select(name, volume.mm3)
  
  # Calculate total volume
  total_volume <- sum(region_volumes$volume.mm3)
  
  if (total_volume == 0) {
    # GRACEFUL: Return empty tibble instead of stopping
    warning(paste("Total volume is zero for", region_name, 
                  "- excluding from output"))
    return(tibble::tibble())
  }
  
  # Calculate volume fractions
  region_volumes <- region_volumes %>%
    dplyr::mutate(volume_fraction = volume.mm3 / total_volume)
  
  # Apply volume weighting to TACs data
  time_cols <- c("frame_start", "frame_end")
  
  # Initialize result with time columns
  combined_tac <- tacs_data %>%
    dplyr::select(dplyr::all_of(time_cols))
  
  # Calculate volume-weighted TAC for each time frame
  weighted_tac_values <- rep(0, nrow(tacs_data))
  
  for (region in available_regions) {
    volume_frac <- region_volumes$volume_fraction[region_volumes$name == region]
    # Convert region name to TACs column format (hyphens to dots)
    region_tacs_col <- stringr::str_replace_all(region, "-", ".")
    region_tac <- tacs_data[[region_tacs_col]]
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

#' Create Kinfitr Regions Files Mapping
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
  regions_config <- utils::read.table(kinfitr_regions_file, sep = "\t", 
                                     header = TRUE, stringsAsFactors = FALSE)
  
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
    all_tacs_files <- all_tacs_files[!grepl("desc-combined_tacs\\.tsv$", all_tacs_files)]
    
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
        dplyr::select(sub, ses, rec, task, run, desc, hemi, pvc, path)
      
      # Match files where ALL non-identifier attributes are identical
      # Individual identifiers to exclude from matching: sub, ses, rec, task, run, pet
      identifier_cols <- c("sub", "ses", "rec", "task", "run", "pet")
      
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
  
  utils::write.table(regions_files, output_file, sep = "\t", 
                    row.names = FALSE, quote = FALSE)
  
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
    utils::read.table(tacs_full_path, sep = "\t", header = TRUE, stringsAsFactors = FALSE)
  }, error = function(e) {
    warning(paste("Error reading TACs file:", e$message, "- skipping"))
    return(NULL)
  })
  
  if (is.null(tacs_data)) {
    return(tibble::tibble())
  }
  
  morph_data <- tryCatch({
    utils::read.table(morph_full_path, sep = "\t", header = TRUE, stringsAsFactors = FALSE)
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

#' Process All Kinfitr Regions Across All Files
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
  regions_files <- utils::read.table(kinfitr_regions_files_path, sep = "\t", 
                                    header = TRUE, stringsAsFactors = FALSE)
  
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
        utils::write.table(combined_results, output_path, sep = "\t", 
                          row.names = FALSE, quote = FALSE)
        
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
#' @return Tibble with BIDS attributes (sub, ses, rec, task, run, desc, pet)
#' @export
extract_bids_attributes_from_filename <- function(filename) {
  # Parse filename to extract BIDS key-value pairs
  # Required: sub, ses, rec, task, run, desc, pet
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
  required_keys <- c("sub", "ses", "rec", "task", "run", "desc")
  other_attributes <- attributes[!names(attributes) %in% required_keys]
  
  for (key in names(other_attributes)) {
    result[[key]] <- other_attributes[[key]]
  }
  
  return(result)
}

#' Create Consolidated Kinfitr Combined TACs Output
#'
#' @description Create single consolidated TSV with BIDS attributes and long-format TACs
#'
#' @param kinfitr_regions_files_path Path to kinfitr_regions_files.tsv
#' @param derivatives_folder Base path to derivatives folder
#' @param output_dir Where to save consolidated combined TACs file
#' @return Tibble with all combined TACs data in long format with BIDS attributes
#' @export
create_kinfitr_combined_tacs <- function(kinfitr_regions_files_path, derivatives_folder, output_dir) {
  
  # Validate inputs
  if (!file.exists(kinfitr_regions_files_path)) {
    stop(paste("kinfitr_regions_files.tsv not found:", kinfitr_regions_files_path))
  }
  
  if (!dir.exists(derivatives_folder)) {
    stop(paste("Derivatives folder not found:", derivatives_folder))
  }
  
  # Create output folder if it doesn't exist
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
    cat("Created output folder:", output_dir, "\n")
  }
  
  # Read regions files mapping
  regions_files <- utils::read.table(kinfitr_regions_files_path, sep = "\t", 
                                    header = TRUE, stringsAsFactors = FALSE)
  
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
    
    # Add BIDS attributes to each row (including pet column)
    combined_results_with_bids <- combined_results %>%
      dplyr::mutate(
        sub = bids_attributes$sub,
        ses = bids_attributes$ses, 
        rec = bids_attributes$rec,
        task = bids_attributes$task,
        run = bids_attributes$run,
        desc = bids_attributes$desc,
        pet = bids_attributes$pet
      ) %>%
      # Reorder columns to match bloodstream pattern (pet before region)
      dplyr::select(sub, ses, rec, task, run, desc, pet, region = name, frame_start, frame_end, frame_dur, frame_mid, TAC, volume_mm3 = `volume-mm3`) %>%
      # Convert BIDS identifier columns to character (following bloodstream pattern)
      dplyr::mutate(across(c(sub, ses, rec, task, run, desc, pet), as.character))
    
    return(combined_results_with_bids)
  })
  
  if (nrow(all_combined_data) == 0) {
    warning("No regions were successfully combined across all files")
    return(tibble::tibble())
  }
  
  # Save single consolidated TSV
  output_file <- file.path(output_dir, "desc-combined_tacs.tsv")
  utils::write.table(all_combined_data, output_file, sep = "\t", 
                    row.names = FALSE, quote = FALSE)
  
  cat("\n=== Consolidated Output Summary ===\n")
  cat("Total rows:", nrow(all_combined_data), "\n")
  cat("Total regions:", length(unique(all_combined_data$region)), "\n")
  cat("Unique subjects:", length(unique(all_combined_data$sub)), "\n")
  cat("Unique sessions:", length(unique(all_combined_data$ses)), "\n")
  cat("Output file:", output_file, "\n")
  
  return(all_combined_data)
}