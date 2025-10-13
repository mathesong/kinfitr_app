#!/usr/bin/env Rscript
# Test region combination with and without morph data

library(tidyverse)
devtools::load_all()

derivatives_folder <- "/home/granville/Repositories/OpenNeuro/KaSP_UCBJ_BIDS_mini/derivatives/petprep"

cat("Testing Region Combination\n")
cat("==========================\n\n")

# Get mapping
mapping <- create_tacs_morph_mapping(derivatives_folder)

# Test case 1: With morph data (seg-gtm)
cat("Test 1: Combination WITH morph data (seg-gtm)\n")
cat("----------------------------------------------\n")
test1_tacs <- mapping$tacs_path[grep("sub-P3.*seg-gtm_tacs", mapping$tacs_path)][1]
test1_morph <- mapping$morph_path[grep("sub-P3.*seg-gtm_tacs", mapping$tacs_path)][1]

cat(sprintf("TACs file: %s\n", basename(test1_tacs)))
cat(sprintf("Morph file: %s\n", basename(test1_morph)))

# Read files
tacs_data <- readr::read_tsv(test1_tacs, show_col_types = FALSE)
morph_data <- get_region_volumes_from_morph(test1_morph)

# Get some regions to combine
regions_list <- colnames(tacs_data)[!colnames(tacs_data) %in% c("frame_start", "frame_end", "frame_dur", "frame_mid")]
test_regions <- head(regions_list, 3)

cat(sprintf("\nCombining regions: %s\n", paste(test_regions, collapse=", ")))

combined_tac <- combine_single_region_tac(
  tacs_data = tacs_data,
  morph_data = morph_data,
  constituent_regions = test_regions,
  region_name = "Test_Region"
)

cat(sprintf("✓ Successfully combined %d regions\n", length(test_regions)))
cat(sprintf("  Output has %d rows (frames)\n", nrow(combined_tac)))
cat(sprintf("  Volume: %.1f mm³\n", combined_tac$`volume-mm3`[1]))

# Test case 2: Without morph data (volume=1 fallback)
cat("\n\nTest 2: Combination WITHOUT morph data (volume=1 fallback)\n")
cat("----------------------------------------------------------\n")
test2_tacs <- mapping$tacs_path[grep("sub-P5.*seg-wm_tacs", mapping$tacs_path)][1]

cat(sprintf("TACs file: %s\n", basename(test2_tacs)))
cat("Morph file: NULL (simulating missing morph)\n")

tacs_data2 <- readr::read_tsv(test2_tacs, show_col_types = FALSE)
regions_list2 <- colnames(tacs_data2)[!colnames(tacs_data2) %in% c("frame_start", "frame_end", "frame_dur", "frame_mid")]
test_regions2 <- head(regions_list2, 3)

cat(sprintf("\nCombining regions: %s\n", paste(test_regions2, collapse=", ")))

combined_tac2 <- combine_single_region_tac(
  tacs_data = tacs_data2,
  morph_data = NULL,  # Simulate missing morph
  constituent_regions = test_regions2,
  region_name = "Test_Region2"
)

cat(sprintf("✓ Successfully combined %d regions with volume=1 fallback\n", length(test_regions2)))
cat(sprintf("  Output has %d rows (frames)\n", nrow(combined_tac2)))
cat(sprintf("  Volume: %.1f mm³ (sum of equal weights)\n", combined_tac2$`volume-mm3`[1]))

cat("\n\nAll tests passed!\n")
