#!/usr/bin/env Rscript
# Test script for new seg/label-based matching system

library(tidyverse)

# Load the package
devtools::load_all()

# Test with KaSP_UCBJ_BIDS_mini dataset
derivatives_folder <- "/home/granville/Repositories/OpenNeuro/KaSP_UCBJ_BIDS_mini/derivatives/petprep"

cat("Testing create_tacs_morph_mapping() with KaSP_UCBJ_BIDS_mini dataset\n")
cat("==================================================================\n\n")

# Test the mapping function
mapping <- create_tacs_morph_mapping(derivatives_folder)

cat("Mapping Results:\n")
cat("----------------\n")
print(mapping, n = Inf)

cat("\n\nExpected Matches:\n")
cat("-----------------\n")
cat("For sub-P3:\n")
cat("  - sub-P3_pvc-AGTM_desc-preproc_seg-gtm_tacs.tsv → sub-P3_desc-preproc_seg-gtm_morph.tsv\n")
cat("  - sub-P3_desc-preproc_seg-gtm_tacs.tsv → sub-P3_desc-preproc_seg-gtm_morph.tsv\n")
cat("  - sub-P3_desc-preproc_seg-wm_tacs.tsv → sub-P3_desc-preproc_seg-wm_morph.tsv\n")
cat("  - sub-P3_label-semiovale_desc-preproc_tacs.tsv → sub-P3_label-semiovale_desc-ref_morph.tsv\n")
cat("\nSame pattern for sub-P5 and sub-C1\n")

cat("\n\nVerifying matches:\n")
cat("------------------\n")
for (i in seq_len(nrow(mapping))) {
  tacs_base <- basename(mapping$tacs_path[i])
  morph_base <- if (!is.na(mapping$morph_path[i])) basename(mapping$morph_path[i]) else "NO MATCH"
  cat(sprintf("%s → %s\n", tacs_base, morph_base))
}

cat("\n\nTesting volume extraction:\n")
cat("--------------------------\n")
# Test with a valid morph file
test_morph <- file.path(derivatives_folder, "sub-P3/anat/sub-P3_desc-preproc_seg-gtm_morph.tsv")
morph_data <- get_region_volumes_from_morph(test_morph)
if (!is.null(morph_data)) {
  cat("✓ Successfully read morph file\n")
  cat(sprintf("  Regions found: %d\n", nrow(morph_data)))
  cat(sprintf("  Columns: %s\n", paste(colnames(morph_data), collapse=", ")))
} else {
  cat("✗ Failed to read morph file\n")
}

# Test with NULL (missing morph)
cat("\nTesting volume=1 fallback:\n")
morph_data_null <- get_region_volumes_from_morph(NULL)
if (is.null(morph_data_null)) {
  cat("✓ Correctly returns NULL for missing morph file\n")
}

cat("\n\nTest complete!\n")
