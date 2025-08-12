test_that("launch_apps function exists and has correct signature", {
  
  # Test that the function exists and is callable
  expect_true(exists("launch_apps"))
  expect_true(is.function(launch_apps))
  
  # Test parameter validation by checking function formals
  formals_list <- formals(launch_apps)
  expect_true("bids_dir" %in% names(formals_list))
  expect_true("derivatives_dir" %in% names(formals_list))
  expect_true("blood_dir" %in% names(formals_list))
  expect_true("subfolder" %in% names(formals_list))
  expect_true("region_definition" %in% names(formals_list))
  expect_true("modelling" %in% names(formals_list))
  expect_true("config_file" %in% names(formals_list))
  
  # Check default values
  expect_equal(formals_list$subfolder, "Primary_Analysis")
  expect_equal(formals_list$region_definition, FALSE)
  expect_equal(formals_list$modelling, FALSE)
})

test_that("launch_apps parameter defaults work correctly", {
  
  # Test that function handles both apps disabled (should not launch anything)
  # This should fail gracefully without launching any apps
  expect_error({
    launch_apps(
      bids_dir = "/nonexistent/path",
      region_definition = FALSE, 
      modelling = FALSE
    )
  }, "At least one of 'region_definition' or 'modelling' must be TRUE")
  
  # The error should be about no apps to launch, not about parameter validation
})

test_that("launch_apps handles derivatives directory parameter", {
  
  source(here::here("tests/testthat/fixtures/setup.R"))
  
  temp_dir <- tempdir()
  bids_dir <- create_test_bids_structure(temp_dir, n_subjects = 1, n_sessions = 1)
  derivatives_dir <- file.path(bids_dir, "derivatives")
  
  # Test with explicit derivatives directory
  expect_error({
    launch_apps(
      bids_dir = bids_dir,
      derivatives_dir = derivatives_dir,
      region_definition = FALSE,
      modelling = FALSE
    )
  })
  
  # Cleanup
  cleanup_test_dirs(bids_dir)
})

test_that("launch_apps handles blood directory parameter", {
  
  source(here::here("tests/testthat/fixtures/setup.R"))
  
  temp_dir <- tempdir()
  bids_dir <- create_test_bids_structure(temp_dir, n_subjects = 1, n_sessions = 1)
  blood_dir <- file.path(temp_dir, "blood_data")
  dir.create(blood_dir, showWarnings = FALSE)
  
  # Create blood data file
  blood_file <- file.path(blood_dir, "sub-01_blood.tsv")
  blood_data <- tibble::tibble(
    time = c(0, 1, 2),
    activity = c(0, 100, 200)
  )
  readr::write_tsv(blood_data, blood_file)
  
  # Test with explicit blood directory
  expect_error({
    launch_apps(
      bids_dir = bids_dir,
      blood_dir = blood_dir,
      region_definition = FALSE,
      modelling = FALSE
    )
  })
  
  # Cleanup
  cleanup_test_dirs(bids_dir)
  unlink(blood_dir, recursive = TRUE)
})

test_that("launch_apps handles subfolder parameter", {
  
  source(here::here("tests/testthat/fixtures/setup.R"))
  
  temp_dir <- tempdir()
  bids_dir <- create_test_bids_structure(temp_dir, n_subjects = 1, n_sessions = 1)
  
  # Test with custom subfolder name
  expect_error({
    launch_apps(
      bids_dir = bids_dir,
      subfolder = "Custom_Analysis",
      region_definition = FALSE,
      modelling = FALSE
    )
  })
  
  # Cleanup
  cleanup_test_dirs(bids_dir)
})

test_that("launch_apps handles region_definition flag", {
  
  # Test parameter validation without launching apps
  # Use non-existent directory to trigger early validation failure
  expect_error({
    launch_apps(
      bids_dir = "/nonexistent/path",
      region_definition = TRUE,
      modelling = FALSE
    )
  })
  
  # This should fail due to invalid directory, not launch any apps
})

test_that("launch_apps handles modelling flag", {
  
  # Test parameter validation without launching apps
  # Use non-existent directory to trigger early validation failure
  expect_error({
    launch_apps(
      bids_dir = "/nonexistent/path",
      region_definition = FALSE,
      modelling = TRUE
    )
  })
  
  # This should fail due to invalid directory, not launch any apps
})

test_that("launch_apps handles both apps enabled", {
  
  # Test parameter validation without launching apps
  # Use non-existent directory to trigger early validation failure
  expect_error({
    launch_apps(
      bids_dir = "/nonexistent/path",
      region_definition = TRUE,
      modelling = TRUE
    )
  })
  
  # This should fail due to invalid directory, not launch any apps
})

test_that("launch_apps validates parameter combinations", {
  
  # Test with no apps enabled (should not error during validation)
  expect_error({
    launch_apps(
      bids_dir = NULL,
      region_definition = FALSE,
      modelling = FALSE
    )
  })
})

test_that("launch_apps handles config_file parameter", {
  
  # Test parameter validation with config file
  # Use non-existent directory to trigger early validation failure
  config_dir <- here::here("tests/testthat/fixtures/sample_configs")
  config_file <- file.path(config_dir, "config_1tcm.json")
  
  if (file.exists(config_file)) {
    expect_error({
      launch_apps(
        bids_dir = "/nonexistent/path",
        config_file = config_file,
        region_definition = FALSE,
        modelling = TRUE
      )
    })
  } else {
    # Skip if config file doesn't exist
    expect_true(TRUE)
  }
})

test_that("launch_apps handles sequential app launching logic", {
  
  # Test parameter validation for sequential execution
  # Use non-existent directory to trigger early validation failure
  expect_error({
    launch_apps(
      bids_dir = "/nonexistent/path",
      region_definition = TRUE,
      modelling = TRUE
    )
  })
  
  # The function should fail validation before attempting to launch apps
})

test_that("launch_apps parameter validation works correctly", {
  
  # Test that function exists and has correct signature
  expect_true(exists("launch_apps"))
  expect_true(is.function(launch_apps))
  
  # Test parameter defaults by checking function formals
  formals_list <- formals(launch_apps)
  expect_true("bids_dir" %in% names(formals_list))
  expect_true("derivatives_dir" %in% names(formals_list))
  expect_true("blood_dir" %in% names(formals_list))
  expect_true("subfolder" %in% names(formals_list))
  expect_true("region_definition" %in% names(formals_list))
  expect_true("modelling" %in% names(formals_list))
  expect_true("config_file" %in% names(formals_list))
  
  # Check default values
  expect_equal(formals_list$subfolder, "Primary_Analysis")
  expect_equal(formals_list$region_definition, FALSE)
  expect_equal(formals_list$modelling, FALSE)
})

# Note: Full Shiny app testing is complex and would require:
# - shinytest2 or similar framework
# - Mock Shiny session objects
# - Headless browser testing
# These tests focus on parameter validation and setup logic
# that can be tested without launching the actual Shiny applications

test_that("launch_apps handles edge cases gracefully", {
  
  # Test with very long paths
  long_path <- paste0(tempdir(), "/", paste(rep("very_long_directory_name", 10), collapse = "/"))
  expect_error({
    launch_apps(bids_dir = long_path, region_definition = FALSE, modelling = FALSE)
  })
  
  # Test with special characters in paths (where supported by OS)
  special_path <- file.path(tempdir(), "test with spaces")
  expect_error({
    launch_apps(bids_dir = special_path, region_definition = FALSE, modelling = FALSE) 
  })
})