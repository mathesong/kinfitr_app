# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

This will include an R Shiny web application that creates customized kinfitr BIDS App configuration files for PET imaging analysis, which will be within a docker container. It will also include parameterised reports which will run using the parameters in the configuration files. The app will provide a user interface for configuring kinetic modeling parameters for Time Activity Curves (TACs).

The system supports multiple usage modes:
1. **Non-interactive processing**: Run kinetic modelling using pre-existing .json configuration files without GUI
2. **GUI-assisted setup**: Use the Shiny apps to create configuration files, then run processing
3. **Interactive exploration**: Use the modelling app's interactive tab to test model fits on individual TACs

The two apps work sequentially but independently:
- **Region Definition App**: Creates brain region definitions and combined TACs
- **Modelling App**: Configures kinetic models and creates analysis configurations
  - Includes an interactive tab for testing model fits on individual PET measurements and regions
  - Allows users to validate model specifications and parameter bounds before full processing

## Commands

### Running the Application
```r
# Load the package and launch apps
library(kinfitrapp)

# Launch both apps sequentially
launch_apps(bids_dir = "/path/to/bids", region_definition = TRUE, modelling = TRUE)

# Or launch individual apps
region_definition_app(bids_dir = "/path/to/bids")
modelling_app(bids_dir = "/path/to/bids")
```

### Docker Usage (Planned)
Docker integration will support:
- Running processing without GUI using existing configurations
- Launching apps for configuration setup
- Command options for specifying folder locations and processing modes

```bash
# Docker setup is planned for future implementation
docker-compose up
```

### Development
- This is a proper R package with DESCRIPTION file and roxygen documentation
- Two separate Shiny apps: `region_definition_app.R` and `modelling_app.R` in R/ directory
- Parameterised reports are Rmd files located in templates/ folder
- Dependencies managed through DESCRIPTION file
- Modular code structure with separate files for utilities, validation, and UI modules

## Architecture

### Application Structure
- **Two-app system**: 
  - `region_definition_app.R`: For defining brain regions and creating combined TACs
  - `modelling_app.R`: For kinetic model configuration and analysis
- **Package Structure**: Proper R package with R/, man/, data/, and templates/ directories
- **UI Layout**: Both apps use `fluidPage` with sidebar layout
- **Server Logic**: Reactive expressions generate JSON configurations and process data
- **Launcher Function**: `launch_apps()` can run either or both apps sequentially

### Directory Structure
The system uses a standard BIDS (Brain Imaging Data Structure) directory layout:

- **bids_dir**: Root BIDS directory containing raw data and standard BIDS structure
  - `code/kinfitr/`: Contains region configuration files (`kinfitr_regions.tsv`)
  - Standard BIDS subject/session structure for source data
  
- **derivatives_dir**: Processed data following BIDS derivatives convention
  - Default: `{bids_dir}/derivatives/` when bids_dir is provided
  - Can be specified independently to any location for flexibility
  - Contains all processed outputs and analysis results
  
- **kinfitr folder**: `{derivatives_dir}/kinfitr/` - Contains shared kinfitr resources
  - `desc-combinedregions_tacs.tsv`: Combined TACs file from region definition app
  - Shared across all analyses and accessed by modelling app
  
- **analysis folder**: `{derivatives_dir}/kinfitr/{subfolder}/` - Analysis-specific outputs
  - Individual TACs files created by modelling app subsetting
  - Configuration files for specific analyses (e.g., `desc-kinfitroptions_config.json`)
  - Default subfolder: "Primary_Analysis"
  - Each analysis gets its own subfolder to keep configurations separate

### Key Dependencies
- `shiny`: Core web application framework
- `shinythemes`: UI theming
- `bslib`: Bootstrap components
- `jsonlite`: JSON generation for config files
- `kinfitr`: Core kinetic modeling functionality
- `readr`: Robust file reading/writing (replaces base R read.table/write.table)
- Data manipulation: `dplyr`, `tibble`, `purrr`, `stringr`, `tidyr`
- Other utilities: `glue`, `magrittr`, `later`

### Core Functionality
1. **Region Configuration**: Define brain regions and segmentation parameters
2. **BIDS Integration**: Automatic integration of participant data and PET metadata into TACs files
3. **Data Subsetting**: Filter by subject, session, tracer, etc.
4. **Weights Definition**: Create weights for modelling
5. **Fit delay**: Estimate the delay between the blood and tissue curves
6. **Model Selection**: Choose between kinetic models (1TCM, 2TCM, Logan, etc.) for 3 models for comparison of outcomes
7. **Parameter Configuration**: Set model-specific parameters and bounds
8. **Config Generation**: Create JSON configuration files for downstream analysis
9. **Interactive modelling:** Test model fits on individual TACs to validate specifications before full processing
10. **State Persistence**: Automatically save and restore app configuration for seamless workflow continuation


### File Management
- Generates config files: `desc-kinfitroptions_config.json` in analysis folder
- **Combined TACs Files**: `desc-combinedregions_tacs.tsv` with integrated BIDS metadata
- **Individual TACs Files**: Created by modelling app with `desc-combinedregions` naming convention
- **State Persistence**: App automatically saves and restores configuration
  - On startup: Checks for existing config file in analysis folder
  - If found: Restores all UI inputs to previous state with user notification
  - If corrupted: Shows error message and uses defaults
  - On actions: Saves current state before executing operations

### Data Integration and Processing
**BIDS Participant Data Integration**: The system automatically integrates participant demographics and PET metadata into combined TACs files:

1. **Participant Data Loading**: 
   - Reads `participants.tsv` and `participants.json` files from BIDS root
   - Transforms participant_id format (sub-01 → 01) for consistency
   - Maps participant weight to `bodyweight` column for SUV calculations

2. **PET Metadata Extraction**:
   - Uses `kinfitr::bids_parse_study()` for robust BIDS parsing
   - Extracts `InjectedRadioactivity` from PET JSON sidecars
   - Automatically converts radioactivity units to kBq using `kinfitr::unit_convert()`

3. **Combined TACs Structure**:
   **Column order**: `sub, ses, trc, rec, task, run, desc, pet, InjectedRadioactivity, bodyweight, [participant_columns], region, volume_mm3, frame_start, frame_end, frame_dur, frame_mid, TAC`
   
   *Note: Participant columns (e.g., age, sex) are inserted after bodyweight when available from participants.tsv*
   
   - **BIDS identifiers**: sub, ses, trc, rec, task, run, desc, pet (preserved as character types)
   - **Metabolic data**: InjectedRadioactivity (kBq), bodyweight (kg for SUV calculations)  
   - **Participant data**: Optional columns from participants.tsv (e.g., age, sex, weight) - weight mapped to bodyweight
   - **Region data**: region (combined region name), volume_mm3 (total volume)
   - **Time series**: frame_start, frame_end, frame_dur, frame_mid, TAC (volume-weighted average)

4. **Individual Analysis Files**:
   **Exact column order**: `region, volume_mm3, InjectedRadioactivity, bodyweight, frame_start, frame_end, frame_dur, frame_mid, TAC`
   
   - Created by "Create Analysis Data" button in modelling app
   - Essential kinetic modeling metadata positioned after volume_mm3, before frame timing
   - Use `desc-combinedregions` naming convention (not `desc-combinedtacs`)
   - Filename pattern: `{pet_id}_desc-combinedregions_tacs.tsv`

### File I/O Standards
**CRITICAL**: Use appropriate packages for different file types to ensure robust data handling:

#### Tabular Data (.tsv, .csv files)
- **Reading**: Use `readr::read_tsv()` instead of `read.table()`
  - Preserves character data types (prevents "01" → 1 conversion)
  - Handles various encoding and formatting issues robustly
  - Use `show_col_types = FALSE` to suppress column type messages

- **Writing**: Use `readr::write_tsv()` instead of `write.table()`
  - Consistent tab-separated output formatting
  - Proper handling of special characters and encoding

#### JSON Data (.json files)
- **Reading/Writing**: Use `jsonlite` functions as usual
  - `jsonlite::read_json()` and `jsonlite::write_json()`
  - `jsonlite::fromJSON()` and `jsonlite::toJSON()`

**Example patterns:**
```r
# Reading/writing TSV files
data <- readr::read_tsv(file_path, show_col_types = FALSE)
readr::write_tsv(data, output_path)

# Reading/writing JSON files
config <- jsonlite::read_json(json_path)
jsonlite::write_json(config, output_path, pretty = TRUE, auto_unbox = TRUE)
```

This ensures consistent, reliable file operations and prevents data type conversion issues with tabular data while maintaining proper JSON handling.

### Critical Implementation Details

#### File I/O Migration Issues
**IMPORTANT**: When migrating from `read.table()` to `readr::read_tsv()`, several column name handling assumptions must be updated:

1. **Column Name Preservation**: 
   - `read.table()` sometimes converts hyphens to dots (`"Left-Accumbens-area"` → `"Left.Accumbens.area"`)
   - `readr::read_tsv()` preserves original column names with hyphens
   - **Fix**: Remove any hyphen-to-dot conversion logic in region matching

2. **Morph File Columns**:
   - Old assumption: `volume.mm3` (with dot)  
   - Reality with readr: `volume-mm3` (with hyphen)
   - **Fix**: Use backticks for non-standard column names: `select(name, \`volume-mm3\`)`

3. **Character Type Preservation**:
   - `readr::read_tsv()` automatically maintains character types for BIDS identifiers
   - Subject IDs stay as "01", "02", "03" instead of converting to numeric 1, 2, 3
   - No additional `colClasses` specification needed

#### Common Migration Pitfalls
These specific code patterns needed fixing during the readr migration:

```r
# WRONG (old read.table assumptions)
constituent_regions_tacs <- str_replace_all(constituent_regions, "-", ".")
available_in_tacs <- constituent_regions[constituent_regions_tacs %in% colnames(tacs_data)]
region_volumes <- morph_data %>% select(name, volume.mm3)

# CORRECT (readr-compatible)  
available_in_tacs <- constituent_regions[constituent_regions %in% colnames(tacs_data)]
region_volumes <- morph_data %>% select(name, `volume-mm3`)
```

### Configuration Management
**CRITICAL DESIGN PRINCIPLE**: When adding new functionality to the modelling app, always ensure backward compatibility with existing JSON configuration files.

**Requirements for new features:**
1. **Safe Loading**: Use null coalescing (`%||%`) when accessing new config properties
2. **Default Values**: Provide sensible defaults for missing configuration sections
3. **Error Handling**: Gracefully handle missing or invalid configuration data
4. **User Feedback**: Inform users when config loading fails or succeeds
5. **State Restoration**: Add UI update logic for any new input fields

**Example pattern for new features:**
```r
# Safe restoration of new feature
if (!is.null(existing_config$NewFeature)) {
  updateTextInput(session, "new_input", value = existing_config$NewFeature$parameter %||% "default")
}
```

This ensures users can seamlessly continue work with existing configurations even after app updates.

### Model Types Supported
- **1TCM**: Single tissue compartment model with K1, k2, vB parameters
- **2TCM**: Two tissue compartment model with K1, k2, k3, k4, vB parameters  
- **Logan**: Logan graphical analysis with t* parameter

This will eventually contain more models


### UI Components
- Conditional panels that show/hide based on selected model
- Numeric inputs with validation (min/max bounds, step sizes)
- Text inputs for region names and filtering criteria
- Checkbox inputs for optional parameter fitting

## Troubleshooting

### Combined TACs File Not Generated
**Symptom**: `modelling_app()` processes files but doesn't create `desc-combinedregions_tacs.tsv`

**Root Cause**: Region matching failures due to column name handling inconsistencies

**Diagnostic Steps**:
1. Check for warnings about "No constituent regions found" or "Failed to combine region")
2. Verify TACs file column names preserve hyphens: `Left-Accumbens-area` (not `Left.Accumbens.area`)
3. Verify morph file column names: `volume-mm3` (not `volume.mm3`)

**Solutions**:
- Ensure no hyphen-to-dot conversion in region matching code
- Use backticks for non-standard column names: `select(name, \`volume-mm3\`)`
- Verify `readr::read_tsv()` is used consistently (not `read.table()`)

### Character Type Conversion Issues  
**Symptom**: Subject IDs appear as numeric (1, 2, 3) instead of character ("01", "02", "03")

**Root Cause**: Base R `read.table()` auto-converts character columns to numeric

**Solution**: Replace all `read.table()` calls with `readr::read_tsv()` which preserves character types

### Data Compatibility Errors
**Symptom**: "Assigned data must be compatible with existing data" errors during region combination

**Root Cause**: Column name mismatches preventing proper data joins/assignments

**Solution**: Ensure consistent column naming throughout the pipeline - no automatic conversions
