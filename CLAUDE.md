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
- Parameterised reports are Rmd files located in inst/rmd/ folder (following R package convention)
- Dependencies managed through DESCRIPTION file
- Modular code structure with separate files for utilities, validation, and UI modules

## Architecture

### Application Structure
- **Two-app system**: 
  - `region_definition_app.R`: For defining brain regions and creating combined TACs
  - `modelling_app.R`: For kinetic model configuration and analysis
- **Package Structure**: Proper R package with R/, man/, data/, and inst/rmd/ directories
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
  - `desc-combinedregions_tacs.tsv`: Combined TACs file from region definition app with seg_meanTAC column
  - Contains volume-weighted mean TAC for entire segmentations (seg_meanTAC column)
  - Shared across all analyses and accessed by modelling app
  
- **analysis folder**: `{derivatives_dir}/kinfitr/{subfolder}/` - Analysis-specific outputs
  - Individual TACs files created by modelling app subsetting
  - Configuration files for specific analyses (e.g., `desc-kinfitroptions_config.json`)
  - **reports/** subfolder: Contains parameterised HTML reports for each analysis step
  - Default subfolder: "Primary_Analysis"
  - Each analysis gets its own subfolder to keep configurations separate

### Key Dependencies
- `shiny`: Core web application framework
- `shinythemes`: UI theming
- `bslib`: Bootstrap components
- `jsonlite`: JSON generation for config files
- `kinfitr`: Core kinetic modeling functionality
- `readr`: Robust file reading/writing (replaces base R read.table/write.table)
- `rmarkdown`: Parameterised report generation
- `knitr`: Dynamic document generation for reports
- **Data manipulation**: `tidyverse` ecosystem (`dplyr`, `tibble`, `purrr`, `stringr`, `tidyr`) - preferred over base R equivalents
- Plotting: `ggplot2` (for report visualizations)
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
11. **Parameterised Reports**: Automatically generate HTML reports for each analysis step for quality control and review
12. **Segmentation Mean TACs**: Pre-calculated volume-weighted mean TACs for external segmentations to avoid BIDS directory access during weights calculation

### Parameterised Reports System

The kinfitr app includes a comprehensive parameterised reporting system that automatically generates HTML reports for each analysis step. These reports are designed for quality control, data visualization, and workflow documentation.

#### Report Structure
- **Location**: Reports are generated in `{analysis_folder}/reports/`
- **Format**: HTML documents with embedded plots and tables
- **Template Source**: R Markdown templates in `inst/rmd/` folder
- **Generation**: Automatic generation after each analysis step completion

#### Available Report Templates

**Step-Based Reports:**
- `data_definition_report.Rmd` → `data_definition_report.html`
- `weights_report.Rmd` → `weights_report.html`  
- `delay_report.Rmd` → `delay_report.html`

**Model-Specific Reports:**
- `1tcm_report.Rmd` → Used for 1TCM model fitting
- `2tcm_report.Rmd` → Used for 2TCM model fitting
- `logan_report.Rmd` → Used for Logan analysis
- `fit_delay_report.Rmd` → Used for delay-fitting models

**Procedure Reports:**
- `tstar_finder_report.Rmd` → `tstar_finder_report.html` (for future t* finder tab)

#### Dynamic Template Selection

The system uses **dynamic template selection** for model reports:
- Template chosen based on user's model selection (`input$button`, `input$button2`, `input$button3`)
- Output files always named consistently: `model1_report.html`, `model2_report.html`, `model3_report.html`
- Same template can generate different numbered reports depending on user choices

**Example**: If user selects "2TCM" for Model 1 and "Logan" for Model 2:
- `2tcm_report.Rmd` generates `model1_report.html`
- `logan_report.Rmd` generates `model2_report.html`

#### Report Generation Functions

Located in `R/report_generation.R`:

- `generate_step_report()`: Creates step-based reports (data definition, weights, delay)
- `generate_model_report()`: Creates model-specific reports with dynamic template selection
- `generate_tstar_report()`: Creates t* finder analysis reports
- `get_model_template()`: Maps model types to appropriate template files
- `generate_reports_summary()`: Creates summary page linking all generated reports

#### Integration with Shiny App

Reports are automatically generated by button handlers in `modelling_app.R`:
- **Data Definition**: Generated after "Create Analysis Data" button execution
- **Weights/Delay**: Generated after respective button executions  
- **Model Reports**: Generated after "Fit Model X" button executions
- **User Notifications**: Success messages inform users when reports are generated

#### Report Content and Purpose

**Primary Purpose**: Quality control and data visualization for user review

**Standard Content**:
- Analysis configuration summary
- Data processing statistics  
- Visualization plots (TACs, fits, diagnostics)
- Quality control metrics
- Parameter estimates and uncertainties
- Next steps recommendations
- Session information

**Key Dependencies**: 
- `rmarkdown`: Report rendering engine
- `knitr`: Dynamic document generation
- Standard plotting libraries (`ggplot2`, etc.)

#### Interactive Plotly Reports

The kinfitr system supports advanced interactive reports with plotly visualizations that provide rich, explorable data views.

**IMPORTANT**: Reports are not just passive summaries - they perform the actual computational work. The R Markdown templates contain the core analysis logic for each step (weights calculation, delay fitting, model fitting, etc.), with users able to see exactly how calculations are performed. This design ensures complete transparency and reproducibility of all analysis steps.

##### Technical Requirements and Dependencies

**Core Interactive Libraries:**
- `plotly`: Interactive plotting engine for web-based visualizations
- `crosstalk`: Cross-widget interaction and filtering capabilities  
- `htmltools`: HTML rendering and DOM manipulation for R Markdown

**Critical Rendering Patterns:**
```r
# CORRECT: Use htmltools::tagList() for plotly object lists
htmltools::tagList(plot_list)

# WRONG: Direct printing fails in HTML output
plot_list  # Results in [[1]], [[2]] instead of plots
```

##### Plotly Dimension and Spacing Control

**Dimension Management:**
```r
# CORRECT: Set dimensions in plotly layout (overrides CSS)
ggplotly(p) %>%
  layout(
    width = 800,
    height = 500,
    # other layout options...
  )

# WRONG: CSS dimensions are unreliable for plotly
htmltools::div(plot, style="width: 800px; height: 500px;")  # May be ignored
```

**Spacing Between Plots:**
```r
# CORRECT: CSS margins for spacing between multiple plots
htmltools::tagList(map(plots, ~htmltools::div(.x, style="margin: 20px 0 50px 0;")))

# Spacing explanation:
# - 20px top margin (separation from text above)
# - 50px bottom margin (separation from next plot)  
# - 0 left/right margins (centered alignment)
```

##### Interactive Plot Features

**Standard Interactive Elements:**
- **Cross-filtering**: Hover to highlight, double-click to reset using `crosstalk`
- **Axis Scaling**: Dropdown menus for linear/log combinations on both axes
- **Hover Tooltips**: Context-specific information via `tooltip` parameter
- **SharedData**: Use `crosstalk::SharedData$new()` for cross-plot interactions

**Example Interactive Pattern:**
```r
plots <- data %>% 
  group_by(grouping_var) %>% 
  nest() %>% 
  mutate(
    plot = map2(grouping_var, data, ~{
      shared_data <- SharedData$new(.y, ~identifier)
      
      p <- ggplot(shared_data, aes(...)) + geom_*()
      
      ggplotly(p) %>%
        highlight(on = "plotly_hover", off = "plotly_doubleclick", opacityDim = 0.2) %>%
        layout(width = 800, height = 500)
    })
  ) %>% 
  pull(plot)

htmltools::tagList(map(plots, ~htmltools::div(.x, style="margin: 20px 0 50px 0;")))
```

##### Advanced R Markdown Patterns

**Conditional Chunk Evaluation:**
```r
# Dynamic chunk execution based on conditions
```{r, eval=condition_variable, echo=condition_variable}
# Code only runs when condition_variable is TRUE
```

**Dynamic Content Generation:**
```r
```{r, echo=FALSE}
#| results: asis

if(condition) {
  str_glue("Dynamic text based on data characteristics")
}
```

##### User Experience Best Practices

**Progressive Notification System:**
```r
# CORRECT: Progressive feedback for long operations
showNotification("Generating report...", duration = NULL, id = "generating_report")
# ... perform work ...
removeNotification(id = "generating_report")
showNotification("Report generated successfully", duration = 5)

# WRONG: Technical implementation details in notifications
showNotification("Created 15 files in analysis folder", ...)  # Too technical
```

**Professional Report Presentation:**
- **Table of Contents**: `toc: true, toc_depth: 2` in YAML header
- **Code Folding**: `code_folding: hide` to show results while hiding implementation
- **Timestamps**: `format(Sys.time(), "%Y-%m-%d %H:%M:%S")` for generation tracking
- **Session Info**: `sessionInfo()` for complete reproducibility

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
   **Exact column order**: `pet, region, volume_mm3, InjectedRadioactivity, bodyweight, frame_start, frame_end, frame_dur, frame_mid, TAC`
   
   - Created by "Create Analysis Data" button in modelling app
   - **pet column first**: Essential for data tracking and analysis identification
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
- **Critical JSON Formatting**: Always use `auto_unbox = TRUE` when writing JSON configuration files
  - Prevents single values from being wrapped in arrays: `"mean_combined"` instead of `["mean_combined"]`
  - Essential for proper configuration loading and template processing

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

### Delay Fitting System Architecture

**Blood Data Detection and Status Display**: The system provides Docker-compatible blood data detection with clean visual feedback:

#### Blood Data Detection Logic
The system detects blood data files using the pattern `"_(blood|inputfunction)\\.tsv$"` to find both:
- `*_blood.tsv` files (raw BIDS blood data)
- `*_inputfunction.tsv` files (processed input functions)

#### Status Display Scenarios

| Scenario | bids_dir | blood_dir | Blood Files Found | Display |
|----------|----------|-----------|-------------------|---------|
| **Explicit blood directory provided** | Any | ✓ Provided | ✓ Found | ✅ **Green tick**: "Blood data found" + file count |
| **Explicit blood directory provided** | Any | ✓ Provided | ✗ Not found | ❌ **Red cross**: "No blood data found in blood_dir" |
| **BIDS directory only** | ✓ Provided | ✗ Not provided | ✓ Found | ✅ **Green tick**: "Blood data found" + recommendation |
| **BIDS directory only** | ✓ Provided | ✗ Not provided | ✗ Not found | ❌ **Red cross**: "No blood data found in bids_dir" |
| **No directories provided** | ✗ Not provided | ✗ Not provided | N/A | ⚠️ **Warning**: "No blood data available" |

#### Key Features
- **Docker-compatible**: No directory paths shown (problematic in containers where paths may differ)
- **Comprehensive detection**: Handles both raw blood data and processed input functions
- **Visual feedback**: Clear green tick (✓) for success, red cross (✗) for missing data
- **Consistent logic**: Same detection used for both UI display and processing validation
- **User-friendly messaging**: File counts and helpful recommendations without technical details

#### Implementation Details
- Status display function: `delay_blood_status_display` (modelling_app.R:778-843)
- Processing validation: `run_delay` event handler (modelling_app.R:1148-1169)
- Helper function: `check_blood_files()` for consistent file detection logic

**Delay Estimation Approaches**: Comprehensive set of delay estimation methods ordered by computational speed:

- `"Set to zero (i.e. no delay fitting to be performed)"` - Skip delay estimation entirely
- `"1TCM Delay from Single Representative TAC (Quick)"` - Single region approach
- `"2TCM Delay from Single Representative TAC (Less Quick)"` - Complex model, single region  
- `"1TCM Median Delay from Multiple Regions (Recommended, Slow)"` - **Default**, multiple region analysis
- `"2TCM Median Delay from Multiple Regions (Very Slow)"` - Most comprehensive approach

**Note**: Linear 2TCM Profile method is commented out in the code for future implementation.

**Multiple Regions Analysis**: For "Multiple Regions" approaches:
- **Optional Region Subsetting**: `delay_multiple_regions` field allows semicolon-separated region specification
- **Default Behavior**: Leave blank to analyze all available regions
- **Conditional UI**: Text input only appears when multiple regions approaches are selected

**Configuration Fields**: FitDelay section includes:
- `blood_source`: Selected blood data source
- `model`: Delay estimation approach (default: `"1tcm_median"`)
- `time_window`: Minutes of data for fitting (default: 5, recommended for early phase sensitivity)
- `regions`: General region selection (legacy)
- `multiple_regions`: Region subset for multiple regions approaches
- `vB_value`, `fit_vB`, `use_weights`: Parameter settings
- `inpshift_lower`, `inpshift_upper`: Blood input time shift search limits in minutes (defaults: -0.5, 0.5)

**Blood Input Time Shift Controls**: The system provides user-configurable limits for the delay parameter search range:

- **UI Controls**: Two numeric inputs in the "Blood Time Shift Search Range" section of the delay fitting tab
  - Lower limit: Default -0.5 minutes (range: -5 to 0, step 0.1)
  - Upper limit: Default 0.5 minutes (range: 0 to 5, step 0.1)
- **Purpose**: Defines the search boundaries for the `inpshift` parameter during model fitting
- **Interpretation**: Time shift represents the degree to which blood input times are adjusted to align with tissue data
- **Implementation**: Added to all delay estimation model fitting functions (`onetcm`, `twotcm`) via `inpshift.lower` and `inpshift.upper` parameters
- **Report Display**: Time shift limits are shown in the delay report's Parameter Settings section for transparency

### Weights System Architecture

**External Segmentation Support**: The system now supports using volume-weighted mean TACs from entire segmentations for weights calculation through an optimized workflow:

1. **Region Definition App**: 
   - Calculates `seg_meanTAC` column during combined regions creation
   - Volume-weighted mean across ALL regions within each segmentation (desc) 
   - Added to `desc-combinedregions_tacs.tsv` as additional column

2. **Modelling App**:
   - Reads unique `desc` values from combined regions file to populate external segmentation dropdown
   - Default weights region type is now "Mean of external segmentation" (optimal approach)
   - Validates combined regions files exist before allowing weights calculation

3. **Weights Report Template**:
   - Uses pre-calculated `seg_meanTAC` for external segmentation weights
   - No longer requires access to original BIDS directory during weights calculation
   - Automatically saves individual weight files in BIDS structure alongside TACs files
   - Generates `{pet_id}_desc-weights_weights.tsv` in same directories as corresponding TACs files

**Configuration Management**: 
- JSON config now stores actual formulas in `formula` field (replaces `custom_formula`)
- For predefined methods: stores mathematical formula (e.g., "sqrt(frame_dur * tac_uncor)")
- For custom methods: stores user-provided formula
- Uses shorter variable names (`tac_uncor` instead of `tac_uncorrected`) for cleaner formulas

### Coding Standards

**IMPORTANT**: Follow tidyverse conventions throughout the codebase for consistency and maintainability.

#### Data Manipulation Standards
- **Use `tidyverse` over base R**: Import `library(tidyverse)` in reports and use tidyverse functions
- **Data structures**: Use `tibble()` instead of `data.frame()`
- **Functional programming**: Use `purrr` functions (`map()`, `walk()`, etc.) instead of `apply()` family
- **String manipulation**: Use `stringr` functions (`str_detect()`, `str_replace()`, etc.) instead of base R
- **Data transformation**: Use `dplyr` verbs (`mutate()`, `filter()`, `select()`, etc.)

#### Examples of Preferred Patterns
```r
# PREFERRED (tidyverse)
library(tidyverse)
data_df <- tibble(
  Parameter = names(config_section),
  Value = map_chr(config_section, as.character)
)

# AVOID (base R)
library(dplyr)
library(ggplot2) 
data_df <- data.frame(
  Parameter = names(config_section),
  Value = sapply(config_section, as.character)
)
```

#### Report Template Standards
- Always load `library(tidyverse)` instead of individual packages
- Use `tibble()` for creating data frames in reports
- Use `map_*()` functions for iteration instead of `sapply()` or `apply()`
- **Use British English spelling**: "visualisation" not "visualization", "colour" not "color", "analyse" not "analyze", etc.
- Maintain consistent code style across all templates

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

#### JSON Configuration Best Practices
**Critical Implementation Details for Weights and Other Configuration Sections**:

1. **Conditional Field Management**: 
   - Set unused conditional fields to empty strings (`""`) in JSON
   - Convert empty strings to `NULL` in R templates/functions
   - Prevents confusion where irrelevant fields show values from other options

2. **Proper JSON Structure**:
   ```r
   # CORRECT: Conditional fields set appropriately
   Weights <- list(
     region_type = input$weights_region_type %||% "mean_combined",
     region = if(input$weights_region_type == "single") input$weights_region %||% "" else "",
     external_tacs = if(input$weights_region_type == "external") input$weights_external_tacs %||% "" else "",
     radioisotope = input$weights_radioisotope %||% "C11",
     halflife = if(input$weights_radioisotope == "Other") as.character(input$weights_halflife %||% 20.4) else "",
     method = input$weights_method %||% "2",
     custom_formula = if(input$weights_method == "custom") input$weights_custom_formula %||% "" else "",
     minweight = input$weights_minweight %||% 0.25
   )
   ```

3. **Template Processing**:
   ```r
   # CORRECT: Convert empty strings to NULL for function calls
   halflife <- if(halflife_raw == "" || is.null(halflife_raw)) NULL else as.numeric(halflife_raw)
   custom_formula <- if(custom_formula_raw == "" || is.null(custom_formula_raw)) NULL else custom_formula_raw
   ```

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

### Report Generation Issues

**Symptom**: Reports fail to generate or show warning messages

**Common Causes and Solutions**:

1. **Missing Template Files**:
   - **Cause**: Report template not found in `inst/rmd/` folder
   - **Solution**: Verify template files exist and are properly named
   - **Check**: Use `system.file("rmd", "template_name.Rmd", package = "kinfitrapp")` to verify paths

2. **Rmarkdown Rendering Errors**:
   - **Cause**: Issues with R Markdown content, missing packages, or data formatting
   - **Solution**: Check error messages in console output
   - **Debug**: Test template rendering independently with `rmarkdown::render()`

3. **Missing Dependencies**:
   - **Cause**: Required packages not available (`rmarkdown`, `knitr`, `ggplot2`)
   - **Solution**: Ensure all report dependencies are installed and loaded

4. **Permission Issues**:
   - **Cause**: Cannot write to reports directory
   - **Solution**: Verify write permissions for `{analysis_folder}/reports/` directory

5. **Parameter Mismatch**:
   - **Cause**: Report parameters don't match template expectations
   - **Solution**: Check parameter names and data structure in report generation functions

**Diagnostic Steps**:
1. Check console output for specific error messages
2. Verify template files exist in `inst/rmd/`
3. Test report generation manually with `generate_step_report()` or `generate_model_report()`
4. Check directory permissions and disk space
