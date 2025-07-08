# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

This will include an R Shiny web application that creates customized kinfitr BIDS App configuration files for PET imaging analysis, which will be within a docker container. It will also include parameterised reports which will run using the parameters in the configuration files. The app will provide a user interface for configuring kinetic modeling parameters for Time Activity Curves (TACs).

The idea is that users will be able to either run kinetic modelling using a pre-existing .json configuration file in a non-interactive manner using the docker container, or alternatively launch the shiny app from the docker container to create the configuration file (and ideally, press a button at the bottom of the tab to run that process, and launch the parameterised report while providing feedback that it is processing), and finally, there will be a final tab of the shiny app to allow doing modelling interactively with reactive figures and parameters that come up.

## Commands

### Running the Application
```r
# In R/RStudio
shiny::runApp()
```

for now, until we dockerise the app at a later point.

### Development
- The shiny app is contained in a single file: `app.R`.
- The parameterised reports will  be Rmd files located in a templates folder.
- No package management files (DESCRIPTION, renv.lock) are present yet
- Dependencies are loaded via `library()` calls at the top of `app.R`

## Architecture

### Application Structure
- **Single-file Shiny app**: All UI and server logic in `app.R`
- **UI Layout**: Uses `fluidPage` with sidebar layout
  - Sidebar: Region definition and data subsetting inputs
  - Main panel: Tabbed interface for model selection and configuration
- **Server Logic**: Reactive expression generates JSON configuration based on user inputs

### Key Dependencies
- `shiny`: Core web application framework
- `shinythemes`: UI theming (uses "flatly" theme)
- `bslib`: Bootstrap components
- `jsonlite`: JSON generation for config files
- Data manipulation: `dplyr`, `tibble`, `purrr`, `stringr`

### Core Functionality
1. **Region Configuration**: Define brain regions and segmentation parameters
2. **Data Subsetting**: Filter by subject, session, tracer, etc.
3. **Weights Definition**: Create weights for modelling
4. **Fit delay**: Estimate the delay between the blood and tissue curves
5. **Model Selection**: Choose between kinetic models (1TCM, 2TCM, Logan, etc.) for 3 models for comparison of outcomes
6. **Parameter Configuration**: Set model-specific parameters and bounds
7. **Config Generation**: Create JSON configuration files for downstream analysis
8. **Interactive modelling:** Fit the models interactively to gauge performance


### File Management
- Generates config files: `config_id-XXXX.json`

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
