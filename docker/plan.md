# Docker Implementation Plan for kinfitr Apps (Final)

## Overview
Create a Docker implementation that supports both interactive (Shiny apps) and automatic (config-driven) modes for the kinfitr region definition and modeling apps.

## Architecture

### 1. Docker Setup
- **Base Image**: `rocker/shiny-verse:latest` (includes Shiny server + tidyverse)
- **Default Port**: 3838 (standard Shiny port, can be remapped by user)
- **No FreeSurfer license required**

### 2. File Structure
```
docker/
├── Dockerfile                # Main Docker configuration
├── run_kinfitr.R             # Main entry script with argument parsing
├── docker-compose.yml        # Optional: for easy local development
├── entrypoint.sh            # Shell script to handle Shiny server startup
└── install_packages.R       # R dependency installation script
```

### 3. Entry Script Logic (`run_kinfitr.R`)
**Command-line interface:**
```bash
# BIDS directory only (derivatives will default to bids_dir/derivatives)
docker run -it --rm \
  -v /path/to/bids_dir:/data/bids_dir \
  -p 3838:3838 \
  mathesong/kinfitr:latest \
  --func modelling

# Derivatives directory only (no BIDS directory needed)
docker run -it --rm \
  -v /path/to/derivatives_dir:/data/derivatives_dir \
  -p 3838:3838 \
  mathesong/kinfitr:latest \
  --func modelling

# Automatic mode with specific step
docker run -it --rm \
  -v /path/to/derivatives_dir:/data/derivatives_dir \
  -v /path/to/blood_dir:/data/blood_dir \
  mathesong/kinfitr:latest \
  --func modelling \
  --mode automatic \
  --step weights
```

**Arguments:**
- `--func`: `regiondef` OR `modelling` (required)
- `--mode`: `interactive` (default) OR `automatic`
- `--step`: `datadef`, `weights`, `delay`, `model1`, `model2`, `model3` (only for automatic mode)
- `--kinfitr_output_foldername`: default `"kinfitr"`
- `--analysis_foldername`: default `"Primary_Analysis"`

### 4. Flexible Volume Mount Strategy
**Mount validation logic (matching current app behavior):**
- **At least one of** `/data/bids_dir` OR `/data/derivatives_dir` must be mounted
- **If only bids_dir mounted**: `derivatives_dir` defaults to `/data/bids_dir/derivatives`
- **If only derivatives_dir mounted**: Works independently (no bids_dir needed)
- **If both mounted**: Use both as specified (explicit control)

**Optional mounts:**
- `/data/blood_dir` → Blood data directory (only required conditionally)

### 5. Container Exit Strategy

#### Interactive Mode
**Improved exit behavior:**
- Launch Shiny app in foreground (not as daemon)
- **Container exits automatically when app is closed by user**
- Uses `shiny::runApp()` directly instead of Shiny server
- Provides clean shutdown when user closes browser tab or stops app

**Implementation approach:**
```r
# In run_kinfitr.R for interactive mode
launch_apps(
  bids_dir = bids_dir,
  derivatives_dir = derivatives_dir,
  # ... other args
)
# Container exits when launch_apps() returns (app closed)
```

#### Automatic Mode
- **Container exits after successful completion of processing**
- **Container exits with error code if processing fails**
- No long-running processes remain

### 6. Port Handling
**Default behavior:**
- Container exposes port 3838 internally for interactive mode
- User can remap to any external port: `-p EXTERNAL:3838`
- **Port is only used in interactive mode** - automatic mode doesn't need it

### 7. Step-Based Execution
**For `--mode automatic --step STEPNAME`:**
- `datadef`: Execute data definition report only
- `weights`: Execute weights report only (requires datadef to be complete)
- `delay`: Execute delay report only (requires blood data validation)
- `model1`: Execute Model 1 report only
- `model2`: Execute Model 2 report only  
- `model3`: Execute Model 3 report only

### 8. Blood Data Conditional Logic
**Blood directory validation:**
- Only required when delay fitting AND invasive models are used
- **Applied to step execution**:
  - `delay` step: Always requires blood data validation
  - `model1`, `model2`, `model3` steps: Only if that specific model is invasive
- Clear error messages when required but not mounted

### 9. Mode Implementation

#### Interactive Mode (Default)
1. Parse command-line arguments and detect mounted directories
2. Call `launch_apps()` directly in foreground (uses `shiny::runApp()`)
3. **Container exits automatically when user closes the app**
4. **Ignore `--step` argument** (only relevant for automatic mode)

#### Automatic Mode
1. Parse command-line arguments and detect mounted directories
2. Validate that config JSON exists in analysis folder
3. **If `--step` specified**: Execute only that step
4. **If no `--step`**: Execute full pipeline (same as "Run All")
5. **Exit container when processing complete** (success or failure)

### 10. Container Lifecycle Management
**Interactive mode benefits:**
- Clean resource cleanup when app closes
- No orphaned containers running in background
- Better integration with orchestration systems (Docker Compose, Kubernetes)
- Proper exit codes for monitoring

**User experience:**
- User closes app → Container automatically stops
- No need for manual `docker stop` commands
- Container logs show clean shutdown

### 11. Integration with Existing "Run All" Button
**New function: `run_automatic_pipeline(analysis_folder, bids_dir, blood_dir, step = NULL)`**
- Handles `bids_dir = NULL` or `derivatives_dir = NULL` scenarios
- `step = NULL`: Run full pipeline (used by "Run All" button)
- `step = "weights"`: Run only weights step (used by Docker step execution)

### 12. New Functions Required

#### `detect_mounted_directories()`
- Check which Docker mount points exist and are accessible
- Return list of available directories for app initialization

#### `validate_directory_requirements(func, mode, bids_dir, derivatives_dir)`
- Ensure required directories exist for the selected function and mode
- Handle the flexible bids_dir/derivatives_dir logic

#### `run_automatic_pipeline(analysis_folder, bids_dir, blood_dir, step = NULL)`
- Execute full pipeline OR single step based on `step` parameter
- Handle cases where bids_dir or derivatives_dir might be NULL

### 13. Error Handling & Exit Codes
- **Exit code 0**: Successful completion (interactive app closed cleanly, or automatic processing succeeded)
- **Exit code 1**: Configuration/validation errors
- **Exit code 2**: Missing required files/directories
- **Exit code 3**: Processing errors during automatic execution
- Clear error messages for different failure scenarios

### 14. Development Steps
1. **Create basic Dockerfile** with rocker/shiny-verse base
2. **Build entry script** with flexible directory detection and proper exit handling
3. **Modify `launch_apps()` to support foreground execution** (if needed)
4. **Implement directory validation functions**
5. **Update `run_automatic_pipeline()` to handle NULL directories**
6. **Implement Docker interactive mode** with clean exit behavior
7. **Implement Docker automatic mode** with step-based execution
8. **Test exit behavior** for both modes thoroughly
9. **Create docker-compose.yml** for development testing

### 15. Testing Strategy
- **Interactive exit**: Test that container stops when app is closed
- **Automatic completion**: Test that container exits after processing
- **Error scenarios**: Test proper exit codes for different failure modes
- **Directory flexibility**: Test all mounting scenarios
- **Step execution**: Test each step with minimal required mounts
- **Resource cleanup**: Verify no orphaned processes remain

This plan ensures containers behave like proper applications - they start, do their work, and exit cleanly when done, which is much better practice for containerized environments.