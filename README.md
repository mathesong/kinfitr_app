# kinfitrapp: kinfitr BIDS App Configuration Interface

An R Shiny web application suite for creating customized kinfitr BIDS App configuration files for PET imaging analysis. This package provides an intuitive user interface for configuring kinetic modeling parameters for Time Activity Curves (TACs), with support for both interactive GUI-based configuration and automated batch processing.

## Overview

The kinfitrapp package consists of two complementary Shiny applications that work together to provide a complete workflow for PET kinetic modeling analysis:

1. **Region Definition App**: Creates brain region definitions and combined TACs from segmentation data
2. **Modelling App**: Configures kinetic models and creates comprehensive analysis configurations

Both applications support multiple usage modes including interactive GUI configuration, automated batch processing, and Docker containerization for reproducible research environments.

## Features

### Core Functionality
- **Dual App Architecture**: Separate applications for region definition and kinetic modeling
- **BIDS Compliance**: Full integration with Brain Imaging Data Structure (BIDS) conventions
- **Flexible Data Input**: Works with BIDS directories, derivatives directories, or both
- **Comprehensive Model Support**: Supports both invasive and non-invasive kinetic models
- **Interactive Data Exploration**: Built-in visualization and model testing capabilities
- **Automated Pipeline Execution**: Run complete analysis workflows programmatically
- **Docker Integration**: Containerized deployment for reproducible research

### Supported Kinetic Models

#### Invasive Models (Require Blood Input)
- **1TCM**: Single tissue compartment model
- **2TCM**: Two tissue compartment model  
- **Logan**: Logan graphical analysis
- **MA1**: Multilinear analysis

#### Non-Invasive Models (Reference Region Based)
- **SRTM**: Simplified reference tissue model
- **refLogan**: Reference Logan analysis
- **MRTM1**: Multilinear reference tissue model
- **MRTM2**: Multilinear reference tissue model 2

### Advanced Features
- **Parameterized Reports**: Automatic generation of HTML quality control reports
- **Interactive Plotly Visualizations**: Rich, explorable data visualizations in reports
- **State Persistence**: Automatic saving and restoration of app configurations
- **Weights Calculation**: Multiple weighting methods for kinetic model fitting
- **Delay Estimation**: Blood-tissue delay estimation with multiple approaches
- **Three Model Comparison**: Configure up to 3 models simultaneously

## Installation

### Prerequisites
- R (≥ 4.0.0)
- Required R packages (automatically installed with the package)

### Install from Source
```r
# Install development version from GitHub
devtools::install_github("mathesong/kinfitrapp")

# Or install from local source
devtools::install("path/to/kinfitrapp")
```

### Docker Installation

#### Option 1: Pull Pre-built Image (Recommended)

```bash
# Pull the latest pre-built image from Docker Hub
docker pull mathesong/kinfitr_app:latest
```

#### Option 2: Build Manually from Source

```bash
# Clone the repository
git clone https://github.com/mathesong/kinfitr_app.git
cd kinfitr_app

# Build the Docker image (this may take 10-15 minutes)
docker build -f docker/Dockerfile -t mathesong/kinfitr_app:latest .

# Alternatively, use docker-compose to build
cd docker/
docker-compose build
```

**Manual Building Requirements**:
- Docker daemon with at least 4GB RAM allocated
- ~10-15 minutes build time (downloads ~3GB base image)
- ~5GB free disk space for final image

## Quick Start

### R Package Usage

#### Launch Both Apps Sequentially
```r
library(kinfitrapp)

# Launch both region definition and modelling apps
launch_apps(
  bids_dir = "/path/to/your/bids/dataset",
  region_definition = TRUE,
  modelling = TRUE
)
```

#### Launch Individual Apps
```r
# Region definition app only
region_definition_app(bids_dir = "/path/to/bids/dataset")

# Modelling app only  
modelling_app(bids_dir = "/path/to/bids/dataset")

# Modelling app with derivatives directory
modelling_app(
  derivatives_dir = "/path/to/derivatives",
  blood_dir = "/path/to/blood/data"
)
```

### Docker Usage

#### Interactive Mode (Default)

**Region Definition App (Interactive)**
```bash
# Launch region definition app interactively
docker run -it --rm \
  --user $(id -u):$(id -g) \
  -v /path/to/your/bids:/data/bids_dir:ro \
  -v /path/to/your/derivatives:/data/derivatives_dir:rw \
  -p 3838:3838 \
  mathesong/kinfitr_app:latest \
  --func regiondef

# Then open http://localhost:3838 in your browser
```

**Modelling App (Interactive)**
```bash
# Launch modelling app interactively
docker run -it --rm \
  --user $(id -u):$(id -g) \
  -v /path/to/your/bids:/data/bids_dir:ro \
  -v /path/to/your/derivatives:/data/derivatives_dir:rw \
  -v /path/to/your/blood:/data/blood_dir:ro \
  -p 3838:3838 \
  mathesong/kinfitr_app:latest \
  --func modelling

# Then open http://localhost:3838 in your browser
```

**Detached Mode (Background - use docker logs to see startup messages)**
```bash
# Launch modelling app in background
docker run -d --name kinfitr-server \
  --user $(id -u):$(id -g) \
  -v /path/to/your/bids:/data/bids_dir:ro \
  -v /path/to/your/derivatives:/data/derivatives_dir:rw \
  -v /path/to/your/blood:/data/blood_dir:ro \
  -p 3838:3838 \
  mathesong/kinfitr_app:latest \
  --func modelling

# Check startup messages and get browser URL
docker logs kinfitr-server

# Then open http://localhost:3838 in your browser

# Stop and remove when done
docker stop kinfitr-server
docker rm kinfitr-server
```

#### Automatic Processing
```bash
# Run complete analysis pipeline
docker run --rm \
  --user $(id -u):$(id -g) \
  -v /path/to/your/bids:/data/bids_dir:ro \
  -v /path/to/your/derivatives:/data/derivatives_dir:rw \
  -v /path/to/your/blood:/data/blood_dir:ro \
  mathesong/kinfitr_app:latest \
  --func modelling \
  --mode automatic

# Run specific analysis step
docker run --rm \
  --user $(id -u):$(id -g) \
  -v /path/to/your/bids:/data/bids_dir:ro \
  -v /path/to/your/derivatives:/data/derivatives_dir:rw \
  -v /path/to/your/blood:/data/blood_dir:ro \
  mathesong/kinfitr_app:latest \
  --func modelling \
  --mode automatic \
  --step weights
```

## Detailed Usage Guide

### System Architecture

The kinfitrapp system uses a standardized directory structure that follows BIDS conventions:

```

 bids_directory/                    # Raw BIDS data
 ├── participants.tsv
 ├── participants.json
 ├── code/
 │   └── kinfitr/
 │       └── kinfitr_regions.tsv    # Region definitions
 └── sub-*/
     └── ses-*/
         └── pet/
 derivatives/                       # Processed outputs
 └── kinfitr/                       # kinfitr outputs
     ├── desc-combinedregions_tacs.tsv  # Combined TACs
     └── Analysis_Name/             # Analysis-specific folder
         ├── desc-kinfitroptions_config.json
         ├── *_desc-combinedregions_tacs.tsv
         └── reports/               # HTML reports
         └── sub-*/
              └── ses-*/
                  └── pet/
```

### Region Definition Workflow

The region definition app creates combined brain regions from segmentation data:

1. **Configure Data Sources**: Specify BIDS directory and segmentation files
2. **Define Custom Regions**: Create combined regions from individual segments
3. **Generate Combined TACs**: Produce volume-weighted time activity curves
4. **Export Configuration**: Save region definitions for reproducibility

#### Key Outputs
- `kinfitr_regions.tsv`: Region definition configuration
- `desc-combinedregions_tacs.tsv`: Combined TACs with metadata integration

### Modelling App Workflow

The modelling app provides comprehensive kinetic modeling configuration:

#### 1. Data Definition
- **Subset Selection**: Filter by subject, session, tracer, etc.
- **Region Selection**: Choose brain regions for analysis

#### 2. Weights Calculation
- **Multiple Methods**: Choose from predefined or custom weighting formulas

#### 3. Delay Estimation (Optional)
- **Blood-Tissue Alignment**: Estimate temporal delays between blood and tissue
- **Multiple Approaches**: Quick single-region and multi-region methods

#### 4. Model Configuration
- **Three Sequential Models**: Configure multiple models for comparison and parameter inheritance
- **Parameter Control**: Set start values, bounds, and fitting options
- **Model-Specific Settings**: Tailored interfaces for each kinetic model type

#### 5. Interactive Exploration
- **Manual Data Loading**: Explore specific PET measurements and regions
- **Validation Testing**: Test model configurations before full processing

### Automatic Pipeline Execution

#### Using the "Run All" Button
The modelling app includes a "Run All" button that executes the complete analysis pipeline:

1. Saves current configuration to JSON
2. Executes all configured analysis steps sequentially
3. Generates comprehensive HTML reports
4. Provides progress notifications and error handling

#### Programmatic Execution
```r
# Execute complete pipeline programmatically
result <- run_automatic_pipeline(
  analysis_folder = "/path/to/analysis",
  bids_dir = "/path/to/bids",
  blood_dir = "/path/to/blood",
  step = NULL  # NULL = full pipeline
)

# Execute specific step
result <- run_automatic_pipeline(
  analysis_folder = "/path/to/analysis",
  bids_dir = "/path/to/bids",
  step = "weights"  # specific step only
)
```

### Docker Advanced Usage

#### Volume Mount Permissions Strategy
The Docker implementation follows security best practices for data access:

- **BIDS Directory** (`/data/bids_dir`): Always mounted **read-only (`:ro`)** to protect source data
- **Derivatives Directory** (`/data/derivatives_dir`): Always mounted **read-write (`:rw`)** for processing outputs
- **Blood Directory** (`/data/blood_dir`): Mounted **read-only (`:ro`)** as it contains reference data

This approach ensures:
- Source BIDS data remains protected from accidental modification
- Processing outputs are written to appropriate derivatives location
- Configuration files are stored in derivatives (not BIDS) for Docker compatibility


#### Blood Data Conditional Mounting
Blood data is only required when:
- Delay fitting is enabled (not "none" or "zero")
- AND at least one invasive model is configured

```bash
# Automatic validation - only mount blood when needed
docker run --rm \
  --user $(id -u):$(id -g) \
  -v /study/bids:/data/bids_dir:ro \
  -v /study/derivatives:/data/derivatives_dir:rw \
  -v /study/blood:/data/blood_dir:ro \  # Only needed for invasive + delay
  mathesong/kinfitr_app:latest \
  --func modelling --mode automatic --step delay
```

#### Server Deployment
```bash
# Production server deployment
docker run -d --name kinfitr-server \
  --user $(id -u):$(id -g) \
  --restart unless-stopped \
  -v /data/studies:/data/bids_dir:ro \
  -v /data/derivatives:/data/derivatives_dir:rw \
  -p 8080:3838 \
  mathesong/kinfitr_app:latest \
  --func modelling

# Access at http://your-server:8080
```

### Development and Testing

#### Using Docker Compose
```bash
cd docker/

# Launch interactive modelling app
docker-compose up kinfitr-interactive
# Access at http://localhost:3838

# Launch region definition app  
docker-compose up kinfitr-regiondef
# Access at http://localhost:3839

# Test automatic processing
docker-compose up kinfitr-auto-full

# Test specific step processing
docker-compose up kinfitr-auto-step
```

#### Local Development
```r
# Load package for development
devtools::load_all()

# Test functions locally
validate_directory_requirements("modelling", "automatic", "/path/to/bids", NULL)

# Test automatic pipeline
result <- run_automatic_pipeline("/path/to/analysis", "/path/to/bids")
```

## Configuration Management

### State Persistence
Both apps automatically save and restore their complete configuration state:

- **On Startup**: Checks for existing `desc-kinfitroptions_config.json`
- **Auto-Save**: Saves state before executing operations
- **Full Restoration**: Restores all UI inputs to previous state
- **Error Handling**: Graceful fallback for corrupted configurations

### Configuration File Structure
```json
{
  "Subsetting": {
    "subjects": "01,02,03",
    "sessions": "",
    "tracers": "C11_raclopride"
  },
  "Weights": {
    "region_type": "mean_combined",
    "method": "2",
    "formula": "sqrt(frame_dur * tac_uncor)"
  },
  "FitDelay": {
    "model": "1tcm_median",
    "time_window": 5
  },
  "Model1": {
    "model": "2TCM",
    "startValues": { "K1": 0.5, "k2": 0.3 },
    "lowerBounds": { "K1": 0, "k2": 0 }
  }
}
```

## Report Generation System

### Automatic Report Creation
The system generates comprehensive HTML reports for each analysis step:

- **Data Definition Report**: Data subsetting and TACs creation summary
- **Weights Report**: Weighting calculation results and validation
- **Delay Report**: Blood-tissue delay estimation results  
- **Model Reports**: Individual reports for each configured model

### Interactive Visualizations
Reports include advanced Plotly visualizations with:
- **Cross-filtering**: Hover to highlight, double-click to reset
- **Axis Scaling**: Dropdown menus for linear/log combinations
- **Hover Tooltips**: Context-specific information
- **Professional Formatting**: Publication-ready plots and tables

### Report Access
```bash
# Reports are generated in the analysis folder
your_analysis/
└── reports/
    ├── data_definition_report.html
    ├── weights_report.html
    ├── delay_report.html
    ├── model1_report.html
    ├── model2_report.html
    └── model3_report.html
```

## Troubleshooting

### Common Issues

#### Missing Combined TACs File
**Symptoms**: Modelling app shows no data available
**Solutions**:
1. Run region definition app first to generate combined TACs
2. Check that `desc-combinedregions_tacs.tsv` exists in kinfitr directory
3. Verify region matching between segmentation and TACs data

#### Docker Container Won't Start
**Symptoms**: Container exits immediately or shows port errors
**Solutions**:
1. Check if port 3838 is already in use: `netstat -tlnp | grep 3838`
2. Use different port mapping: `-p 3839:3838`
3. Verify volume mount paths exist and are accessible

#### Blood Data Validation Errors
**Symptoms**: "Blood data required" errors in automatic mode
**Solutions**:
1. Verify delay fitting is not set to "none" or "zero"
2. Mount blood directory: `-v /path/to/blood:/data/blood_dir`
3. Ensure blood files follow naming pattern: `*_blood.tsv` or `*_inputfunction.tsv`

#### Report Generation Failures
**Symptoms**: Reports not generated or show errors
**Solutions**:
1. Check analysis folder write permissions
2. Verify all required R packages are installed
3. Check for missing template files in `inst/rmd/`
4. Ensure sufficient disk space for report generation

### Getting Help

#### Log Information
- **Interactive Mode**: Check R console for detailed messages
- **Docker Mode**: Use `docker logs <container_name>` to view output
- **Report Errors**: Check browser developer console for JavaScript errors

#### Debug Mode
```r
# Enable detailed logging
options(shiny.trace = TRUE)

# Launch with debug information
region_definition_app(bids_dir = "/path/to/bids")
```

## Performance Considerations

### Memory Usage
- **Large Datasets**: Consider processing subsets of data for memory-constrained environments
- **Docker Resources**: Allocate sufficient memory to Docker daemon (≥4GB recommended)
- **Parallel Processing**: Reports may benefit from multiple CPU cores

### Storage Requirements
- **Input Data**: Original BIDS datasets (varies by study)
- **Processed Data**: Combined TACs and analysis files (~10-50MB per analysis)
- **Reports**: HTML reports with embedded plots (~5-20MB each)
- **Docker Images**: Base image ~2-3GB, kinfitr image ~3-4GB

## Contributing

### Development Setup
```bash
# Clone repository
git clone https://github.com/mathesong/kinfitrapp.git
cd kinfitrapp

# Install development dependencies
R -e "devtools::install_dev_deps()"

# Load package for development
R -e "devtools::load_all()"
```

### Testing

#### R Package Testing

Forthcoming...

```bash
# Run package tests
R -e "devtools::test()"
```

#### Docker Build and Testing
```bash
# Test Docker build
docker build -f docker/Dockerfile -t mathesong/kinfitr_app:latest .

# Test Docker functionality
docker run --rm mathesong/kinfitr_app:latest --help

# Test with docker-compose (recommended for development)
cd docker/
docker-compose build
docker-compose up kinfitr-interactive

# Verify the build worked with a simple test
docker run --rm mathesong/kinfitr_app:latest --func modelling --help
```

#### Build Troubleshooting
- **Memory Issues**: Increase Docker daemon RAM allocation to ≥4GB
- **Slow Build**: Build process downloads large R dependencies (~2-3GB)
- **Network Timeouts**: On slower connections, allow 20-30 minutes for complete build
- **Disk Space**: Use `docker system prune` to free space if build fails due to insufficient storage
- **Permission Errors**: Ensure Docker daemon is running and user has appropriate permissions

### Code Standards
- Follow tidyverse style conventions
- Use `tidyverse` packages over base R equivalents
- Include roxygen2 documentation for all exported functions
- Write tests for core functionality
- British English spelling in documentation and reports

## License

This project is licensed under the MIT License - see the LICENSE file for details.

## Citation

If you use kinfitrapp in your research, please cite *kinfitr for now*:

An introduction to the package:

> Matheson, G. J. (2019). *Kinfitr: Reproducible PET Pharmacokinetic
> Modelling in R*. bioRxiv: 755751. <https://doi.org/10.1101/755751>

A validation study compared against commercial software:

> Tjerkaski, J., Cervenka, S., Farde, L., & Matheson, G. J. (2020).
> *Kinfitr – an open source tool for reproducible PET modelling:
> Validation and evaluation of test-retest reliability*. bioRxiv:
> 2020.02.20.957738. <https://doi.org/10.1101/2020.02.20.957738>

## Acknowledgments

- Built around the *kinfitr* package for PET kinetic modeling
- Uses the Shiny framework for interactive web applications  
- Docker implementation based on rocker/shiny-verse
- Follows BIDS conventions for neuroimaging data organization

---

For more detailed information, see the documentation in the `docker/` directory and the function documentation accessible via `?function_name` in R.
