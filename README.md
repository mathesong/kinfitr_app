# kinfitrapp: kinfitr BIDS App Configuration Interface

A comprehensive R Shiny web application suite for creating customized kinfitr BIDS App configuration files for PET imaging analysis. This package provides an intuitive user interface for configuring kinetic modeling parameters for Time Activity Curves (TACs), with support for both interactive GUI-based configuration and automated batch processing.

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
devtools::install_github("yourusername/kinfitrapp")

# Or install from local source
devtools::install("path/to/kinfitrapp")
```

### Docker Installation

**Note**: Pre-built Docker images are not yet available on Docker Hub. Users must currently build the image locally from source.

```bash
# Clone the repository
git clone https://github.com/yourusername/kinfitrapp.git
cd kinfitrapp

# Build the Docker image (this may take 10-15 minutes)
docker build -f docker/Dockerfile -t kinfitr:local .

# Alternatively, use docker-compose to build
cd docker/
docker-compose build

# When pre-built images become available (future):
# docker pull mathesong/kinfitr:latest
```

**Building Requirements**:
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
```bash
# Launch modelling app interactively
docker run -it --rm \
  -v /path/to/your/data:/data/bids_dir \
  -p 3838:3838 \
  kinfitr:local \
  --func modelling

# Then open http://localhost:3838 in your browser
```

#### Automatic Processing
```bash
# Run complete analysis pipeline
docker run --rm \
  -v /path/to/derivatives:/data/derivatives_dir \
  -v /path/to/blood:/data/blood_dir \
  kinfitr:local \
  --func modelling \
  --mode automatic

# Run specific analysis step
docker run --rm \
  -v /path/to/derivatives:/data/derivatives_dir \
  kinfitr:local \
  --func modelling \
  --mode automatic \
  --step weights
```

## Detailed Usage Guide

### System Architecture

The kinfitrapp system uses a standardized directory structure that follows BIDS conventions:

```
your_study/
├── bids_directory/                    # Raw BIDS data
│   ├── participants.tsv
│   ├── participants.json
│   ├── code/
│   │   └── kinfitr/
│   │       └── kinfitr_regions.tsv    # Region definitions
│   └── sub-*/
│       └── ses-*/
│           └── pet/
└── derivatives/                       # Processed outputs
    └── kinfitr/                       # kinfitr outputs
        ├── desc-combinedregions_tacs.tsv  # Combined TACs
        └── Analysis_Name/             # Analysis-specific folder
            ├── desc-kinfitroptions_config.json
            ├── *_desc-combinedregions_tacs.tsv
            └── reports/               # HTML reports
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
- **BIDS Integration**: Automatic participant and PET metadata integration

#### 2. Weights Calculation
- **Multiple Methods**: Choose from predefined or custom weighting formulas
- **External Segmentations**: Use pre-calculated volume-weighted mean TACs
- **Quality Control**: Automatic validation and report generation

#### 3. Delay Estimation (Optional)
- **Blood-Tissue Alignment**: Estimate temporal delays between blood and tissue
- **Multiple Approaches**: From quick single-region to comprehensive multi-region methods
- **Conditional Execution**: Only required for invasive models with delay fitting

#### 4. Model Configuration
- **Three Simultaneous Models**: Configure multiple models for comparison
- **Complete Parameter Control**: Set start values, bounds, and fitting options
- **Model-Specific Settings**: Tailored interfaces for each kinetic model type

#### 5. Interactive Exploration
- **Manual Data Loading**: Explore specific PET measurements and regions
- **Model-Aware Visualization**: Professional plotting with ggplot2
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

#### Flexible Volume Mounting
The Docker implementation supports flexible directory mounting strategies:

```bash
# BIDS directory only (derivatives auto-created)
docker run --rm \
  -v /study/bids:/data/bids_dir \
  kinfitr:local --func modelling

# Derivatives directory only (no BIDS needed)
docker run --rm \
  -v /study/derivatives:/data/derivatives_dir \
  kinfitr:local --func modelling

# Both directories explicitly
docker run --rm \
  -v /study/bids:/data/bids_dir \
  -v /analysis/derivatives:/data/derivatives_dir \
  kinfitr:local --func modelling
```

#### Blood Data Conditional Mounting
Blood data is only required when:
- Delay fitting is enabled (not "none" or "zero")
- AND at least one invasive model is configured

```bash
# Automatic validation - only mount blood when needed
docker run --rm \
  -v /study/derivatives:/data/derivatives_dir \
  -v /study/blood:/data/blood_dir \  # Only needed for invasive + delay
  kinfitr:local \
  --func modelling --mode automatic --step delay
```

#### Server Deployment
```bash
# Production server deployment
docker run -d --name kinfitr-server \
  --restart unless-stopped \
  -v /data/studies:/data/bids_dir \
  -p 8080:3838 \
  kinfitr:local \
  --func modelling

# Access at http://your-server:8080
```

#### Batch Processing Multiple Analyses
```bash
# Process multiple analysis configurations
for analysis in Primary_Analysis Secondary_Analysis Exploratory; do
  echo "Processing $analysis..."
  docker run --rm \
    -v /data/derivatives:/data/derivatives_dir \
    -v /data/blood:/data/blood_dir \
    kinfitr:local \
    --func modelling \
    --mode automatic \
    --analysis_foldername "$analysis"
done
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
1. Check if invasive models are configured (1TCM, 2TCM, Logan, MA1)
2. Verify delay fitting is not set to "none" or "zero"
3. Mount blood directory: `-v /path/to/blood:/data/blood_dir`
4. Ensure blood files follow naming pattern: `*_blood.tsv` or `*_inputfunction.tsv`

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
git clone https://github.com/yourusername/kinfitrapp.git
cd kinfitrapp

# Install development dependencies
R -e "devtools::install_dev_deps()"

# Load package for development
R -e "devtools::load_all()"
```

### Testing

#### R Package Testing
```bash
# Run package tests
R -e "devtools::test()"
```

#### Docker Build and Testing
```bash
# Test Docker build (use kinfitr:local tag for consistency)
docker build -f docker/Dockerfile -t kinfitr:local .

# Test Docker functionality
docker run --rm kinfitr:local --help

# Test with docker-compose (recommended for development)
cd docker/
docker-compose build
docker-compose up kinfitr-interactive

# Verify the build worked with a simple test
docker run --rm kinfitr:local --func modelling --help
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
- Use British English spelling in documentation and reports

## License

This project is licensed under the MIT License - see the LICENSE file for details.

## Citation

If you use kinfitrapp in your research, please cite:

```
[Add appropriate citation information when available]
```

## Acknowledgments

- Built on the kinfitr package for PET kinetic modeling
- Uses the Shiny framework for interactive web applications  
- Docker implementation based on rocker/shiny-verse
- Follows BIDS conventions for neuroimaging data organization

---

For more detailed information, see the documentation in the `docker/` directory and the function documentation accessible via `?function_name` in R.