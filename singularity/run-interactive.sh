#!/bin/bash

# Singularity run script for kinfitr interactive mode
# Usage: ./run-interactive.sh [options]

set -e

# Default values
CONTAINER="kinfitr_latest.sif"
FUNC="modelling"
PORT=3838
HOST_PORT=3838
BIDS_DIR=""
DERIVATIVES_DIR=""
BLOOD_DIR=""
KINFITR_FOLDER="kinfitr"
ANALYSIS_FOLDER="Primary_Analysis"

# Help function
show_help() {
    cat << EOF
Run kinfitr Singularity container in interactive mode

Usage: $0 [options]

Options:
    -c, --container PATH    Path to Singularity container (default: $CONTAINER)
    -f, --func FUNC         App function: 'regiondef' or 'modelling' (default: $FUNC)
    -p, --port PORT         Internal port for Shiny app (default: $PORT)
    --host-port PORT        Host port to map to (default: $HOST_PORT)
    --bids-dir PATH         Path to BIDS directory to mount
    --derivatives-dir PATH  Path to derivatives directory to mount
    --blood-dir PATH        Path to blood data directory to mount
    --kinfitr-folder NAME   Name for kinfitr output folder (default: $KINFITR_FOLDER)
    --analysis-folder NAME  Name for analysis subfolder (default: $ANALYSIS_FOLDER)
    -h, --help              Show this help message

Examples:
    # Basic modelling app
    $0 --bids-dir /path/to/bids

    # Region definition app
    $0 --func regiondef --bids-dir /path/to/bids

    # With separate derivatives and blood directories
    $0 --bids-dir /data/bids --derivatives-dir /data/derivatives --blood-dir /data/blood

    # Custom port mapping
    $0 --host-port 8080 --bids-dir /path/to/bids

    # Custom container
    $0 --container ./kinfitr_dev.sif --bids-dir /path/to/bids

Directory Requirements:
    - At least one of --bids-dir or --derivatives-dir must be provided
    - For region definition: --bids-dir recommended
    - For modelling: --bids-dir or --derivatives-dir required
    - Blood data (--blood-dir) only needed for delay fitting with invasive models
EOF
}

# Parse command line arguments
while [[ $# -gt 0 ]]; do
    case $1 in
        -c|--container)
            CONTAINER="$2"
            shift 2
            ;;
        -f|--func)
            FUNC="$2"
            shift 2
            ;;
        -p|--port)
            PORT="$2"
            shift 2
            ;;
        --host-port)
            HOST_PORT="$2"
            shift 2
            ;;
        --bids-dir)
            BIDS_DIR="$2"
            shift 2
            ;;
        --derivatives-dir)
            DERIVATIVES_DIR="$2"
            shift 2
            ;;
        --blood-dir)
            BLOOD_DIR="$2"
            shift 2
            ;;
        --kinfitr-folder)
            KINFITR_FOLDER="$2"
            shift 2
            ;;
        --analysis-folder)
            ANALYSIS_FOLDER="$2"
            shift 2
            ;;
        -h|--help)
            show_help
            exit 0
            ;;
        *)
            echo "Unknown option: $1"
            show_help
            exit 1
            ;;
    esac
done

# Validate required arguments
if [ "$FUNC" != "regiondef" ] && [ "$FUNC" != "modelling" ]; then
    echo "Error: --func must be 'regiondef' or 'modelling'"
    exit 1
fi

if [ -z "$BIDS_DIR" ] && [ -z "$DERIVATIVES_DIR" ]; then
    echo "Error: At least one of --bids-dir or --derivatives-dir must be provided"
    echo "Use --help for more information"
    exit 1
fi

# Check if container exists
if [ ! -f "$CONTAINER" ] && [ ! -d "$CONTAINER" ]; then
    echo "Error: Container not found: $CONTAINER"
    echo "Build the container first using: ./build.sh"
    exit 1
fi

# Validate directories exist
if [ -n "$BIDS_DIR" ] && [ ! -d "$BIDS_DIR" ]; then
    echo "Error: BIDS directory does not exist: $BIDS_DIR"
    exit 1
fi

if [ -n "$DERIVATIVES_DIR" ] && [ ! -d "$DERIVATIVES_DIR" ]; then
    echo "Error: Derivatives directory does not exist: $DERIVATIVES_DIR"
    exit 1
fi

if [ -n "$BLOOD_DIR" ] && [ ! -d "$BLOOD_DIR" ]; then
    echo "Error: Blood directory does not exist: $BLOOD_DIR"
    exit 1
fi

# Build bind mounts
BIND_MOUNTS=""
if [ -n "$BIDS_DIR" ]; then
    BIND_MOUNTS="$BIND_MOUNTS --bind $BIDS_DIR:/data/bids_dir"
fi

if [ -n "$DERIVATIVES_DIR" ]; then
    BIND_MOUNTS="$BIND_MOUNTS --bind $DERIVATIVES_DIR:/data/derivatives_dir"
fi

if [ -n "$BLOOD_DIR" ]; then
    BIND_MOUNTS="$BIND_MOUNTS --bind $BLOOD_DIR:/data/blood_dir"
fi

# Build command arguments
CMD_ARGS="--func $FUNC --mode interactive"
if [ -n "$KINFITR_FOLDER" ]; then
    CMD_ARGS="$CMD_ARGS --kinfitr_output_foldername $KINFITR_FOLDER"
fi
if [ -n "$ANALYSIS_FOLDER" ]; then
    CMD_ARGS="$CMD_ARGS --analysis_foldername $ANALYSIS_FOLDER"
fi

echo "=== kinfitr Singularity Interactive Mode ==="
echo "Container: $CONTAINER"
echo "Function: $FUNC"
echo "Port mapping: $HOST_PORT -> $PORT"
if [ -n "$BIDS_DIR" ]; then
    echo "BIDS directory: $BIDS_DIR"
fi
if [ -n "$DERIVATIVES_DIR" ]; then
    echo "Derivatives directory: $DERIVATIVES_DIR"
fi
if [ -n "$BLOOD_DIR" ]; then
    echo "Blood directory: $BLOOD_DIR"
fi
echo "kinfitr folder: $KINFITR_FOLDER"
echo "Analysis folder: $ANALYSIS_FOLDER"
echo

echo "Starting interactive Shiny app..."
echo "App will be available at: http://localhost:$HOST_PORT"
echo "Press Ctrl+C to stop the container"
echo

# Check if Singularity is installed
if ! command -v singularity &> /dev/null; then
    echo "Error: Singularity is not installed or not in PATH"
    exit 1
fi

# Run the container
echo "Command: singularity run $BIND_MOUNTS $CONTAINER $CMD_ARGS"
echo

exec singularity run $BIND_MOUNTS "$CONTAINER" $CMD_ARGS