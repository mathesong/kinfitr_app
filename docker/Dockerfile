# Use rocker/shiny-verse as base image (includes Shiny + tidyverse)
FROM rocker/shiny-verse:latest

# Install system dependencies
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libgit2-dev \
    && rm -rf /var/lib/apt/lists/*

# Install additional R packages required by kinfitr
RUN install2.r --error --skipinstalled \
    optparse \
    remotes \
    && rm -rf /tmp/downloaded_packages/ /tmp/*.rds

# Install kinfitr from GitHub (adjust repository as needed)
# Note: This assumes kinfitr is available via remotes::install_github()
# RUN R -e "remotes::install_github('mathesong/kinfitr', dependencies = TRUE)"

# Create app directory
WORKDIR /app

# Copy the kinfitr app package files
COPY . /app/

# Install the kinfitrapp package
RUN R -e "devtools::install('.', dependencies = TRUE)"

# Copy Docker-specific scripts
COPY docker/run_kinfitr.R /app/
RUN chmod +x /app/run_kinfitr.R

# Expose port 3838 for Shiny apps
EXPOSE 3838

# Set the entry point
ENTRYPOINT ["/app/run_kinfitr.R"]