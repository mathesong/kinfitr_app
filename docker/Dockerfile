# Use rocker/shiny-verse as base image (includes Shiny + tidyverse)
FROM rocker/shiny-verse:latest

# Arguments for user configuration
ARG USER_ID=1000
ARG GROUP_ID=1000
ARG USER_NAME=kinfitr

# Install system dependencies
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libgit2-dev \
    && rm -rf /var/lib/apt/lists/*

# Create user and group with specified IDs
RUN groupadd -g ${GROUP_ID} ${USER_NAME} && \
    useradd -u ${USER_ID} -g ${GROUP_ID} -m -s /bin/bash ${USER_NAME}

# Install additional R packages required by kinfitr
RUN install2.r --error --skipinstalled \
    optparse \
    remotes \
    && rm -rf /tmp/downloaded_packages/ /tmp/*.rds

# Install kinfitr from GitHub
RUN R -e "remotes::install_github('mathesong/kinfitr', dependencies = TRUE)"

# Create app directory
WORKDIR /app

# Copy the kinfitr app package files
COPY . /app/

# Install the kinfitrapp package
RUN R -e "devtools::install('.', dependencies = TRUE)"

# Copy Docker-specific scripts
COPY docker/run_kinfitr.R /app/
RUN chmod +x /app/run_kinfitr.R

# Change ownership of app directory to non-root user
RUN chown -R ${USER_NAME}:${USER_NAME} /app

# Create data directory and set permissions
RUN mkdir -p /data && chown -R ${USER_NAME}:${USER_NAME} /data

# Switch to non-root user
USER ${USER_NAME}

# Expose port 3838 for Shiny apps
EXPOSE 3838

# Set the entry point
ENTRYPOINT ["/app/run_kinfitr.R"]