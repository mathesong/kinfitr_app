# Dockerfile for kinfitr BIDS App
FROM rocker/shiny:latest

# Install system dependencies
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    && rm -rf /var/lib/apt/lists/*

# Install R packages
RUN R -e "install.packages(c('shiny', 'shinythemes', 'bslib', 'jsonlite', \
                             'dplyr', 'tibble', 'purrr', 'stringr', \
                             'rmarkdown', 'knitr'), repos='https://cran.rstudio.com/')"

# Copy app files
COPY . /srv/shiny-server/kinfitr_app/

# Set working directory
WORKDIR /srv/shiny-server/kinfitr_app/

# Expose port
EXPOSE 3838

# Run app
CMD ["R", "-e", "shiny::runApp(host='0.0.0.0', port=3838)"]