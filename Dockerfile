# Use the official R base image
FROM rocker/shiny:4.3.0

# Set the working directory
WORKDIR /srv/shiny-server

# Install system dependencies
RUN apt-get update && apt-get install -y \
    libpq-dev \
    libssl-dev \
    libcurl4-openssl-dev \
    libxml2-dev \
    && rm -rf /var/lib/apt/lists/*

# Install R packages
RUN R -e "install.packages(c('shiny', 'shinydashboard', 'DT', 'plotly', 'dplyr', 'RPostgreSQL', 'lubridate', 'scales', 'ggplot2'), repos='https://cran.rstudio.com/')"

# Copy the Shiny app
COPY app.R /srv/shiny-server/

# Make sure the app has the right permissions
RUN chmod -R 755 /srv/shiny-server/

# Expose the port that Shiny runs on
EXPOSE 3838

# Set environment variables for Railway
ENV PORT=3838
ENV HOST=0.0.0.0

# Run the Shiny app
CMD ["R", "-e", "shiny::runApp('/srv/shiny-server/app.R', host='0.0.0.0', port=3838)"]