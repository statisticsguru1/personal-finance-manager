FROM rstudio/plumber

# Install system dependencies for R packages
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    default-jdk \
    && R CMD javareconf \
    && rm -rf /var/lib/apt/lists/*

# Set working directory to mimic your local repo
WORKDIR /personal-finance-manager

# Copy only the API folder
COPY . ./api

# Install R dependencies (CRAN + GitHub package)
RUN Rscript api/requirements.R

# Expose plumber port
EXPOSE 8000

# Start the API
CMD ["Rscript", "api/main.R"]
