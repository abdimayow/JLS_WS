# Use a base R image
FROM rocker/r-ver:4.2.1  # Update this if you need a specific R version

# Install OS-level dependencies
RUN apt-get update && apt-get install -y \
    software-properties-common \
    curl \
    gnupg \
    && curl -fsSL https://www.postgresql.org/media/keys/ACCC4CF8.asc | gpg --dearmor -o /usr/share/keyrings/postgres.gpg \
    && echo "deb [signed-by=/usr/share/keyrings/postgres.gpg] http://apt.postgresql.org/pub/repos/apt/ jammy-pgdg main" > /etc/apt/sources.list.d/pgdg.list \
    && apt-get update -y \
    && apt-get install -y libgdal-dev libproj-dev  # Install any other dependencies required for your project
    && rm -rf /var/lib/apt/lists/*

# Install necessary R packages
RUN R -e "install.packages('plumber')"

# Copy your application code into the container
COPY . /app
WORKDIR /app

# Define the command to run your Shiny app
CMD ["R", "-e", "shiny::runApp('/app')"]
