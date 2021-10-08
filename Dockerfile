# Get base docker image.
FROM rocker/r-ver:4.0.5

# Install linux packages.
RUN apt-get update && apt-get install -y \
    libcairo2-dev \
    libcurl4-openssl-dev \
    libfontconfig1-dev \
    libssl-dev \
    libxml2-dev \
    libxt6 \
    libz-dev


# Create a working directory.
WORKDIR /project

# Copy shiny app to the project.
ADD . /project

# Use renv to install packages.
ENV RENV_VERSION 0.13.2
RUN R -e "install.packages('remotes', repos = c(CRAN = 'https://cloud.r-project.org'))"
RUN R -e "remotes::install_github('rstudio/renv@${RENV_VERSION}')"

#COPY renv.lock renv.lock
RUN R -e 'renv::restore()'

# Install the app package.
RUN R -e 'renv::install(".")'

# Expose the port.
EXPOSE 3838

# Run the app.
CMD R -e "options('shiny.port'=3838,shiny.host='0.0.0.0'); GENESISModelExplorer::run_app()"
