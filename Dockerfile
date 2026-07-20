FROM rocker/shiny:4.4.3

USER root

RUN apt-get update && apt-get install -y --no-install-recommends \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libfontconfig1-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff-dev \
    libjpeg-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libgit2-dev \
    libicu-dev \
    libglpk-dev \
    libzip-dev \
    build-essential \
    pandoc \
    zip \
    unzip \
    && rm -rf /var/lib/apt/lists/*

RUN R -e "options( \
    repos = c(CRAN = 'https://cloud.r-project.org'), \
    timeout = 1200 \
); install.packages(c( \
    'shinydashboard', \
    'DT', \
    'plotly', \
    'dplyr', \
    'tidyr', \
    'purrr', \
    'readxl', \
    'openxlsx', \
    'stringr', \
    'scales', \
    'tibble', \
    'jsonlite', \
    'quarto', \
    'rmarkdown' \
), Ncpus = 2)"

RUN R -e "stopifnot( \
    requireNamespace('shiny', quietly = TRUE), \
    requireNamespace('shinydashboard', quietly = TRUE), \
    requireNamespace('DT', quietly = TRUE), \
    requireNamespace('plotly', quietly = TRUE), \
    requireNamespace('dplyr', quietly = TRUE), \
    requireNamespace('readxl', quietly = TRUE), \
    requireNamespace('openxlsx', quietly = TRUE) \
)"

WORKDIR /app

COPY . /app

RUN chown -R shiny:shiny /app

USER shiny

EXPOSE 8080

CMD ["R", "-e", "shiny::runApp('/app', host='0.0.0.0', port=as.numeric(Sys.getenv('PORT', '8080')))"]
