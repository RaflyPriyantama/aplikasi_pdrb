FROM rocker/r-ver:4.6.1

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

RUN R -e "install.packages('pak', repos='https://r-lib.github.io/p/pak/stable/')"

RUN R -e "pak::pkg_install(c( \
    'shiny', \
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
    'ggplot2', \
    'tibble', \
    'htmltools', \
    'jsonlite', \
    'knitr', \
    'rmarkdown' \
))"

RUN R -e "stopifnot(requireNamespace('shiny', quietly = TRUE))"

WORKDIR /app

COPY . /app

EXPOSE 8080

CMD ["R", "-e", "shiny::runApp('/app', host='0.0.0.0', port=as.numeric(Sys.getenv('PORT', '8080')))"]
