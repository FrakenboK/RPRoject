FROM --platform=linux/amd64 rocker/shiny:latest

RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    && rm -rf /var/lib/apt/lists/*


RUN R -e "options(repos = c(CRAN = 'https://cloud.r-project.org')); \
    install.packages(c('devtools', 'httr2', 'ipaddress', 'DT', 'RColorBrewer'), \
                     dependencies = c('Depends', 'Imports'), \
                     Ncpus = 2)"


WORKDIR /app


COPY . /app/yisasnxlooukup/

RUN R -e "setwd('/app/yisasnxlooukup'); \
    if (!requireNamespace('devtools', quietly = TRUE)) install.packages('devtools', repos='https://cloud.r-project.org'); \
    devtools::install(dependencies = FALSE, upgrade = 'never')"


RUN mkdir -p /srv/ourpack/data && \
    chown -R shiny:shiny /srv/ourpack/data && \
    chmod -R 755 /srv/ourpack/data

ENV OURPACK_DATA_DIR=/srv/ourpack/data

EXPOSE 3838


CMD ["R", "-e", "yisasnxlooukup::run_app(host='0.0.0.0', port=3838)"]
