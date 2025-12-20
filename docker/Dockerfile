FROM --platform=linux/amd64 rocker/shiny:latest

RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    git \
    && rm -rf /var/lib/apt/lists/*

RUN R -e "install.packages('pak', repos = 'https://cloud.r-project.org')"

RUN R -e "pak::pkg_install('FrakenboK/RPRoject')"

RUN mkdir -p /srv/ourpack/data && \
    chown -R shiny:shiny /srv/ourpack/data && \
    chmod -R 755 /srv/ourpack/data

ENV OURPACK_DATA_DIR=/srv/ourpack/data

EXPOSE 3838

CMD ["R", "-e", "yisasnxlooukup::run_app(host='0.0.0.0', port=3838)"]
