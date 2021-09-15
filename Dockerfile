FROM rocker/r-ver:4.0.5
RUN apt-get update && apt-get install -y  git-core libcurl4-openssl-dev libgit2-dev libicu-dev libpng-dev libssl-dev libxml2-dev make pandoc pandoc-citeproc && rm -rf /var/lib/apt/lists/*
RUN echo "options(repos = c(CRAN = 'https://cran.rstudio.com/'), download.file.method = 'libcurl', Ncpus = 4)" >> /usr/local/lib/R/etc/Rprofile.site
RUN R -e 'install.packages("remotes")'
RUN Rscript -e 'remotes::install_version("magrittr",upgrade="never", version = "2.0.1")'
RUN Rscript -e 'remotes::install_version("tibble",upgrade="never", version = "3.1.1")'
RUN Rscript -e 'remotes::install_version("rlang",upgrade="never", version = "0.4.11")'
RUN Rscript -e 'remotes::install_version("glue",upgrade="never", version = "1.4.2")'
RUN Rscript -e 'remotes::install_version("stringr",upgrade="never", version = "1.4.0")'
RUN Rscript -e 'remotes::install_version("processx",upgrade="never", version = "3.5.2")'
RUN Rscript -e 'remotes::install_version("withr",upgrade="never", version = "2.4.2")'
RUN Rscript -e 'remotes::install_version("htmltools",upgrade="never", version = "0.5.1.1")'
RUN Rscript -e 'remotes::install_version("pkgload",upgrade="never", version = "1.2.1")'
RUN Rscript -e 'remotes::install_version("attempt",upgrade="never", version = "0.3.1")'
RUN Rscript -e 'remotes::install_version("testthat",upgrade="never", version = "3.0.2")'
RUN Rscript -e 'remotes::install_version("shiny",upgrade="never", version = "1.6.0")'
RUN Rscript -e 'remotes::install_version("config",upgrade="never", version = "0.3.1")'
RUN Rscript -e 'remotes::install_version("tidyselect",upgrade="never", version = "1.1.1")'
RUN Rscript -e 'remotes::install_version("rsconnect",upgrade="never", version = "0.8.18")'
RUN Rscript -e 'remotes::install_version("vdiffr",upgrade="never", version = "0.3.3")'
RUN Rscript -e 'remotes::install_version("globals",upgrade="never", version = "0.14.0")'
RUN Rscript -e 'remotes::install_version("spelling",upgrade="never", version = "2.2")'
RUN Rscript -e 'remotes::install_version("charlatan",upgrade="never", version = "0.4.0")'
RUN Rscript -e 'remotes::install_version("Biobase",upgrade="never", version = "2.50.0")'
RUN Rscript -e 'remotes::install_version("shinytest",upgrade="never", version = "1.5.0")'
RUN Rscript -e 'remotes::install_version("shinyFiles",upgrade="never", version = "0.9.0")'
RUN Rscript -e 'remotes::install_version("hexbin",upgrade="never", version = "1.28.2")'
RUN Rscript -e 'remotes::install_version("golem",upgrade="never", version = "0.3.1")'
RUN Rscript -e 'remotes::install_version("ggplot2",upgrade="never", version = "3.3.3")'
RUN Rscript -e 'remotes::install_version("DT",upgrade="never", version = "0.18")'
RUN Rscript -e 'remotes::install_version("dplyr",upgrade="never", version = "1.0.5")'
RUN Rscript -e 'remotes::install_github("UW-GAC/GENESIS@c3936ed4d60cb13537944a9afc5716eb401a1a35")'
RUN mkdir /build_zone
ADD . /build_zone
WORKDIR /build_zone
RUN R -e 'remotes::install_local(upgrade="never")'
RUN rm -rf /build_zone
EXPOSE 3838
CMD R -e "options('shiny.port'=3838,shiny.host='0.0.0.0');shinyNullModel::run_app()"
