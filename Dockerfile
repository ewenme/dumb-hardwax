FROM rocker/r-ver:3.4.1
LABEL maintainer="Ewen"
RUN export DEBIAN_FRONTEND=noninteractive; apt-get -y update \
 && apt-get install -y gdal-bin \
	git-core \
	libcairo2-dev \
	libcurl4-openssl-dev \
	libgeos-dev \
	libgl1-mesa-dev \
	libglu1-mesa-dev \
	libgmp-dev \
	libmariadb-client-lgpl-dev \
	libpng-dev \
	libpq-dev \
	libproj-dev \
	libssl-dev \
	libudunits2-dev \
	libxml2-dev \
	make \
	pandoc \
	pandoc-citeproc \
	tk-table \
	zlib1g-dev
RUN ["install2.r", "-r 'https://cloud.r-project.org'", "bindrcpp", "googleComputeEngineR", "rtweet", "stringr", "dplyr", "purrr", "readr", "tidyr", "tibble", "tidyverse", "reshape2", "listenv", "haven", "lattice", "testthat", "colorspace", "htmltools", "rlang", "foreign", "glue", "modelr", "readxl", "bindr", "plyr", "munsell", "gtable", "cellranger", "rvest", "future", "devtools", "codetools", "psych", "memoise", "forcats", "httpuv", "curl", "broom", "Rcpp", "xtable", "openssl", "scales", "jsonlite", "mime", "googleAuthR", "mnormt", "hms", "digest", "stringi", "shiny", "magrittr", "lazyeval", "crayon", "pkgconfig", "xml2", "lubridate", "rstudioapi", "assertthat", "httr", "R6", "globals", "nlme", "remotes"]
RUN ["installGithub.r", "tidyverse/ggplot2@53a22cd", "jimhester/withr@190d293"]
WORKDIR /payload/
COPY [".", "./"]
CMD ["R", "--vanilla", "-f", "hardwax_bot.R"]
