#FROM openanalytics/r-base
#FROM rstudio/r-base:4.2.2-rockylinux8
FROM rstudio/r-base:4.2.2-focal

MAINTAINER John Erickson "erickj4@rpi.edu"

# system libraries of general use
RUN apt-get update && apt-get install -y --no-install-recommends \
    sudo \
    cmake \
    libharfbuzz-dev \
    libfreetype6-dev \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    libssl-dev \
    libssh2-1-dev \
    libxml2-dev \
    libproj-dev \
    libgdal-dev \
    libudunits2-dev \
    librdf0 \
    librdf0-dev \
    && rm -rf /var/lib/apt/lists/*

## Regular R packages
RUN R -e 'install.packages("XML",repos="https://cloud.r-project.org/")'
RUN R -e 'install.packages("dplyr",repos="https://cloud.r-project.org/")'
RUN R -e 'install.packages("shiny",repos="https://cloud.r-project.org/")'
RUN R -e 'install.packages("shinyjs",repos="https://cloud.r-project.org/")'
RUN R -e 'install.packages("bslib",repos="https://cloud.r-project.org/")'
RUN R -e 'install.packages("RCurl",repos="https://cloud.r-project.org/")'
RUN R -e 'install.packages("stringr",repos="https://cloud.r-project.org/")'

# copy the app to the image
RUN mkdir /root/fairness
COPY fmo-interface /root/fairness

COPY Rprofile.site /usr/lib/R/etc/

EXPOSE 3838
#EXPOSE 1824

CMD ["R", "-e", "shiny::runApp('/root/fairness')"]
