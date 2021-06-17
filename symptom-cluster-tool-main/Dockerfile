# Example shiny app docker file
# https://blog.sellorm.com/2021/04/25/shiny-app-in-docker/

# get shiny serves plus tidyverse packages image
FROM rocker/shiny-verse:latest

# set clock?
RUN echo "Acquire::Check-Valid-Until \"false\";\nAcquire::Check-Date \"false\";" | cat > /etc/apt/apt.conf.d/10no--check-valid-until

# system libraries
# Try to only install system libraries you actually need
# Package Manager is a good resource to help discover system deps
#RUN apt-get update && apt-get install -y  
RUN apt-get update && apt-get install -y  \
    libxml2-dev \
    libudunits2-dev \
    libssh2-1-dev \
    libcurl4-openssl-dev \
    libsasl2-dev \
    libv8-dev \
    unixodbc-dev \
    && rm -rf /var/lib/apt/lists/*

# install R packages required 
# Change the packages list to suit your needs
RUN R -e 'install.packages(c(\
              "shiny", \
              "here", \
              "tidyverse", \
              "DT", \
              "plotly", \
              "FactoMineR", \
              "shinycssloaders", \
              "ggplot2" \
            ), \
            repos="https://packagemanager.rstudio.com/cran/__linux__/focal/2021-04-23"\
          )'


# copy the app directory into the image
COPY ./shiny-app/* /srv/shiny-server/
COPY ./shiny-app/www/cluster_pca_3d.png /srv/shiny-server/www/cluster_pca_3d.png

# run app
CMD ["/usr/bin/shiny-server"]