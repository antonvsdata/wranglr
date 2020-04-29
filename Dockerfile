# Install R version 3.6.3
FROM r-base:3.6.3

# Install Ubuntu packages
RUN apt-get update && apt-get install -y \
    sudo \
    gdebi-core \
    pandoc \
    pandoc-citeproc \
    libcurl4-gnutls-dev \
    libcairo2-dev/unstable \
    libxt-dev \
    libssl-dev

# Download and install ShinyServer (latest version)
RUN wget --no-verbose https://s3.amazonaws.com/rstudio-shiny-server-os-build/ubuntu-12.04/x86_64/VERSION -O "version.txt" && \
    VERSION=$(cat version.txt)  && \
    wget --no-verbose "https://s3.amazonaws.com/rstudio-shiny-server-os-build/ubuntu-12.04/x86_64/shiny-server-$VERSION-amd64.deb" -O ss-latest.deb && \
    gdebi -n ss-latest.deb && \
    rm -f version.txt ss-latest.deb

# Install R packages that are required
# TODO: add further package if you need!
# RUN R -e "install.packages(c('shiny', 'shinydashboard'), repos='http://cran.rstudio.com/')"
COPY docker/install_packages.R /tmp/
RUN Rscript tmp/install_packages.R

# Copy configuration files into the Docker image
COPY docker/shiny-server.conf  /etc/shiny-server/shiny-server.conf
COPY app /srv/shiny-server/

RUN chown -R shiny:shiny /srv/shiny-server
RUN chown -R shiny:shiny /var/lib/shiny-server

# Make the ShinyApp available at port 80
EXPOSE 80

# Copy further configuration files into the Docker image
COPY docker/shiny-server.sh /usr/bin/shiny-server.sh

CMD ["/usr/bin/shiny-server.sh"]