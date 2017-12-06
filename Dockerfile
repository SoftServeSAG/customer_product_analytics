FROM rocker/shiny

# Add application codebase
ADD ./ /srv/shiny-server/customer_product_analytics
RUN chmod a+rwx -R /srv/shiny-server/customer_product_analytics

# Temporal fix for RStan package
# http://discourse.mc-stan.org/t/error-when-installing-rstan-2-16-2/1730
# https://github.com/stan-dev/rstan/wiki/Installing-RStan-on-Mac-or-Linux
RUN echo "\nCXXFLAGS=-O3 -mtune=native -march=native -Wno-unused-variable -Wno-unused-function" >> /etc/R/Makeconf
RUN echo "\nCXXFLAGS+=-DBOOST_PHOENIX_NO_VARIADIC_EXPRESSION" >> /etc/R/Makeconf
RUN echo "\nCPPFLAGS+=-DBOOST_PHOENIX_NO_VARIADIC_EXPRESSION" >> /etc/R/Makeconf

# Install additional Ubuntu packages
RUN sudo apt-get update && sudo apt-get -y install libssl-dev
RUN sudo apt-get -y install libfftw3-3 libfftw3-bin libfftw3-dev libfftw3-double3

# Install additional R packages
RUN R -e "install.packages('shiny')"
RUN R -e "install.packages('DT')"
RUN R -e "install.packages('ggplot2')"
RUN R -e "install.packages('plotly')"
RUN R -e "install.packages('dygraphs')"
RUN R -e "install.packages('devtools')"
RUN R -e "devtools::install_version('shinydashboard', version = '0.5.3', repos = 'http://cran.us.r-project.org')"

RUN R -e "install.packages('data.table')"
RUN R -e "install.packages('dplyr')"
RUN R -e "install.packages('xts')"
RUN R -e "install.packages('Rssa', type = 'source')"
RUN R -e "install.packages('moments')"
RUN R -e "install.packages('prophet')"