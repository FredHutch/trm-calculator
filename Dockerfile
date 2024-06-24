FROM fredhutch/r-shiny-base:4.3.2
RUN apt-get update
RUN apt-get install -y pandoc
RUN R -e "install.packages(c('shiny', 'shinythemes', 'shinydashboard', 'shinyvalidate', 'dplyr', 'gt', 'gtExtras'), repos='https://cran.rstudio.com/')"

ADD app/. /home/shiny/
RUN R -f /home/shiny/check.R --args shiny shinythemes shinydashboard shinyvalidate dplyr gt gtExtras
RUN chown -R shiny:shiny /home/shiny 
WORKDIR /home/shiny
USER shiny
EXPOSE 3838
CMD R -f app.R 
