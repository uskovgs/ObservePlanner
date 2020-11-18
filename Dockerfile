FROM rocker/shiny-verse:4.0.2

RUN apt-get update && apt-get install -y

RUN R -e "install.packages('DT, repos = 'http://cran.rstudio.com/')"
RUN R -e "install.packages('suncalc, repos = 'http://cran.rstudio.com/')"
RUN R -e "install.packages('celestial, repos = 'http://cran.rstudio.com/')"
RUN R -e "install.packages('astrolibR, repos = 'http://cran.rstudio.com/')"
RUN R -e "install.packages('lubridate, repos = 'http://cran.rstudio.com/')"
RUN R -e "install.packages('plotly, repos = 'http://cran.rstudio.com/')"


COPY app.R /srv/shiny-server/app.R