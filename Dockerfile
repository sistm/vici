FROM rocker/tidyverse:4.5


WORKDIR /app

RUN apt-get -y update && apt-get -y install \
    libcurl4-openssl-dev \
    libssl-dev  \
    cmake \
    libxml2-dev

COPY . .

RUN R -e 'install.packages("remotes")'
RUN R -e 'install.packages("renv")'
RUN R -e 'renv::activate()'
RUN R -e 'renv::install()'

# RUN R -e 'remotes::install_github("r-lib/remotes", ref = "97bbf81")'
# RUN R -e 'remotes::install_cran("cowplot")'
# RUN R -e 'remotes::install_cran("DT")'
# RUN R -e 'remotes::install_cran("ggplot2")'
# RUN R -e 'install.packages("pbkrtest")'
# RUN R -e 'remotes::install_cran("ggpubr")'
# RUN R -e 'remotes::install_cran("nlme")'
# RUN R -e 'remotes::install_cran("shiny")'
# RUN R -e 'remotes::install_cran("tidyr")'
# RUN R -e 'remotes::install_cran("covr")'
# RUN R -e 'remotes::install_cran("testthat")'
# RUN R -e 'remotes::install_cran("RSelenium")'
# RUN R -e 'remotes::install_cran("Rlabkey")'
# RUN R -e 'remotes::install_cran("RColorBrewer")'
# RUN R -e 'remotes::install_cran("shinyWidgets")'
# RUN R -e 'remotes::install_cran("colourpicker")'
# RUN R -e 'install.packages("golem")'

RUN tar -czvf /app.tar.gz .

RUN R -e 'remotes::install_local("/app.tar.gz")'

COPY .Rprofile /usr/lib/R/etc/

EXPOSE 8080

CMD ["R", "-e", "vici::run_app()"]
