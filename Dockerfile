# ============================================
# ZE1 Report Generator - Docker Image
# Basis: rocker/shiny (Shiny Server + R)
# ============================================

FROM rocker/shiny:4.3

LABEL maintainer="ZE1 Abwasserapp"
LABEL description="ZE1 Report Generator - Abwasseranalytik"

# System-Abhängigkeiten (LaTeX für PDF-Reports, system libs für R-Pakete)
RUN apt-get update && apt-get install -y --no-install-recommends \
    texlive-latex-base \
    texlive-latex-extra \
    texlive-lang-german \
    texlive-fonts-recommended \
    texlive-science \
    libxml2-dev \
    libssl-dev \
    libcurl4-openssl-dev \
    libmariadb-dev \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

# R-Pakete vorinstallieren (nicht zur Laufzeit!)
RUN install2.r --error --skipinstalled \
    shinyFiles \
    shinydashboard \
    shinyjs \
    shinyalert \
    shinyBS \
    dplyr \
    tidyr \
    magrittr \
    stringr \
    DT \
    rhandsontable \
    xlsx \
    kableExtra \
    data.table \
    ggplot2 \
    RColorBrewer \
    cowplot \
    openssl \
    outliers \
    flux \
    lubridate \
    plyr \
    rmarkdown \
    readr \
    tools \
    && rm -rf /tmp/downloaded_packages/ /tmp/*.rds

# App-Verzeichnis im Container
RUN mkdir -p /srv/shiny-server/myShinyApps/R_Viewer
RUN mkdir -p /srv/shiny-server/myShinyApps/QC_Viewer_org

# App-Code kopieren
COPY myShinyApps/R_Viewer/ /srv/shiny-server/myShinyApps/R_Viewer/
COPY myShinyApps/QC_Viewer_org/ /srv/shiny-server/myShinyApps/QC_Viewer_org/

# Rechte für shiny-User setzen
RUN chown -R shiny:shiny /srv/shiny-server/myShinyApps

# Shiny Server läuft auf Port 3838
EXPOSE 3838

# Shiny Server starten
CMD ["/usr/bin/shiny-server"]
