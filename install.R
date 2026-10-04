# Install the packages this project needs (run once)
pkgs <- c("shiny", "bslib", "leaflet", "sf", "dplyr", "tidyr", "readr",
          "stringr", "lubridate", "plotly", "DT", "ggplot2", "rsconnect")
missing <- setdiff(pkgs, rownames(installed.packages()))
if (length(missing)) install.packages(missing)
