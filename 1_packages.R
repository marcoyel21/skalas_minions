############################################################
#### LIBRERIAS #############################################
############################################################

load.lib<-c("openxlsx", "readxl", "stringr",
            "stringi","dplyr","ggmap",
            "geosphere","leaflet","dplyr",
            "purrr","DT","shiny")


install.lib<-load.lib[!load.lib %in% installed.packages()]
for(lib in install.lib) install.packages(lib,dependencies=TRUE)
sapply(load.lib,require,character=TRUE)