import_data <- function() {
  # Librerias
  library(openxlsx)
  library(readxl)
  
  file <- 'data/refugios_nayarit.xlsx'
  refugios <- data.frame()
  for (i in 1:length(excel_sheets(file))){
    refugios <- rbind(refugios,head(read.xlsx(file, sheet= i, startRow = 7, colNames = F),-1))
  }
  
  colnames(refugios) <- c('id',
                          'refugio',
                          'municipio',
                          'calle',
                          'uso',
                          'servicios',
                          'capacidad',
                          'coordN',
                          'coordW',
                          'altitud',
                          'responsable',
                          'telefono')
  return(refugios)
}

refugios <- import_data()

summary(refugios)

sum(is.na(refugios$coordN))
sum(is.na(refugios$coordW))
sum(is.na(refugios$refugio))

library(tidyr)
# stringi y stringr 

# si eliminamos todas las filas con na's nos quedan de 437 a 425 filas. 
# refugios <- refugios %>% drop_na() 

# quitando solo las filas con na en las coordenadas nos quedan 430 filas. 
# refugios <- refugios %>% drop_na(coordN, coordW)
