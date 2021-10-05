import_data <- function() {
  # Librerias
  library(openxlsx)
  library(readxl)
  library(stringr) #para limpiza regex
  library(stringi) #para limpiza regex
  library(dplyr) #para limpiza regex
  
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
  #_____________ LIMPIEZA TELEFONOS
  #_____________
  refugios<-refugios %>% mutate(NEXTEL=ifelse(str_detect(telefono,"NEXTEL"),1,0),
                                aux_tel= str_remove_all(telefono,"[A-Z]|[.]|[-]|[:]|[,]"),
                                #telefono1=str_extract(aux_tel,"[0-9]"),
                                vector_telef=stri_extract_all(aux_tel,regex="[0-9]{0,2}[*]{0,1}[0-9]{6,12}[*]{0,1}[0-9]{0,2}"),
                                tel1=as.character(lapply(vector_telef, `[`, 1)),
                                tel2=as.character(lapply(vector_telef, `[`, 2)),
                                tel3=as.character(lapply(vector_telef, `[`, 3)),
                                tel4=as.character(lapply(vector_telef, `[`, 4)),
                                
                                #correccion ad hoc para numeros nextel
                                nextel=ifelse(NEXTEL==1,tel2,NA),
                                tel2=ifelse(NEXTEL==1,NA,tel2))
  refugios$NEXTEL<-NULL
  refugios$vector_telef<-NULL
  refugios$aux_tel<-NULL
  refugios$telefono<-NULL
  #_____________ 
  #_____________
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
