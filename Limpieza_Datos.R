import_data <- function() {
  # Librerias
  library(openxlsx)
  library(readxl)
  library(stringr) #para limpiza regex
  library(stringi) #para limpiza regex
  library(dplyr) #para limpiza regex
  library(tidyverse)
  library(ggmap)#para rellenar coordenadas faltantes
  
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
  ##### Limpieza de coordenadas ######
  refugios<- refugios %>% mutate(## Limpieza particular del id 213
    #coordN=ifelse(id==213,paste("21",coordN,sep="°"),coordN),
    ### para coordN
    coordN1=stri_extract_all(coordN,regex="[0-9]{2,3}"),
    coordN2=paste(as.character(lapply(coordN1, `[`, 1)),
                  as.character(lapply(coordN1, `[`, 2)),sep = "º"),
    coordN3=paste(as.character(lapply(coordN1, `[`, 4)),"\"", sep = ""),
    coordN4=paste(as.character(lapply(coordN1, `[`, 3)),coordN3, sep="."),
    coordN=paste(coordN2,coordN4, sep="\'"),
    ### para coordW
    coordW1=stri_extract_all(coordW,regex="[0-9]{2,3}"),
    coordW2=paste(as.character(lapply(coordW1, `[`, 1)),
                  as.character(lapply(coordW1, `[`, 2)),sep = "º"),
    coordW3=paste(as.character(lapply(coordW1, `[`, 4)),"\"", sep = ""),
    coordW4=paste(as.character(lapply(coordW1, `[`, 3)),coordW3, sep="."),
    coordW=paste(coordW2,coordW4, sep="\'"),
    ## Agrego NAs
    coordN=ifelse(str_detect(coordN,"NA"),NA,coordN),
    coordW=ifelse(str_detect(coordW,"NA"),NA,coordW),
    ##### Limpieza de coordenadas ad hoc para el id 434######
    coord_aux=coordN,
    coordN=ifelse(id==434,coordW,coordN),
    coordW=ifelse(id==434,coord_aux,coordW),
    ##### Limpieza de coordenadas ad hoc para el id 338######
    calle=ifelse(id==338,str_remove(calle,"POR ENTRADA PRINCIPAL DEL "),calle)
  )
  refugios$coord_aux<-NULL
  
  refugios$coordN1<-NULL
  refugios$coordN2<-NULL
  refugios$coordN3<-NULL
  refugios$coordN4<-NULL
  
  refugios$coordW1<-NULL
  refugios$coordW2<-NULL
  refugios$coordW3<-NULL
  refugios$coordW4<-NULL
  

  #_____________ 
  #_____________
  ##### Relleno de coordenadas faltantes con google API ######
  new_DF <- refugios[is.na(refugios$coordN),]
  #################################################################################PONER LLAVE API DE GOOGLE
  register_google(key = "TOKEN", write = TRUE) #registro de llave
  
  cc <- map_df(1:nrow(new_DF), ~ geocode(paste(new_DF$calle[.], new_DF$municipio[.] , sep=" "))) #crea df de coordenadas faltantes 
  
  refugios[is.na(refugios$coordN), ]$coordN <- cc$lat #rellelna latitud
  refugios[is.na(refugios$coordW), ]$coordW <- cc$lon #rellena longitud
  
  return(refugios)
}

refugios <- import_data()
#write_csv(refugios,"refugios.csv")
#new_DF <- refugios[is.na(refugios$coordN),]

#a<-paste(new_DF$calle, new_DF$municipio , sep=" ")


