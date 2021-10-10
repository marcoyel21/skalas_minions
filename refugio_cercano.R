library(geosphere)

r2 <- tail(refugios,3)

refugio_cercano <- function (long,lat){
  #limpieza (diagonal final)
  r2$coordN <- substr(r2$coordN,1,nchar(r2$coordN)-1)
  r2$coordW <- substr(r2$coordW,1,nchar(r2$coordW)-1)
  
  #texto a coordenadas
  r2 <- r2 %>% mutate(
                w = as.double(map(strsplit(r2$coordW,"([º,'])"),1)) + #grados
                  as.double(map(strsplit(r2$coordW,"([º,'])"),2))/60 + #minutos
                  round(as.double(map(strsplit(r2$coordW,"([º,'])"),3)))/3600, #segundos
                n = as.double(map(strsplit(r2$coordN,"([º,'])"),1)) + 
                  as.double(map(strsplit(r2$coordN,"([º,'])"),2))/60 + 
                  round(as.double(map(strsplit(r2$coordN,"([º,'])"),3)))/3600)

  r2 <- r2 %>% mutate(distancia = distVincentyEllipsoid(c(long,lat),  r2[,17:18])/1000) #en km
  
  head(r2[order(r2$distancia,decreasing = TRUE),],1)


  
}


