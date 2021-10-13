library(geosphere)
#install.packages("leaflet")
library(leaflet)

r2 <- tail(refugios,3)

#limpieza (diagonal final)
r2$coordN <- substr(r2$coordN,1,nchar(r2$coordN)-1)
r2$coordW <- substr(r2$coordW,1,nchar(r2$coordW)-1)

r2 <- r2 %>% mutate(
  w = as.double(map(strsplit(r2$coordW,"([º,'])"),1)) + #grados
    as.double(map(strsplit(r2$coordW,"([º,'])"),2))/60 + #minutos
    round(as.double(map(strsplit(r2$coordW,"([º,'])"),3)))/3600, #segundos
  n = as.double(map(strsplit(r2$coordN,"([º,'])"),1)) + 
    as.double(map(strsplit(r2$coordN,"([º,'])"),2))/60 + 
    round(as.double(map(strsplit(r2$coordN,"([º,'])"),3)))/3600)



#####con coordenadas#####
refugio_cercano <- function (long,lat){
  
  r2 <- r2 %>% mutate(distancia = distVincentyEllipsoid(c(long,lat),  r2[,c('w','n')])/1000) #en km
  
  head(r2[order(r2$distancia,decreasing = TRUE),],1) %>% select(w,n)

}

ref <- refugio_cercano(150.234,23.450) #ejemplo

m <- leaflet() %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addMarkers(lng=-ref[[1]], lat=ref[[2]], popup="Refugio más cercano")
m  # Print the map


#####con municipio#####
refugios_municipio <- function(municipio) {
  coord <- r2 %>% filter(r2$municipio==municipio) %>% select(w,n) 
  m <- leaflet() %>%
    addTiles() %>%  # Add default OpenStreetMap map tiles
    addMarkers(lng=-coord[[1]], lat=coord[[2]])
  m  # Print the map
  
}
