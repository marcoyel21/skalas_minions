library(geosphere)
library(readr)
library(leaflet)
library(dplyr)
library(purrr)
#refugios<-read_csv("refugios.csv")
r2 <- refugios %>% filter(id != 337)%>%
  filter(id != 75)%>% 
  filter(id != 83)%>% 
  filter(id != 84)%>% 
  filter(id != 338)%>% 
  filter(id != 336)%>% 
  filter(id != 118)%>% 
  filter(id != 286)%>% 
  filter(id != 213)%>% 
  filter(id != 283)%>% 
  filter(id != 121)%>% 
  filter(id != 310)%>% 
    filter(id != 281)

#r2<-tail(r2,1)


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
  
  head(r2[order(r2$distancia,decreasing = FALSE),],1) %>% select(w,n,id,municipio)

}

#####con municipio#####
refugios_municipio <- function(mun) {
  coord <- r2 %>% filter(municipio==mun) %>% select(w,n) 
  m <- leaflet() %>%
    addTiles() %>%  # Add default OpenStreetMap map tiles
    addMarkers(lng=-coord[[1]], lat=coord[[2]])
  m  # Print the map
  
}



