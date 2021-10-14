library(shiny)

source("Limpieza_Datos.R")
source("refugio_cercano.R")

r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()

ui <- fluidPage(
  # EN esta parte solo se acomoda el output estetico
  titlePanel("Encuentra tu refugio más cercano"),
  sidebarLayout(
    sidebarPanel(
      textInput("calle", "Calle", "Rio Hondo"),
      textInput("num", "Número", "1"),
      textInput("mun", "Municipio", "Alvaro Obregón"),
      actionButton("go", "Detecta mi ubicación"),
      actionButton("go2", "Detecta mi refugio más cercano"),
      leafletOutput("mymap1"),
    ),
    mainPanel(
      p("Con esta aplicacion puedes encontrar tu refugio más cercano."),
      br(),
      p("En primer lugar, encuentra tu ubicación en el cuadro de la izquierda vaciando la calle, número y municipio en el que te encuentras y dando click en \"Detecta mi ubicación\". Te aparecerá un mapa donde se indica tu ubicación actual. Si tu ubicación es la correcta oprime el segundo botón \"Detecta mi refugio más cercano\". Este segundo botón te mostrara un mapa de la ubicación del refugio más cercano y te mostrara información relevante como la dirección, los telefonos, la capacidad del refugio, entre otros aspectos."),
      leafletOutput("mymap2"),
      fluidRow(
        column(8,
               tableOutput('table')
        ))
    ))
  
    
)

server <- function(input, output, session) {
#PARTE 1#### FUNCIONES INTERACTIVAS
  
    mi_ubi <- eventReactive(input$go, {
      #extrae la ubicacion para mostrarla en el mapa
      ubi<-paste(input$calle, input$num , input$mun, sep=" ")
      ubi<-geocode(ubi)
       m1 <-leaflet() %>%
        addTiles() %>%  # Add default OpenStreetMap map tiles
        addMarkers(lng=ubi[[1]], lat=ubi[[2]])
      m1
    })
    
    mi_refugio <- eventReactive(input$go2, {
      #extrae la ubicacion para mostrar un mapa con el refugio
      ubi<-paste(input$calle, input$num , input$mun, sep=" ")
      ubi<-geocode(ubi)
      ref<-refugio_cercano(ubi[[1]],ubi[[2]]) #PAU wtf, como deben entrar las coordenadas?
      m2 <-leaflet() %>%
        addTiles() %>%  # Add default OpenStreetMap map tiles
        addMarkers(lng=-ref[[1]], lat=ref[[2]])
      m2
    })
    
    mitabla<-eventReactive(input$go2,{ 
      #simpelemente muestra el data frame filtrado por el id del refugio
      ubi<-paste(input$calle, input$num , input$mun, sep=" ")
      ubi<-geocode(ubi)
      ref<-refugio_cercano(ubi[[1]],ubi[[2]])#PAU wtf, como deben entrar las co
      tabla<-refugios %>% 
        filter(id==ref[[3]]) %>% 
        select(-coordN,-coordW,-altitud) 
      tabla
      })
    #PARTE 2#### OUTPUT de FUNCIONES INTERACTIVAS
    
    output$mymap1 <- renderLeaflet({
           mi_ubi()
    })
    
    output$mymap2 <- renderLeaflet({
      mi_refugio()
    })
    
    output$table <- renderTable({ mitabla()
      })
}

shinyApp(ui, server)


