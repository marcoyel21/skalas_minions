library(shiny)

source("Limpieza_Datos.R")
source("refugio_cercano.R")
library(DT)

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
      br(),
      p("Tu refugio más cercano se encuentra aquí:"),
      fluidRow(
        column(8,
               tableOutput('table')
        )),
      leafletOutput("mymap2"),
      br(),
      p("Asimismo, también puedes checar estos otros refugios en tu localidad"),
      br(),
      leafletOutput("mymap3"),
      br(),
      fluidRow(
        column(8,
               DT::dataTableOutput('table2')
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
      ref<-refugio_cercano(abs(ubi[[1]]),abs(ubi[[2]]))
      m2 <-leaflet() %>%
        addTiles() %>%  # Add default OpenStreetMap map tiles
        addMarkers(lng=-ref[[1]], lat=ref[[2]])
      m2
    })
    
    mitabla<-eventReactive(input$go2,{ 
      #simpelemente muestra el data frame filtrado por el id del refugio
      ubi<-paste(input$calle, input$num , input$mun, sep=" ")
      ubi<-geocode(ubi)
      ref<-refugio_cercano(abs(ubi[[1]]),abs(ubi[[2]]))
      tabla<-refugios %>% 
        filter(id==ref[[3]]) %>% 
        select(-coordN,-coordW,-altitud) 
      tabla
      })
    
    ### fucniones de la tarea 3 sobre multiples
    mis_refugios <- eventReactive(input$go2, {
      #extrae la ubicacion para mostrar un mapa con el refugio
      ubi<-paste(input$calle, input$num , input$mun, sep=" ")
      ubi<-geocode(ubi)
      ref<-refugio_cercano(abs(ubi[[1]]),abs(ubi[[2]]))
      refugios_municipio_map(ref[[4]])
    })
    
    mistablas<-eventReactive(input$go2,{ 
      #simpelemente muestra el data frame filtrado por el id del refugio
      ubi<-paste(input$calle, input$num , input$mun, sep=" ")
      ubi<-geocode(ubi)
      ref<-refugio_cercano(abs(ubi[[1]]),abs(ubi[[2]]))
      tabla2<-refugios %>% 
        filter(municipio==ref[[4]]) %>% 
        select(-coordN,-coordW,-altitud) 
      tabla2
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
    
    output$mymap3 <- renderLeaflet({ 
      mis_refugios()
      
    })
    
    output$table2 <- DT::renderDataTable({ mistablas()
    },server=TRUE)
    
    
    #PARTE 3#### Mapa y tabla de lugares cercanos
    
}

shinyApp(ui, server)


