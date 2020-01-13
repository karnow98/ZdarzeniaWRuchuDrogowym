

library(shiny)
library(datasets)
library(RColorBrewer)

#paleta kolorów
n <- 60
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
#inna notacja
options(scipen=999)
#wczytane pliki
data <- read.table("Dni.csv", header = T, sep = ";",row.names = 1)
data2 <- data.matrix(data)
data3 <- read.table("Woj.csv", header = T, sep = ";",row.names = 1)
data4 <- data.matrix(data3)

ui <- fluidPage( 
  
  titlePanel("Wypadki drogowe"),
  sidebarLayout(      
    # Define the sidebar with one input
    sidebarPanel(
      selectInput("region", "Wybierz kat:", 
                  choices=colnames(data2)),
      hr(),
        selectInput("Rok", "Wybierz rok:",
                  choices = c("2018", "2017", "2016")),
      hr(),
      selectInput("Plik", "Wybierz rodzaj danych:",
                  choices = c("Dni", "Woj")),

      helpText("Dane ze strony dane.gov.pl")
    ),
    
    
    
    mainPanel(
      plotOutput("phonePlot"),
      textOutput("Stats")
    )
    
  )
)


server <- function(input, output) {
  
  
  datasetInput <- reactive({
    switch(input$Plik,
           "Dni" = data2,
           "Woj" = data4)
  })
  
  output$Stats <- renderText({ 
    paste("Najmniejsza", input$region, "to", head(sort(datasetInput()[-nrow(datasetInput()),input$region]),1), "w", names(head(sort(datasetInput()[-nrow(datasetInput()),input$region]),1)))
  })
  
  HurtStats <- reactive({
    datasetInput()[length(datasetInput()),"Liczba.Rannych"] + datasetInput()[length(datasetInput()),"Liczba.Zabitych"]
  })
  
  AccidentsStats <- reactive({
    datasetInput()[length(datasetInput()),"Liczba.Kolizji"] + datasetInput()[length(datasetInput()),"Liczba.Wypadków"]
  })
  
  output$phonePlot <- renderPlot({
       barplot(datasetInput()[-nrow(datasetInput()),input$region], 
            main=input$region,
            xlab=input$region,cex.names=0.7, xpd = FALSE,
            las=1, horiz = TRUE,legend = rownames(datasetInput()[-nrow(datasetInput()),]),xlim =  c(0,tail(sort(datasetInput()[-nrow(datasetInput()),input$region]),1) +tail(sort(datasetInput()[-nrow(datasetInput()),input$region]),1)*0.36), 
            col = col_vector) 
  })
}


shinyApp(ui = ui, server = server)