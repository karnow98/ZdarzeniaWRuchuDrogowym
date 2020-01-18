

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
data <- read.table("Bazy/2016/Dni_tygodnia.csv", header = T, sep = ";",row.names = 1)
Dni2016 <- data.matrix(data)
data3 <- read.table("Bazy/2017/Dni_tygodnia.csv", header = T, sep = ";",row.names = 1)
Dni2017 <- data.matrix(data3)
data3 <- read.table("Bazy/2018/Dni_tygodnia.csv", header = T, sep = ";",row.names = 1)
Dni2018 <- data.matrix(data3)

data3 <- read.table("Bazy/2016/Godziny.csv", header = T, sep = ";",row.names = 1)
Godz2016 <- data.matrix(data3)
data3 <- read.table("Bazy/2017/Godziny.csv", header = T, sep = ";",row.names = 1)
Godz2017 <- data.matrix(data3)
data3 <- read.table("Bazy/2018/Godziny.csv", header = T, sep = ";",row.names = 1)
Godz2018 <- data.matrix(data3)

data3 <- read.table("Bazy/2016/Województwa.csv", header = T, sep = ";",row.names = 1)
Woj2016 <- data.matrix(data3)
data3 <- read.table("Bazy/2017/Województwa.csv", header = T, sep = ";",row.names = 1)
Woj2017 <- data.matrix(data3)
data3 <- read.table("Bazy/2018/Województwa.csv", header = T, sep = ";",row.names = 1)
Woj2018 <- data.matrix(data3)

data3 <- read.table("Bazy/2016/Miesiące.csv", header = T, sep = ";",row.names = 1)
Mie2016 <- data.matrix(data3)
data3 <- read.table("Bazy/2017/Miesiące.csv", header = T, sep = ";",row.names = 1)
Mie2017 <- data.matrix(data3)
data3 <- read.table("Bazy/2018/Miesiące.csv", header = T, sep = ";",row.names = 1)
Mie2018 <- data.matrix(data3)


listfiles <- list(data2, data4)

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
                  choices = c("Godziny","Dni", "Miesiace", "Wojewodztwa" )),

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
    switch(input$Rok,
           "2016" = 
             {
               switch(input$Plik,
                      "Godziny"= Godz2016,
                      "Dni" = Dni2016,
                      "Wojewodztwa" = Woj2016,
                      "Miesiace" = Mie2016,
                      
                        )
               
             },
           "2017" =
             {
                 switch(input$Plik,
                        "Godziny"= Godz2017,
                        "Dni" = Dni2017,
                        "Wojewodztwa" = Woj2017,
                        "Miesiace" = Mie2017,
                        
                 )
               },
           "2018" =
             {
               switch(input$Plik,
                      "Godziny"= Godz2018,
                      "Dni" = Dni2018,
                      "Wojewodztwa" = Woj2018,
                      "Miesiace" = Mie2018,
                      
               )
             }
           
           )
  })
  
  output$Stats <- renderText({ 
  #  paste("Najmniejsza", input$region, "to", head(sort(datasetInput()[-nrow(datasetInput()),input$region]),1), "w", names(head(sort(datasetInput()[-nrow(datasetInput()),input$region]),1)))
   # paste("Test statystyk", YearsHurtStats(), AccidentsStats())
  }) ## sprawdzanie którego używałem do wypisywania danych plus 
  
  Top3forCategoryinFileValue <- reactive({
    head(sort(datasetInput()[-nrow(datasetInput()),input$region]),1)
  }) #wartosći trzech najlepszych słupków w pokazanym wykresie
  
  Top3forCategoryinFileNames <- reactive({
    names(head(sort(datasetInput()[-nrow(datasetInput()),input$region]),1))
  }) #nazwy powyższych najlepszych wartości
  
  
  HurtStats <- reactive({
    datasetInput()[nrow(datasetInput()),"Liczba.Rannych"] + datasetInput()[nrow(datasetInput()),"Liczba.Zabitych"]
  }) 
  
  AccidentsStats <- reactive({
    datasetInput()[nrow(datasetInput()),"Liczba.Kolizji"] + datasetInput()[nrow(datasetInput()),"Liczba.Wypadkow"]
  }) # Wyszystkie lata dla tego rodzaju pliku(np Dni)
  
  YearsHurtStats <- reactive({
    value <- 0
    for(i in listfiles)
    {
      value <- value + i[nrow(i),"Liczba.Rannych"] + i[nrow(i),"Liczba.Zabitych"]
    }
    value
    
  })
  
  YearsAccidentsStats <- reactive({
    value <- 0
    for(i in listfiles)
    {
      value <- value + i[nrow(i),"Liczba.Wypadkow"] + i[nrow(i),"Liczba.Kolizji"]
    }
    value
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