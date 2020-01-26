library(shiny)
library(datasets)
library(RColorBrewer)
library(shinydashboard)
#library(shinyBS)

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


listfiles <- list(Mie2016,Mie2017,Mie2018)
#Ogólne statystyki poszkodowanych dla wszystkich lat(Ranni i Zabici)
YearsHurtStats <- ({
  value <- 0
  for(i in listfiles)
  {
    value <- value + i[nrow(i),"Liczba.Rannych"] + i[nrow(i),"Liczba.Zabitych"]
  }
  value
  
})
# Ilość wypadków i kolizji dla wszystkich lat
YearsAccidentsStats <- ({
  value <- 0
  for(i in listfiles)
  {
    value <- value + i[nrow(i),"Liczba.Wypadkow"] + i[nrow(i),"Liczba.Kolizji"]
  }
  value
})


ui <-dashboardPage( skin="purple",
  dashboardHeader(title = "Wypadki Drogowe",disable = FALSE, .list = NULL),
  dashboardSidebar(
      selectInput("region", "Wybierz kat:", 
                  choices=colnames(data3)),
      selectInput("Rok", "Wybierz rok:",
                  choices = c("2018", "2017", "2016")),
      selectInput("Plik", "Wybierz rodzaj danych:",
                  choices = c("Godziny","Dni", "Miesiace", "Wojewodztwa" ), selected = "Dni"),
      
      span(actionButton("zmiany","Porównanie"),
           style="position:absolute;left:2em;"),
      span(actionButton("top","TOP 3"),
            style="position:absolute;right:2em;"),
      br(),
      br(),
      hr(),
      helpText("Dane ze strony dane.gov.pl",align="center")
  ),
  dashboardBody(
      plotOutput("phonePlot"),
      #textOutput("Stats")
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
  
  
  
  datasetInputBefore <- reactive({
    switch(input$Rok,
           "2016" = 
             {
               
             },
           "2017" =
             {
               switch(input$Plik,
                      "Godziny"= Godz2016,
                      "Dni" = Dni2016,
                      "Wojewodztwa" = Woj2016,
                      "Miesiace" = Mie2016,
                      
               )
             },
           "2018" =
             {
               switch(input$Plik,
                      "Godziny"= Godz2017,
                      "Dni" = Dni2017,
                      "Wojewodztwa" = Woj2017,
                      "Miesiace" = Mie2017,
                      
               )
             }
           
    )
  })

  
  #output$Stats <- renderText({ 
   #paste("Najmniejsza", input$region, "to", head(sort(datasetInput()[-nrow(datasetInput()),input$region]),1), "w", names(head(sort(datasetInput()[-nrow(datasetInput()),input$region]),1)))
  # paste("Test statystyk", YearsHurtStats(), AccidentsStats())
  #}) ## sprawdzanie którego używałem do wypisywania danych plus 
  
  Top3forCategoryinFileValue<- reactive({
    head(sort(datasetInput()[-nrow(datasetInput()),input$region]),3)
  }) #wartości trzech najlepszych słupków w pokazanym wykresie
  
  
  Top3forCategoryinFileValueBefore <- reactive({
    if (is.null(datasetInputBefore())) {
      head(sort(datasetInput()[-nrow(datasetInput()),input$region]),3)
    } else {
      head(sort(datasetInputBefore()[-nrow(datasetInputBefore()),input$region]),3)
    }
    
  }) #wartości trzech najlepszych wyników z poprzedniego roku
  
  
  Top3forCategoryinFileNames <- reactive({
    names(head(sort(datasetInput()[-nrow(datasetInput()),input$region]),3))
  }) #nazwy powyższych najlepszych wartości
  
  Top3forCategoryinFileNamesBefore <- reactive({
    names(head(sort(datasetInputBefore()[-nrow(datasetInputBefore()),input$region]),3))
  }) #nazwy powyższych najlepszych wartości
  
  
  HurtStats <- reactive({
    datasetInput()[nrow(datasetInput()),"Liczba.Rannych"] + datasetInput()[nrow(datasetInput()),"Liczba.Zabitych"]
  }) 
  
  AccidentsStats <- reactive({
    datasetInput()[nrow(datasetInput()),"Liczba.Kolizji"] + datasetInput()[nrow(datasetInput()),"Liczba.Wypadkow"]
  }) # Ilosc wypadkow dla tego rodzaju pliku(np Dni) w danym roku
  
  
  observeEvent(input$top,{
    showModal(modalDialog(
      title="Top 3",
      output$Stats <- renderText({ 
          paste("Najmniejsza", input$region, "to",Top3forCategoryinFileValue()[1], "w",
                Top3forCategoryinFileNames()[1],"(", round(((Top3forCategoryinFileValueBefore()[1]-Top3forCategoryinFileValue()[1])/Top3forCategoryinFileValue()[1])*100,2),"%)")
        }),
      output$Stats <- renderText({ 
          paste("Druga najmniejsza", input$region, "to",Top3forCategoryinFileValue()[2], "w",
                Top3forCategoryinFileNames()[2],"(", round(((Top3forCategoryinFileValueBefore()[2]-Top3forCategoryinFileValue()[2])/Top3forCategoryinFileValue()[2])*100,2),"%)")
      }),
      output$Stats <- renderText({ 
          paste("A trzecia najmniejsza to ", input$region, "to",Top3forCategoryinFileValue()[3], "w",
                Top3forCategoryinFileNames()[3],"(", round(((Top3forCategoryinFileValueBefore()[3]-Top3forCategoryinFileValue()[3])/Top3forCategoryinFileValue()[3])*100,2),"%)")
      }),
      easyClose = TRUE,
      footer = tagList(
        modalButton("OK"),
      )
    ))
  })
  
  #, "W porównaniu do zeszłego roku", head(sort(datasetInputBefore()[-nrow(datasetInputBefore()),input$region]),3)[1], "i zmalało/zwiększyło o",round((head(sort(datasetInputBefore()[-nrow(datasetInputBefore()),input$region]),3)[1]-head(sort(datasetInput()[-nrow(datasetInput()),input$region]),3)[1])/head(sort(datasetInput()[-nrow(datasetInput()),input$region]),3)[1]*100,2),"%"
  
  
  
  output$phonePlot <- renderPlot({
       barplot(datasetInput()[-nrow(datasetInput()),input$region], 
            main=input$region,
            xlab=input$region,cex.names=0.7, xpd = FALSE,
            las=1, horiz = TRUE,legend = rownames(datasetInput()[-nrow(datasetInput()),]),xlim =  c(0,tail(sort(datasetInput()[-nrow(datasetInput()),input$region]),1) +tail(sort(datasetInput()[-nrow(datasetInput()),input$region]),1)*0.36), 
            col = col_vector) 
  })
}


shinyApp(ui = ui, server = server)