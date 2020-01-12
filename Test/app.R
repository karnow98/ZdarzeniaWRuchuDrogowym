#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
# Eryk Olszewski dopisuje sie :p

library(shiny)
library(datasets)
library(RColorBrewer)
n <- 60
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
options(scipen=999)
data <- read.table("Dni.csv", header = T, sep = ";",row.names = 1)
data2 <- data.matrix(data)
data3 <- read.table("Woj.csv", header = T, sep = ";",row.names = 1)
data4 <- data.matrix(data3)
# Define UI for application that draws a histogram
ui <- fluidPage( 
  
  # Give the page a title
  titlePanel("Wypadki drogowe"),
  # Generate a row with a sidebar
  sidebarLayout(      
    # Define the sidebar with one input
    sidebarPanel(
      selectInput("region", "Wybierz kat:", 
                  choices=colnames(data2)),
      hr(),
        selectInput("Rok", "Wybierz rok:",
                  choices = c("rock", "pressure", "cars")),
      hr(),
      selectInput("Plik", "Wybierz rodzaj danych:",
                  choices = c("Dni", "Woj")),

      helpText("Dane ze strony dane.gov.pl")
    ),
    
    
    
    # Create a spot for the barplot
    mainPanel(
      plotOutput("phonePlot"),
      textOutput("Stats")
    )
    
  )
)


# Define server logic to summarize and view selected dataset ----
server <- function(input, output) {
  
  
  datasetInput <- reactive({
    switch(input$Plik,
           "Dni" = data2,
           "Woj" = data4)
  })
  
  output$phonePlot <- renderPlot({
    # Render a barplot
       barplot(datasetInput()[-nrow(datasetInput()),input$region], 
            main=input$region,
            xlab=input$region,cex.names=0.7, xpd = FALSE,
            las=1, horiz = TRUE,legend = rownames(datasetInput()[-nrow(datasetInput()),]),xlim =  c(0,tail(sort(datasetInput()[-nrow(datasetInput()),input$region]),1) +tail(sort(datasetInput()[-nrow(datasetInput()),input$region]),1)*0.36), 
            col = col_vector) 
  })
}

# Run the application 
shinyApp(ui = ui, server = server)