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
options(scipen=999)
data <- read.table("Dni.csv", header = T, sep = ";",row.names = 1)
data2 <- data.matrix(data)
# Define UI for application that draws a histogram
ui <- fluidPage(    
  
  # Give the page a title
  titlePanel("Wypadki drogowe"),
  # Generate a row with a sidebar
  sidebarLayout(      
    # Define the sidebar with one input
    sidebarPanel(
      selectInput("region", "Kategoria:", 
                  choices=colnames(data2)),
      hr(),
      helpText("Dane ze strony dane.gov.pl")
    ),
    
    # Create a spot for the barplot
    mainPanel(
      plotOutput("phonePlot")
    )
    
  )
)


# Define server logic to summarize and view selected dataset ----
server <- function(input, output) {
  
  output$phonePlot <- renderPlot({
    # Render a barplot
    barplot(data2[,input$region], 
            main=input$region,
            ylab="Liczba Wypadkow",
            xlab="Dni",cex.names=0.9, xpd = FALSE)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)