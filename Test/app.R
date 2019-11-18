#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
# Eryk Olszewski dopisuje sie :p

library(shiny)

# Define UI for dataset viewer app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Wypadki Samochodowe"),
  
  # Sidebar layout with a input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Selector for choosing dataset ----
      selectInput(inputId = "dataset",
                  label = "Wybierz kategorię:",
                  choices = c("Wypadki", "")),
      
      selectInput(inputId = "dzien",
                  label="Dzień Tygodnia:",
                  choices = c("Poniedziałek", "Wtorek", "Środa", "Czwartek", "Piątek", "Sobota", "Niedziela")),
      
      # Input: Numeric entry for number of obs to view ----      
      numericInput(inputId = "obs",
                   label = "Number of observations to view:",
                   value = 10)
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Verbatim text for data summary ----
#      verbatimTextOutput("summary"),

      # Output: HTML table with requested number of observations ----
      tableOutput("view")
      
    )
  )
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output) {
  data <- read.table("Dni.csv", header = T, sep = ";")
  # Return the requested dataset ----
  datasetInput <- reactive({
    switch(input$dataset,
#           "rock" = rock,
           "Wypadki" = data
#,
#           "cars" = cars
)
  })
  
  # Generate a summary of the dataset ----
  output$summary <- renderPrint({
    data <- read.table("Dni.csv", header = T, sep = ";")
      summary(data)
   })
  data <- read.csv("Dni.csv", header = T, sep = ";")

  
  # Show the first "n" observations ----
  output$view <- renderTable({
    head(datasetInput(), n = input$obs)
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)