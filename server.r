
# Loading the needed libraries 
library(shiny)
library(shinysurveys)
library(shinydashboard)
library("googlesheets4")
library("DT")
library(gargle)
library(shinythemes)
library(googledrive)

server <- function(input, output) {
  
  
  observeEvent(input$submit, {
    
    results_d <- data.frame(input$Age, input$Gender, input$Education, 
                            input$Country, input$Marital_Status)
    
    sheet_append(data = results_d, ss = survey_url, sheet = 'Demography')
    
    results_s <- data.frame(input$Choice1, input$Choice2, input$Choice3, 
                            input$Choice4, input$Choice5, input$Choice6,
                            input$Choice7, input$Choice8, input$Choice9,
                            input$Choice10, input$Choice11, input$Choice12)
    sheet_append(data = results_s, ss = survey_url, sheet = 'Survey_Answers')
  })
  
}

# Create Shiny object
shinyApp(ui = ui, server = server)
