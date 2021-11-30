# Loading the needed libraries 
library(shiny)
library(shinysurveys)
library(shinydashboard)
library("googlesheets4")
library("DT")
library(gargle)
library(shinythemes)
# gs4_auth(cache = ".secrets") for the first time 
gs4_auth(cache = ".secrets", email = TRUE, use_oob = TRUE) # when you deploy 
sheet_id <- "1MdqGpii3hfoG1OcvlAQjbQ171UOwxCR3Qfc8aIKfZIo"

# Defining the Demographic Variable
ages_interval <- c('0-18', '19-25', '25-35', '35-45', '45-55', '55-65', '>65')
sex_interval <-  c('Male', 'Famale', 'Other')
educ_interval <- c('Middle school', 'High School', 'Bachelor Degree', 'Master Degree', 'PhD')
Marital_Status <-  c('Married', 'Not Married')
# Defining the attributes and the levels of the product profiles
n_a <-  4 # number of attributes
attrib1 <-  c('lev1', 'lev2', 'lev3')
attrib2 <-  c('lev1', 'lev2', 'lev3')
attrib3 <-  c('lev1', 'lev2', 'lev3')
attrib4 <-  c('lev1', 'lev2', 'lev3')
# Implement a function to create all possible combination of product profiles
# given n = number of time we want to iterate
create_options <-  function(n){
  list_prod <-  c()#c() # check if it is petter using a vector or a list list()
  #  l_attrib <-  list(attrib1, attrib2, attrib3, attrib4)
  # n is the number of product profiles that we want
  for(i in 1:n){
    # initialize the product profile
    prod_prof <- c(
      paste('Attrib1', sample(attrib1,1), 
            'Attrib2', sample(attrib2,1), 
            'Attrib3', sample(attrib3,1),
            'Attrib4', sample(attrib4,1))
    )
    # in order to avoid clones
    if (is.element(prod_prof, list_prod) == FALSE){
      list_prod <- append(prod_prof, list_prod)
    }
  }
  return  (list_prod)
}

# START DEVELOPING THE APP 

# User Interface
ui <- fluidPage(
  # Sets in order to connect to Google Drive 
  
  # Theme
  theme = shinytheme("cerulean"),
  # Creating a navigation bar
  mainPanel(
    h1('Conjoint Survey'),
    # Page Title 
    h2("Demographic Questions"),
    # Age 
    selectInput(
      "Age", "Please indicate your age", ages_interval,
      multiple = FALSE),
    # Gender
    selectInput(
      "Gender", "Please indicate your gender", sex_interval,
      multiple = FALSE),
    # Education 
    selectInput(
      "Education", "Please indicate your education level", educ_interval,
      multiple = FALSE),
    # Home Country
    textInput('Country', 'Please type the name of your home country (en)'),
    
    # Marital Status
    
    radioButtons("Marital_Status", "What is your marital status?",
                 Marital_Status),
    
    
    
    
    # Navbar 1, tabPanel
    
    h2('Questionnaire on product preferences'),
    # 1st Question 
    checkboxGroupInput('Choice1', 'Which product do you prefer ? \n(Please pick ONLY ONE)', sample(create_options(1000),3, replace = F)),
    
    # 2nd Question 
    checkboxGroupInput('Choice2', 'Which product do you prefer ? \n(Please pick ONLY ONE)', sample(create_options(1000),3, replace = F)),
    
    # 3rd Question 
    checkboxGroupInput('Choice3', 'Which product do you prefer ? \n(Please pick ONLY ONE)', sample(create_options(1000),3, replace = F)),
    
    # 4th Question 
    checkboxGroupInput('Choice4', 'Which product do you prefer ? \n(Please pick ONLY ONE)', sample(create_options(1000),3, replace = F)),
    
    # 5h Question 
    checkboxGroupInput('Choice5', 'Which product do you prefer ? \n(Please pick ONLY ONE)', sample(create_options(1000),3, replace = F)),
    
    # 6th Question 
    checkboxGroupInput('Choice6', 'Which product do you prefer ? \n(Please pick ONLY ONE)', sample(create_options(1000),3, replace = F)),
    
    # 7th Question 
    checkboxGroupInput('Choice7', 'Which product do you prefer ? \n(Please pick ONLY ONE)', sample(create_options(1000),3, replace = F)),
    
    # 8th Question 
    checkboxGroupInput('Choice8', 'Which product do you prefer ? \n(Please pick ONLY ONE)', sample(create_options(1000),3, replace = F)),
    
    # 9th Question 
    checkboxGroupInput('Choice9', 'Which product do you prefer ? \n(Please pick ONLY ONE)', sample(create_options(1000),3, replace = F)),
    
    # 10th Question 
    checkboxGroupInput('Choice10', 'Which product do you prefer ? \n(Please pick ONLY ONE)', sample(create_options(1000),3, replace = F)),
    
    # 11th Question 
    checkboxGroupInput('Choice11', 'Which product do you prefer ? \n(Please pick ONLY ONE)', sample(create_options(1000),3, replace = F)),
    
    # 12th Question 
    checkboxGroupInput('Choice12', 'Which product do you prefer ? \n(Please pick ONLY ONE)', sample(create_options(1000),3, replace = F)),
    
    #downloadButton('Results', label = 'Conclude the survye')
    actionButton("submit", "Submit")
  ),  
  # navbarPage
) # fluidPage



# Define server function  
server <- function(input, output) {
  
  
  observeEvent(input$submit, {
    
    results_d <- data.frame(input$Age, input$Gender, input$Education, 
                            input$Country, input$Marital_Status)
    
    sheet_append(data = results_d, ss = sheet_id, sheet = 'Demography')
    
    results_s <- data.frame(input$Choice1, input$Choice2, input$Choice3, 
                            input$Choice4, input$Choice5, input$Choice6,
                            input$Choice7, input$Choice8, input$Choice9,
                            input$Choice10, input$Choice11, input$Choice12)
    sheet_append(data = results_s, ss = sheet_id, sheet = 'Survey_Answers')
  })
  
}

# Create Shiny object
shinyApp(ui = ui, server = server)

