# Loading the needed libraries 
library(shiny)
library(tidyverse)
library(shinythemes)
library(googlesheets4)
library(googledrive)
library(shinyalert)
library(shinyWidgets)
library(bslib)
#setwd('C:/Users/alber/Desktop/UniTn/Data Science/Third Semester/Laboraotry of Business and Customer analytics/Project_Real')

#gs4_auth(cache = ".secrets") #for the first time 
gs4_auth(cache = ".secrets", email = TRUE, use_oob = TRUE) # when you deploy 
(sheet_id <- "1-l3D2dhWjwv1hWXs97db08pJUKZ3DF1DZ4d4yWAVsik")
#sheet_id <- "1MdqGpii3hfoG1OcvlAQjbQ171UOwxCR3Qfc8aIKfZIo"

# Let0s define the demographic variables that will constitute the first part
# of our survey. These infos could be then used for market segmentation

# Defining the Demographic Variable
ages_interval <- c('0-18', '19-25', '25-35', '35-45', '45-55', '55-65', '>65')
sex_interval <-  c('Male', 'Female', 'Other')
educ_interval <- c('Middle school', 'High School', 'Bachelor\'s', 'Master\'s', 'PhD')
Marital_Status <-  c('Married', 'Not Married')
job_sector <-  c('Agricolture', 'Industry', 'Services', 'Other')
# In the following line of code we will implement a function that creates random product
# profiles given the attributes. 
# More precisely, trading platform has been chosen as product of the analysis.
# Now will follow the attributes that we will use to investigate the preferences of 
# possible customers.
platform_type <- c('Web App', 'Desktop App', 'Mobile App')
deposit_minmax <-  c('min 0€ max 1000€', 'min 10€ max 10000€', 'min 100€ max infinte')
fees_on_purchases <- c('0%', '0.015%', '0.025%')
#https://www.investopedia.com/terms/f/financialinstrument.asp
financial_instruments <-  c('Stocks', 'Crypto', 'ETFs', 'Commodities')
leverage <-  c('YES', 'NO')
social_copy <-  c('YES', 'NO')
n_a <-  5
# Now that we have defined the attributes and their levels we can implement a function 
# that creates random profiles
create_options <-  function(){
  
  list_prod <-  c()
  
  for(i in 1:1000){
    # initialize the product profile
    prod_prof <- c(
      paste('Platform Type:', sample(platform_type,1), '|',
            'Amount of Deposit:', sample(deposit_minmax,1), '|',
            'Fees on buy & sell orders:', sample(fees_on_purchases,1), '|',
            'Financial Instruments:', sample(financial_instruments,1), '|',
            'Leverage:', sample(leverage,1), '|', 
            'Social/Copy Trading', sample(social_copy,1))
    )
    # in order to avoid clones
    if (is.element(prod_prof, list_prod) == FALSE){
      list_prod <- append(prod_prof, list_prod)
    }
  }
  return  (list_prod)
}


################################################################################

# START DEVELOPING THE APP 

# User Interface
ui <- fluidPage( 
  # Theme
  theme = shinytheme("cerulean"),
  # Creating a navigation bar
  navbarPage( h1('Trading App Survey'),
    tabPanel( h3('Survey'),
              h5('You may have noticed that filling up your car, going to the hairdresser 
       and shopping for groceries have become more expensive lately.
       While some things are cheaper than a year ago, overall we are paying more for what we buy. 
       This is what we call inflation.
       This November 2021 inflation hit its highest level in 13 years. This is happening 
       for three main reasons: our economy is reopening fast, higher energy prices are pushing up
       inflation, and something that statisticians call the "base effect". Due to inflation there is
       the risk that the value of your savings decreases.'),
              
              h5('At the same time, there is an increasing interest in share-trading apps.'),
              
              h5('For the above reasons this could be the right time to invest and these relatively 
       new share-trading apps could be the "sexiest" solutions for small investors.'),
              
              h5('Hence, with the following questionnaire, we would like to understand what you customers find relevant for these apps. '),
              
              h5('Please remember that this survey HAS NO MARKETING PURPOSES and it is 
       thought just for EDUCATIONAL PURPOSES. Lastly, this survey is fully compliant with GDPR.'),
              
              h4('NB: In the Appendix section are provided some definitions in order to make less experienced
       customers more confortable.'),
              
              # Page Title 
              h3("Demographic Questions"),
              
              tags$head(tags$style(type='text/css', ".span4 { max-width: 600px; }")),
              # Age 
              selectInput("Age", "Please indicate your age", ages_interval,multiple = FALSE),
              
              # Gender
              selectInput("Gender", "Please indicate your gender", sex_interval, multiple = FALSE),
              
              # Education 
              selectInput("Education", "Please indicate your education level", educ_interval, multiple = FALSE),
              
              # Job 
              selectInput("Job", "Please indicate your job sector", job_sector, multiple = FALSE),
              
              # Home Country
              textInput('Country', 'Please type the name of your home country (en)'),
              
              # Marital Status
              radioButtons("Marital_Status", "What is your marital status?", Marital_Status),
              
              # Start the actual survey
              
              h2('Questionnaire on product preferences'),
              
              # 1st Question 
              uiOutput("random_choices1"),
              # 2nd Question 
              uiOutput("random_choices2"),    
              # 3rd Question 
              uiOutput("random_choices3"),    
              # 4th Question 
              uiOutput("random_choices4"),    
              # 5h Question 
              uiOutput("random_choices5"),    
              # 6th Question 
              uiOutput("random_choices6"),    
              # 7th Question 
              uiOutput("random_choices7"),    
              # 8th Question 
              uiOutput("random_choices8"),    
              # 9th Question 
              uiOutput("random_choices9"),    
              # 10th Question 
              uiOutput("random_choices10"),    
              # 11th Question 
              uiOutput("random_choices11"),    
              # 12th Question 
              uiOutput("random_choices12"),    
              #downloadButton('Results', label = 'Conclude the survye'),
              useShinyalert(),
              
              ('Please Submit only Once. It may take few seconds !'),
              
              actionButton("submit", "Submit"),
              
              
   
    
  ), 
  tabPanel(h3('Appendix'),
           h2('Glossary'),
           
           h5('In this page you will find some definitions that can help you to better
              understand what share-trading apps are and to have a first dive into 
              the online investment-trading world.'),
           
           h4('ONLINE TRADING'),
           
           h5('An eletronic trading platform is a piece of software that allows users
              to place orders for financial products over a network with financial
              intermediary. These products includes products such as stocks, bonds,
               currencies, commodities and derivatives.'),
           
           
           h4('PLATFORM TYPE'),
           
           h5('With platform type we simply mean the platform that will host our app: 
              a web app is an application software that runs on a server and to use it you
              need a browser and internet connection,
              a desktop app instead is a computer-based software program that runs
              locally on an operation system, 
              lastly, a mobile app is a software application designed to run on a mobile device
              such as a phone or a tablet.'),
           
           
           h4('DEPOSIT'),
           
           h5('With deposit we refer to the amount of money that you want to invest 
              through your trading app. Some trading platforms set a minimum amount 
              of money needed to open your account and more rarely also a maximum amount
              of money.'),
           
           
           h4('FEES ON PURCHASE'),
           
           h5('Fees on purchase are simply a percentage of money that the trading platform
              retains.'),
           
           
           h4('FINANCIAL INSTRUMENTS'),
           
           h5('Financial Instruments are monetary contracts between parties. They can be 
              created, traded, modified and settled. They can be cash (currences), debts (bonds
              loans), equity (shares), or derivatives'),
           
           
           h5('SHARES'),
           
           h5('A share is a unit used as mutal funds, limited partnerships, and real estate
              investment trusts. A share is an indivisible unit of capital, expressing the ownership
              relationship between the company and the shareholder.'),
           
           
           h5('COMMODITIES'),
           
           h5('A commodity is an economic good, usually a resource, that has full or
              substantial fungibility. The price of a commodity good is typically determined
              as a function of its market as a whole: well-established physical commodities
              have actively traded spot and detivative markets.'),
           
           
           h5('CRIPTO'),
           
           h5(' cryptocurrency is a form of digital asset based on a network that is distributed across a large number of computers. This
              decentralized structure allows them to exist outside the control of governments and central authorities.'),
           
           
           
           h5('ETF'),
           
           h5('An exchange traded fund (ETF) is a type of security that tracks an index, sector, commodity, 
              or other asset, but which can be purchased or sold on a stock exchange the same way a regular 
              stock can. An ETF can be structured to track anything from the price of an individual commodity 
              to a large and diverse collection of securities. ETFs can even be structured to track specific investment strategies.'),
           
           
           h4('SOCIAL TRADING'),
           
           h5('Social trading is a form of investing that allows investors to observe the
              trading behaviour of their peers and expert traders. The primary objective
              is to follow their investment strategies using copy trading or mirror trading.'),
           
           
           h4('FINANCIAL LEVERAGE'),
           
           h5('In finance, leverage is any technique involving using debt rather
              than fresh equity in the purchase of an asset, with the expectation that the after-tax
              profit to equity holders from the transaction will exceed the borrowing cost,
              frequently by several multiples. In other words, leveraging enables gains to be 
              multiplied. On the other hand, losses are also multiplied, and there is a risk that leveraging will result in a loss if financing costs exceed the income from the assset.'),
           
           
           
          
            )) )
  




# Define server function  
server <- function(input, output) {
  s1 <-  c(sample(create_options(), 3, replace = F))
  s2 <-  c(sample(create_options(), 3, replace = F))
  s3 <-  c(sample(create_options(), 3, replace = F))
  s4 <-  c(sample(create_options(), 3, replace = F))
  s5 <-  c(sample(create_options(), 3, replace = F))
  s6 <-  c(sample(create_options(), 3, replace = F))
  s7 <-  c(sample(create_options(), 3, replace = F))
  s8 <-  c(sample(create_options(), 3, replace = F))
  s9 <-  c(sample(create_options(), 3, replace = F))
  s10<-  c(sample(create_options(), 3, replace = F))
  s11<-  c(sample(create_options(), 3, replace = F))
  s12<-  c(sample(create_options(), 3, replace = F))
  
  output$random_choices1 <- renderUI(prettyRadioButtons("Choice1",
                                                        "Which product do you prefer ? ",
                                                        c(s1[1], s1[2], s1[3]),
                                                        icon = icon("check")
  ))
  
  
  output$random_choices2 <- renderUI(prettyRadioButtons("Choice2",
                                                        "Which product do you prefer ? ",
                                                        c(s2[1], s2[2], s2[3]),
                                                        icon = icon("check")
  ))
  output$random_choices3 <- renderUI(prettyRadioButtons("Choice3",
                                                        "Which product do you prefer ?",
                                                        c(s3[1], s3[2], s3[3]),
                                                        icon = icon("check")
  ))
  
  output$random_choices4 <- renderUI(prettyRadioButtons("Choice4",
                                                        "Which product do you prefer ? ",
                                                        c(s4[1], s4[2], s4[3]),
                                                        icon = icon("check")
  ))
  output$random_choices5 <- renderUI(prettyRadioButtons("Choice5",
                                                          "Which product do you prefer ? ",
                                                        c(s5[1], s5[2], s5[3]),
                                                          icon = icon("check")
  ))
  
  output$random_choices6 <- renderUI(prettyRadioButtons("Choice6",
                                                        "Which product do you prefer ? ",
                                                        c(s6[1], s6[2], s6[3]),
                                                        icon = icon("check")
  ))
  output$random_choices7 <- renderUI(prettyRadioButtons("Choice7",
                                                          "Which product do you prefer ? ",
                                                        c(s7[1], s7[2], s7[3]),
                                                          icon = icon("check")
  ))
  
  output$random_choices8 <- renderUI(prettyRadioButtons("Choice8",
                                                        "Which product do you prefer ? ",
                                                        c(s8[1], s8[2], s8[3]),
                                                        icon = icon("check")
  ))
  output$random_choices9 <- renderUI(prettyRadioButtons("Choice9",
                                                          "Which product do you prefer ? ",
                                                        c(s9[1], s9[2], s9[3]),
                                                          icon = icon("check")
  ))
  
  output$random_choices10 <- renderUI(prettyRadioButtons("Choice10",
                                                        "Which product do you prefer ? ",
                                                        c(s10[1], s10[2], s10[3]),
                                                        icon = icon("check")
  ))
  
  output$random_choices11 <- renderUI(prettyRadioButtons("Choice11",
                                                          "Which product do you prefer ?",
                                                         c(s11[1], s11[2], s11[3]),
                                                          icon = icon("check")
  ))
  
  output$random_choices12 <- renderUI(prettyRadioButtons("Choice12",
                                                        "Which product do you prefer ?",
                                                        c(s12[1], s12[2], s12[3]),
                                                        icon = icon("check")
  ))
  
  
 
  
  observeEvent(input$submit, {
    
    results_d <- data.frame(input$Age, input$Gender, input$Education, input$Job, 
                            input$Country, input$Marital_Status)
    
    sheet_append(data = results_d, ss = sheet_id, sheet = 'Demography')
    
    results_s <- data.frame(input$Choice1, input$Choice2, input$Choice3, 
                            input$Choice4, input$Choice5, input$Choice6,
                            input$Choice7, input$Choice8, input$Choice9,
                            input$Choice10, input$Choice11, input$Choice12)
    
    sheet_append(data = results_s, ss = sheet_id, sheet = 'Survey_Answers')
    
    sheet_append(data = as.data.frame(s1), ss = sheet_id, sheet = 'Choices')
    sheet_append(data = as.data.frame(s2), ss = sheet_id, sheet = 'Choices')
    sheet_append(data = as.data.frame(s3), ss = sheet_id, sheet = 'Choices')
    sheet_append(data = as.data.frame(s4), ss = sheet_id, sheet = 'Choices')
    sheet_append(data = as.data.frame(s5), ss = sheet_id, sheet = 'Choices')
    sheet_append(data = as.data.frame(s6), ss = sheet_id, sheet = 'Choices')
    sheet_append(data = as.data.frame(s7), ss = sheet_id, sheet = 'Choices')
    sheet_append(data = as.data.frame(s8), ss = sheet_id, sheet = 'Choices')
    sheet_append(data = as.data.frame(s9), ss = sheet_id, sheet = 'Choices')
    sheet_append(data = as.data.frame(s10), ss = sheet_id, sheet = 'Choices')
    sheet_append(data = as.data.frame(s11), ss = sheet_id, sheet = 'Choices')
    sheet_append(data = as.data.frame(s12), ss = sheet_id, sheet = 'Choices')
    shinyalert("Thank you!", "Your answers have been collected. You can close the survey", type = "success")
    
    })
  
  
    
  
}

# Create Shiny object
shinyApp(ui = ui, server = server)

