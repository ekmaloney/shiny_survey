#load libraries
library(shiny)
library(shinydashboard)
library(googlesheets4)
library(tidyverse)
library(DT)

#get your token to access google drive
options(httr_oob_default = TRUE)
shiny_token <- sheets_auth(email = "emily.maloney24@gmail.com")
shiny_token <- "4/1QEqKINqTBdcwg3GaX9pPwBQOzQ_TuPjf7or6Gj3tR-uxTelCQuoaqI"
saveRDS(shiny_token, "shiny_app_token.rds")

Data <- gs_new("Data") %>% 
  gs_ws_rename(from = "Sheet1", to = "Data")    

df <- tibble(code = NA,
             race = NA,
             sex = NA,
             age = NA,
             platform = NA,
             timestamp = NA)

ss2 <- gs4_create("Shiny Data",
                  sheets = df)


# Define UI for survey app ----
ui <- pageWithSidebar(
  
  # App title ----
  headerPanel("Mass Collaboration Challenge"),
  
  # Sidebar panel for inputs ----
  sidebarPanel(
    
    #Input 1: code 
    textInput("code", 
              "Enter the code assigned to the person who alerted you to this challenge.",
              value = NA),
    
    #Input 2: Platform
    selectInput("platform", 
                "On which social media platform did you find out about this challenge?",
                choices = c("Facebook", "Instagram", "Twitter", "TikTok")),
    
    #Input 3: Gender
    selectInput("gender",
                "What gender do you identify as?",
                choices = c("Man", "Woman", "Transgender", "Nonbinary", "Other")),
    
    #Input 4: Race
    checkboxGroupInput("race", 
                       "What race are you? (select all that apply)",
                       choices = c("White", "Black", "Asian", "American Indian or Alaska Native",
                                   "Native Hawaiian or Other Pacific Islander", "Other")),
    #Input 5: Ethnicity
    checkboxGroupInput("ethnicity",
                       "What is your ethnicity?",
                       choices = c("Hispanic/Latino", "Not Hispanic or Latino")),
    
    #Input 6: Age
    numericInput("age",
                 "How old are you?",
                 min = 0, max = 100, step = 1),
    
    actionButton("submit", "Submit")
  ),
  
  # Main panel for displaying outputs ----
  mainPanel()
)

# Define server logic to plot various variables against mpg ----
server <- function(input, output) {
  
}

shinyApp(ui, server)
