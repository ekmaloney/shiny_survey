#load libraries
library(mwshiny)
library(shiny)
library(shinydashboard)
library(googlesheets4)
library(tidyverse)
library(DT)
library(lubridate)

#get your token to access google drive
options(httr_oob_default = TRUE)
shiny_token <- sheets_auth(email = "emily.maloney24@gmail.com")
shiny_token <- "4/1QEqKINqTBdcwg3GaX9pPwBQOzQ_TuPjf7or6Gj3tR-uxTelCQuoaqI"
saveRDS(shiny_token, "shiny_app_token.rds")

pfs <- c("Facebook", "Instagram", "Twitter", "TikTok")
g <- c("Man", "Woman", "Transgender", "Nonbinary", "Other")
r <- c("White", "Black", "Asian", "American Indian or Alaska Native",
       "Native Hawaiian or Other Pacific Islander", "Other")
e <- c("Hispanic/Latino", "Not Hispanic or Latino")

SampleData <-  as.data.frame(list(Var1=1:1000, Var2=rnorm(1, 1000, 1000)))

rDate <- function(sDate, eDate, SampleData){   
  lenDate <- dim(SampleData)[1]    
  seqDays <- seq.POSIXt(as.POSIXct(sDate), as.POSIXct(eDate), by="secs")  
  aDay <- runif(lenDate, 1, length(seqDays))  
  Date <- seqDays[aDay]  
}

SampleData$TimeStamp <- rDate("2020-06-22", "2020-06-23", SampleData)
SampleData <- SampleData[order(SampleData$TimeStamp), ]
row.names(SampleData) <- NULL
head(SampleData)

#make empty data frame
df <- tibble(code = sample(1:4, 1000, replace=TRUE),
             platform = sample(pfs, 1000, replace = TRUE),
             sex = sample(g, 1000, replace = TRUE),
             race = sample(r, 1000, replace = TRUE),
             ethnicity = sample(e, 1000, replace = TRUE),
             age = sample(25:40, 1000, replace = TRUE),
             timestamp = SampleData$TimeStamp)

#make the google sheet with this information
ss2 <- gs4_create("Shiny Data Collaboration Challenge",
                  sheets = df)


# Define UI for survey app ----
ui <- pageWithSidebar(
  
  # App title ----
  headerPanel("Mobilization Challenge"),
  
  # Sidebar panel for inputs ----
  sidebarPanel(
    
    #Input 1: code 
    textInput("code", 
              "Enter the code assigned to the person who alerted you to this challenge.",
              value = 0),
    
    #Input 2: Platform
    selectInput("platform", 
                "On which social media platform did you find out about this challenge?",
                choices = c("Facebook", "Instagram", "Twitter", "TikTok")),
    
    #Input 3: Gender
    checkboxGroupInput("gender",
                       "What gender do you identify as? (select all that apply)",
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
                 value = 20,
                 min = 0, max = 100, step = 1),
    
    actionButton("submit", "Submit")
  ),
  
  # Main panel for displaying outputs ----
  mainPanel(
    plotOutput("group_plot"),
    tableOutput("all_values")
  )
)

# Define server logic to plot various variables against mpg ----
server <- function(input, output) {
  
  #save input as a data frame
  indv_values <- reactive({
    tibble(code = input$code,
           platform = input$platform,
           gender = input$gender,
           race = input$race,
           ethnicity = input$ethnicity,
           age = input$age,
           timestamp = Sys.time())
    
  })
  
  #first add data to the google sheet
  observeEvent(input$submit, {           
    sheet_append(ss2, data = indv_values())
    
    df <- read_sheet(ss2)
    df <- df %>% 
      group_by(code) %>% 
      mutate(n = 1,
             csum = cumsum(n))
    
    sum_table <- df %>% count(code)
    
    output$all_values <- renderTable({
      sum_table
    })
    
    output$group_plot <- renderPlot({
      ggplot(df, mapping = aes(x = timestamp, y = csum)) + geom_line() + facet_wrap(~code)
    })
    
  }
  )
}

shinyApp(ui, server)
