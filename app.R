#load libraries
library(mwshiny)
library(shiny)
library(shinydashboard)
library(googlesheets4)
library(tidyverse)
library(DT)
library(lubridate)
library(shinyjs)

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

# named list of ui pages that are the contain the title and content of each of my windows
ui_win <- list()

#consent page

ui_win[['Survey']] <- fluidPage(
  
  #title of page 
  titlePanel("Consent to Participate"),
  useShinyjs(),
  #put the consent text in the main panel
  sidebarPanel(textInput("consent", 
                      "<INSERT CONSENT TO PARTICIPATE TEXT> If you consent to participate in this study, type your name here:",
                      "enter your name"),
            actionButton("consent_submit", "Agree to Participate")),
  div(id = "main",
      mainPanel(
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
        
        actionButton("submit", "Submit"))
      ) %>% shinyjs::hidden()
)

ui_win[['Leaderboard']] <- fluidPage(
  titlePanel("Leaderboard"),
  plotOutput("group_plot"),
  tableOutput("lb_table")
)

# setting up the list of calculations I want to do
serv_calc <- list()

serv_calc[[1]] <- function(calc, sess) {
  
#this activates when consent is submitted
  observeEvent(calc$consent_submit, {
    shinyjs::toggle("main")
  })
}

#make data frames
serv_calc[[2]] <- function(calc, sess){
  
  #bring down all of the data 
  df <- read_sheet(ss2)
  
  #calculate the cumulative sum needed for plots
  df <- df %>% 
    group_by(code) %>% 
    mutate(n = 1,
           csum = cumsum(n))
  
  #calculate the grouped sums for leaderboard
  sum_table <- df %>% count(code)
  
  
  # add this to calc, since we want to use this in our rendering
  calc[["plot.df"]] <- df
  calc[["sum.df"]] <- sum_table
  
  # this is going to activate any time I press "submit"
  observeEvent(calc$submit, {
    
    #save input as a data frame
    indv_values <- reactive({
      tibble(code = calc$code,
             platform = calc$platform,
             gender = calc$gender,
             race = calc$race,
             ethnicity = calc$ethnicity,
             age = calc$age,
             timestamp = Sys.time())
      
    })
    
    #first add data to the google sheet
    sheet_append(ss2, data = indv_values())
    
    #bring down all of the data 
    df <- read_sheet(ss2)
    
    #calculate the cumulative sum needed for plots
    df <- df %>% 
      group_by(code) %>% 
      mutate(n = 1,
             csum = cumsum(n))
    
    #calculate the grouped sums for leaderboard
    sum_table <- df %>% count(code)
    
    
    # add this to calc, since we want to use this in our rendering
    calc[["plot.df"]] <- df
    calc[["sum.df"]] <- sum_table
  })
}

serv_out <- list()

serv_out[["group_plot"]] <- function(calc, sess){
  renderPlot({
    # we add this check to make sure our plot doesn't try to render before we've ever pressed "Build!"
    if (!is.null(calc$plot.df)){
      # build plot
      ggplot(calc$plot.df, mapping = aes(x = timestamp, y = csum)) + geom_line() + facet_wrap(~code)
    }
  })
}

serv_out[["lb_table"]] <- function(calc, sess){
  renderTable({
    # we add this check to make sure our plot doesn't try to render before we've ever pressed "Build!"
    if (!is.null(calc$sum.df)){
      # build table
      calc$sum.df
    }
  })
}


mwsApp(ui_win, serv_calc, serv_out)
