library(shiny) 
library(tidyverse)
library(ggthemes)
library(DT)
library(shinythemes)

#Reading in covid data

covid <- readRDS("coviddata.RDS")

# Wrangle Social distancing data
social_distancing <- read_csv("data/social_distancing.csv", skip = 2,
                              col_type = cols(Location = col_character(),
                                              `Status of Reopening` = col_character(),
                                              `Stay at Home Order` = col_character(),
                                              `Mandatory Quarantine for Travelers` = col_character(),
                                              `Non-Essential Business Closures` = col_character(),
                                              `Large Gatherings Ban` = col_character(),
                                              `Restaurant Limits` = col_character(),
                                              `Bar Closures` = col_character(),
                                              `Face Covering Requirement` = col_character(),
                                              `Emergency Declaration` = col_character(),
                                              Footnotes = col_character())) %>%
    select(!Footnotes) %>%
    slice(c(-1, -(53:92)))

# making objects
state.names <- c(covid$state[1:51])
column.names <- c("Deaths" = "deaths", "Positive Cases" = "cases", "New Tests" = "totalTestResultsIncrease")


######################################################################################
######################################################################################

ui <- navbarPage(
    "Social Distancing Policies and COVID-19 State Data",
    theme = shinytheme("journal"),
    tabPanel("COVID-19 Data by State",
        fluidPage(
            titlePanel("COVID-19 Data by State"),
            p("This page allows you to view different COVID-19 statistics for each state"),
            sidebarLayout(
                sidebarPanel(
                    selectInput(
                        inputId = "selected_state",                 # a name for the value you choose here
                        label = "Select a State",   # the name to display on the slider
                        choices = c(state.names),                       # your list of choices to choose from
                        selected = "Massachusetts"
                               ),
                    
                    radioButtons(
                        inputId = "selected_variable",             # a name for the value you choose here
                        label = "Choose a variable!",              # the label to display above the buttons
                        choices = column.names, # the button values to choose from
                        selected = "deaths"
                                )
                             ),
                mainPanel(
                    textOutput("state_message"),              # load a text object called "state_message"
                    # textOutput("text_message"),
                   #  plotOutput("covid_death"),
                   #  plotOutput("covid_positive"),
                   # plotOutput("covid_testing")
                   plotOutput("covid_stats_by_state")
                         )
                        )
        ),
        
    ),
    tabPanel("Social Distancing",
             fluidPage(
                 titlePanel("Social Distancing Data"),
                 mainPanel(DT::dataTableOutput("summary")
                           )
             )
             ),
    
     tabPanel("About",
              titlePanel("About"),
             #    includeHTML("about.html")
             # ))

             h3("About my Project"),
             p("My project is on the effect of COVID-19 state social distancing policies on various outcomes such as positive test rate, deaths, and new administered tests. I chose this project because the pandemic is extremely relevant in the world right now and there is currently disparities in how different states have been handling social distancing policies. I got my data on state social distancing policies from the Kaiser Family Foundation (https://www.kff.org/coronavirus-covid-19/issue-brief/state-data-and-policy-actions-to-address-coronavirus/). This data is updated reguarly to reflect updated policies. I got data on COVID-19 stats from The COVID Tracking Project (https://covidtracking.com/). This data set provides various metrics on COVID-19 for every state and is also updated daily as new information comes in. For my final project, I plan on using the most updated data sets to reflect the current situation."),              h3("Progress"),
              p("So far, I was able to create graphs for the three variables (death, new positice case, new tests) I am interested in and in my app, you are able to select which state you want to see data in. On another tab, I created a data table reflective of the one on the KFF website after wrangling the data a little to remove any extra information. I also filtered out US territories, such as American Samoa and Puerto Rico, from my COVID statistics dataset to reflect the states that were in the social distancing policy data set and to keep things simple. Next time, I will create a separate Rmd file to wrangle the data to make my Shiny code cleaner. Additionally, I added buttons for the three variables that I made graphs for, however, they are currently not functional as that will be my next step to figure out. Some other things that I need to work on is that in the covid stat dataset, some numbers appear to be negative, which does not make sense, so I need to figure out what is going on there. I would also like to figure out a way to show the different data points on the graphs when you toggle over it. Additionally, rather than a table for social distancing policies, I would like to create maps to make things more visually aesthetic and easier to compare between states"),
              h3("About Me"),
              p("My name is Sreya Sudireddy. I am a senior at Harvard College studying Economics with a secondary in Global Health and Health Policy.
 The URL to my Github Repo is here: https://github.com/sreyasudireddy/Milestone_4")
             ))


server <- function(input, output, session) {
    
    
    # output$state_message <- renderText({
    #     paste0("State: ",
    #            input$selected_state,
    #           )
    # })
    
    output$summary = DT::renderDataTable({
       social_distancing
    })
    
 #death graph
  output$covid_stats_by_state <- renderPlot({
    if(input$selected_variable == "deaths") {
    
    # output$covid_death <- renderPlot({
        covid %>%
            filter(state == input$selected_state) %>%
            group_by(state) %>%
            
            ggplot(aes(x = date, y = deaths)) +
            geom_line(color = "blue") +
            labs(title = "COVID-19 Related Deaths",
                 x = "Date",
                 y = "Number of Total Deaths") +
            theme_classic()
    }
    
   #positve case graph 
    #output$covid_positive <- renderPlot({
    else if (input$selected_variable == "cases") {
        covid %>%
            filter(state == input$selected_state) %>%
            group_by(state) %>%
            
            ggplot(aes(x = date, y = cases)) +
            geom_line(color = "purple") +
            labs(title = "COVID-19 Total Cases",
                 x = "Date",
                 y = "Number of Total Cases") +
            theme_classic()
    }
    
  # testing graph 
   # output$covid_testing <- renderPlot({
    else if (input$selected_variable == "totalTestResultsIncrease") {
        covid %>%
            filter(state == input$selected_state) %>%
            group_by(state) %>%
             
            ggplot(aes(x = date, y = totalTestResultsIncrease)) +
            geom_line(color = "red") +
            labs(title = "COVID-19 New Daily Tests",
                 x = "Date",
                 y = "Number of New Tests") +
            theme_classic()
    }
    
    
})
}
shinyApp(ui, server)