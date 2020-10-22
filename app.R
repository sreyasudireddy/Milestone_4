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
                        label = "Choose a variable",              # the label to display above the buttons
                        choices = column.names, # the button values to choose from
                        selected = "deaths"
                                )
                             ),
                mainPanel(
                    textOutput("state_message"),              # load a text object called "state_message"
                    # textOutput("text_message"),
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
             #  titlePanel("About"),
             #    includeHTML("about.html")
             # ))

             h3("About my Project"),
             p("My project is on the effect of COVID-19 state social distancing policies on various outcomes such as positive test rate, deaths, and new administered tests. I chose this project because the pandemic is extremely relevant in the world right now and there is currently disparities in how different states have been handling social distancing policies. I got my data on state social distancing policies from the Kaiser Family Foundation (https://www.kff.org/coronavirus-covid-19/issue-brief/state-data-and-policy-actions-to-address-coronavirus/). This data is updated regularly to reflect updated policies. I got data on COVID-19 stats from The COVID Tracking Project (https://covidtracking.com/) and The New York Times. This data set provides various metrics on COVID-19 for every state and is also updated daily as new information comes in. For my final project, I plan on using the most updated data sets to reflect the current situation."),
             h3("Progress"),
              p("This week, I created a separate Rmd file for the COVID stats data. I downloaded an additional data set from The New York Times to avoid the negative number issue for deaths and number of positive cases. I still have that issue for the number of tests in each state since I could not find that data elsewhere. Over the next few weeks, I will try to examine the issue in the data. My guess is that the value is zero and it defaulted to a negative number. With Ishan's help I was able to make the radio buttons work and now the graph changes with each state and variable (yay!). I also changed the colors to some of my favorite colors. I also changed the theme to make things look more aesthetic. I would also still like to figure out a way to show the different data points on the graphs when you toggle over it. Additionally, rather than a table for social distancing policies, I would like to create maps to make things more visually aesthetic and easier to compare between states. Eventually, I will also need to figure out a model."),
              h3("About Me"),
              p("My name is Sreya Sudireddy. I am a senior at Harvard College studying Economics with a secondary in Global Health and Health Policy.
 The URL to my Github Repo is here: https://github.com/sreyasudireddy/Milestone_4")
             ))


server <- function(input, output, session) {
    
    
    output$state_message <- renderText({
        paste0("State: "
               #input$selected_state,
              )
    })
    
    output$summary = DT::renderDataTable({
       social_distancing
    })
    
 #death graph
  output$covid_stats_by_state <- renderPlot({
    if(input$selected_variable == "deaths") {
        covid %>%
            filter(state == input$selected_state) %>%
            group_by(state) %>%
            
            ggplot(aes(x = date, y = deaths)) +
            geom_line(color = "#ef476f", size = 1) +
            labs(title = "COVID-19 Related Deaths",
                 x = "Date",
                 y = "Number of Total Deaths") +
            theme_classic()
    }
    
   #positve case graph 
    else if (input$selected_variable == "cases") {
        covid %>%
            filter(state == input$selected_state) %>%
            group_by(state) %>%
            
            ggplot(aes(x = date, y = cases)) +
            geom_line(color = "#edae49", size = 1) +
            labs(title = "COVID-19 Total Cases",
                 x = "Date",
                 y = "Number of Total Cases") +
            theme_classic()
    }
    
  # testing graph 
    else if (input$selected_variable == "totalTestResultsIncrease") {
        covid %>%
            filter(state == input$selected_state) %>%
            group_by(state) %>%
             
            ggplot(aes(x = date, y = totalTestResultsIncrease)) +
            geom_line(color = "#00798c", size = 1) +
            labs(title = "COVID-19 New Daily Tests",
                 x = "Date",
                 y = "Number of New Tests") +
            theme_classic()
    }
    
    
})
}
shinyApp(ui, server)