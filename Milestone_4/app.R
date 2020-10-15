library(shiny) 
library(tidyverse)
library(ggthemes)


covid <- read_csv("data/covid_stats.csv") %>%
    filter(!state %in% c("AS", "PR", "GU", "VI", "MP")) 

state.names <- c(covid$state[1:51])
column.names <- c("Deaths" = "death", "New Tests" = "test", "New Positive Cases" = "positive")

social_distancing <- read_csv("data/social_distancing.csv", skip = 2)

######################################################################################
######################################################################################

ui <- navbarPage(
    "Social Distancing Policies and COVID-19 State Data",
    tabPanel(
        "Main",
        fluidPage(
            titlePanel("COVID-19 Data by State"),
            sidebarLayout(
                sidebarPanel(
                    selectInput(
                        inputId = "selected_state",                 # a name for the value you choose here
                        label = "Select a State to view COVID-19 data",   # the name to display on the slider
                        choices = state.names                       # your list of choices to choose from
                    ),
                    
                    radioButtons(
                        inputId = "selected_variable",             # a name for the value you choose here
                        label = "Choose a variable!",              # the label to display above the buttons
                        choices = column.names     # the button values to choose from
                    )),
                mainPanel(
                    textOutput("state_message"),              # load a text object called "state_message"
                    # textOutput("text_message"),
                    plotOutput("covid_death"),
                    plotOutput("covid_testing"),
                    plotOutput("covid_positive")
                )
            )
        ),
        
    ),
    tabPanel("About",
             h3("About Me"),
             p("My name is Sreya Sudireddy. I am a senior at Harvard College studying Economics with a secondary in Global Health and Health Policy."),
             h3("About my Project"),
             p("I did this project because..."))
)

server <- function(input, output, session) {
    # - Then, you use these named objects to update the data on your site via the input object.
    #
    #   -- render() functions are what show content that will change live on your site.
    #
    #   -- so here, renderText() is updating live text based on your choice.
    output$state_message <- renderText({
        paste0("State: ", # this is just a string, so it will never change
               input$selected_state)       # this is based on your input, selected_state defined above.
    })
    
    # output$text_message <- renderText({
    #   paste0("This is the label you typed: ", # this is just a string, so it will never change
    #          input$entered_text, "!")       # this is based on your input, selected_state defined above.
    # })
    
    output$covid_death <- renderPlot({
        covid %>%
            filter(state == input$selected_state) %>%
            group_by(state) %>%
            
            ggplot(aes(x = date, y = death)) +
            geom_line(color = "blue") +
            labs(title = "COVID-19 Related Deaths",
                 x = "Date",
                 y = "Number of Total Deaths") +
            theme_classic()
    })
    
    output$covid_testing <- renderPlot({
        covid %>%
            filter(state == input$selected_state) %>%
            group_by(state) %>%
            
            ggplot(aes(x = date, y = totalTestResultsIncrease)) +
            geom_line(color = "red") +
            labs(title = "COVID-19 New Daily Tests",
                 x = "Date",
                 y = "Number of New Tests") +
            theme_classic()
    })
    
    output$covid_positive <- renderPlot({
        covid %>%
            filter(state == input$selected_state) %>%
            group_by(state) %>%
            
            ggplot(aes(x = date, y = positiveIncrease)) +
            geom_line(color = "purple") +
            labs(title = "COVID-19 New Positive Cases",
                 x = "Date",
                 y = "Number of New Positive Cases") +
            theme_classic()
    })
    
    
}
shinyApp(ui, server)