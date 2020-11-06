library(shiny) 
library(tidyverse)
library(ggthemes)
library(DT)
library(shinythemes)
library(usmap)

#Reading in covid data

covid <- readRDS("coviddata.RDS")

# Reading in social distancing data
social_distancing <- readRDS("socialdistancing.RDS")

#creating policy tibble with renamed columns and statepop fips to create maps
map_policy <- inner_join(social_distancing, statepop, by = c("Location" = "full")) %>%
  rename(reopening_status = `Status of Reopening`) %>%
  rename(restaurant_limits = `Restaurant Limits`) %>%
  rename(gathering_ban = `Large Gatherings Ban`) %>%
  rename(mask_req = `Face Covering Requirement`) %>%
  select(fips, reopening_status, restaurant_limits, gathering_ban, mask_req)


# making objects for selector tools
state.names <- c(covid$state[1:51])
column.names <- c("Deaths" = "deaths", "Positive Cases" = "cases", "New Tests" = "totalTestResultsIncrease")
policy.names <- c("Status of Reopening" = "reopening_status", 
                  "Restaurant Bans" = "restaurant_limits", 
                  "Large Gathering Bans" = "gathering_ban", 
                  "Face Covering Requirement" = "mask_req")


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
                 sidebarLayout(
                   sidebarPanel(
                     selectInput(
                       inputId = "selected_policy",
                       label = "Select a Policy",
                       choices = c(policy.names),
                       selected = "Status of Reopening"
                     )
                   ),
                 mainPanel(
                   plotOutput("policy_maps")
                           )
                 )
             )
             ),
    
     tabPanel("About",
                includeHTML(rmarkdown::render("about.Rmd"))
             ))


server <- function(input, output, session) {
    
    
    output$state_message <- renderText({
        paste0("State: ",
               input$selected_state
              )
    })
    
 
  # status of reopening map   
    output$policy_maps <- renderPlot({
      if(input$selected_policy == "reopening_status") {
        plot_usmap(data = map_reopening, values = "reopening_status") +
          theme(legend.position = "bottom", plot.title = element_text(size = 15, face = "plain")) +
          scale_fill_manual(name = "Policy", 
                            labels = c("New Restrictions Imposed", "Paused", "Proceeding with Reopening", "Reopened"), 
                            values = c("#8ECAE6", "#023047", "#219EBC", "#FFB703")) +
          labs(title = "Current Status of Reopening",
               caption = "Source: Kaiser Family Foundation")
      }
  
  # restaurant limits map
     else if (input$selected_policy == "restaurant_limits") { 
       plot_usmap(data = map_restaurant, values = "restaurant_limits") +
         theme(legend.position = "bottom", plot.title = element_text(size = 15, face = "plain" )) +
         scale_fill_manual(name = "Policy", labels = c("No Data", "New Service Limits", "Reopened to Dine-in Service", "Reopened to Dine-in Service with Capacity Limits"), values = c("#8ECAE6", "#023047", "#FFB703", "#219EBC")) +
         labs(title = "Current Restaurant Limit Policies",
              caption = "Source: Kaiser Family Foundation")
     }
      
  # large gathering ban map  
    else if (input$selected_policy == "gathering_ban") {
      plot_usmap(data = map_gathering, values = "gathering_ban") +
        theme(legend.position = "bottom", plot.title = element_text(size = 15,
                                                                    face = "plain")) +
        scale_fill_manual(name = "Policy", 
                          labels = c("No Data", ">10 People Prohibited", "All Gatherings Prohibited", "Expanded Limit to 25 or Fewer", "Expanded Limit to Greater Than 25", "Lifted", "New Limit on Large Gatherings"), values = c("#FB8500", "#FE5F55", "#023047", "#FFB703", "#219EBC", "#E73462", "#8ECAE6")) +
        labs(title = "Current Large Gathering Ban Policies",
             caption = "Source: Kaiser Family Foundation")
    }
      
 # mask requirement map   
    else if (input$selected_policy == "mask_req") {
      plot_usmap(data = map_masks, values = "mask_req") +
        theme(legend.position = "bottom", 
              legend.direction = "horizontal", 
              plot.title = element_text(size = 15, face = "plain"), 
              legend.title = element_text(size = 10), 
              legend.text = element_text(size = 6)) +
        scale_fill_manual(name = "Policy", labels = c("No Data", "New Service Limits", "Reopened to Dine-in Service", "Reopened to Dine-in Service with Capacity Limits"), values = c("#FB8500", "#8ECAE6", "#023047", "#FFB703", "#219EBC")) +
        labs(title = "Current Face Covering Requirements",
             caption = "Source: Kaiser Family Foundation")
    }
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