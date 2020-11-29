library(shiny) 
library(tidyverse)
library(ggthemes)
library(DT)
library(shinythemes)
library(usmap)
library(plotly)
library(plm)
library(gt)
library(naniar)
library(lubridate)
library(janitor)

#Reading in covid data

covid <- readRDS("coviddata.RDS") %>%
  mutate(positiveIncrease = replace(positiveIncrease, which(positiveIncrease < 0), 0))
  #mutate(totalTestResultsIncrease = replace(totalTestResultsIncrease, which(totalTestResultsIncrease < 0), 0))

# Reading in social distancing data
social_distancing <- readRDS("socialdistancing_11_15.RDS")
social_distancing_oct <- readRDS("socialdistancing.RDS")

# Reading in model data
model_dta <- readRDS("model.RDS")


# creating policy tibbles with renamed columns and statepop fips to create maps
# November
map_policy <- inner_join(social_distancing, statepop, by = c("Location" = "full")) %>%
  rename(reopening_status = `Status of Reopening`) %>%
  rename(restaurant_limits = `Restaurant Limits`) %>%
  rename(gathering_ban = `Large Gatherings Ban`) %>%
  rename(mask_req = `Face Covering Requirement`) %>%
  select(fips, reopening_status, restaurant_limits, gathering_ban, mask_req)

# October
map_policy_oct <- inner_join(social_distancing_oct, statepop, by = c("Location" = "full")) %>%
  rename(reopening_status = `Status of Reopening`) %>%
  rename(restaurant_limits = `Restaurant Limits`) %>%
  rename(gathering_ban = `Large Gatherings Ban`) %>%
  rename(mask_req = `Face Covering Requirement`) %>%
  select(fips, reopening_status, restaurant_limits, gathering_ban, mask_req)

# creating policy tibble for covid maps
map_covid <- inner_join(covid, statepop, by = c("state" = "full")) %>%
  select(date, fips, cases, deaths) %>%
  filter(date == "2020-11-22")


# making objects for selector tools
state.names <- c(covid$state[1:51])
column.names <- c("New Positive Cases" = "positiveIncrease",
                  "Total Positive Cases" = "cases",
                  "Deaths" = "deaths", 
                  "Total Tests" = "totalTestResults"
                  )

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
            p("Many states are experiencing a 'second wave' of COVID-19 cases, 
              which are rising rapdily in recent weeks. Some states, like California and Texas, 
              experienced a summer surge during July/August. During that time, other states, 
              like New York and Massachusetts who were hardest hit by the virus during the early months, 
              saw decreases in new daily positive cases and a flattening of the curve. Nevertheless, all states
              are experiencing some sort of a second wave and the level of new daily positive cases are at/above/or closely approaching
              peak levels from either the spring or summer. Number of deaths are continuing to rise across the country,
              with increases in some states, like Massachusetts and New York, plateauing, while in most other states, deaths
              are continuing to rise everyday. Data was retrieved from the COVID-19 Tracking Project and The New York Times."),
            strong("Use the selectors below to view different COVID-19 statistics and trends for each state"),
            sidebarLayout(
                sidebarPanel(
                    selectInput(
                        inputId = "selected_state",                 
                        label = "Select a State",   
                        choices = c(state.names),
                        selected = "Massachusetts"
                               ),
                    
                    radioButtons(
                        inputId = "selected_variable",
                        label = "Choose a variable",
                        choices = column.names,
                        selected = "positiveIncrease"
                                )
                             ),
                mainPanel(
                   textOutput("state_message"), 
                   plotlyOutput("covid_stats_by_state")
                         )
                        ),
            p("Here are maps displaying the number of cases and deaths on the most recent day.
              These maps help visualize the current severeity of the pandemic in each state
              and helps us easily compare which states have been hit the hardest by the virus."),
            splitLayout(plotOutput("covid_cases_map"), plotOutput("covid_deaths_map"))
        ),
        
    ),
    tabPanel("Social Distancing Policies",
             fluidPage(
                 titlePanel("Social Distancing Data"),
                 sidebarLayout(
                   sidebarPanel(
                     selectInput(
                       inputId = "selected_policy",
                       label = "Select a Policy",
                       choices = c(policy.names),
                       selected = "Status of Reopening"
                     ),
                     radioButtons(
                       inputId = "selected_month",
                       label = "Select a Month",
                       choices = c("October", "November"),
                       selected = "November"
                     )
                   ),
                 mainPanel(
                   plotOutput("policy_maps"),
                   plotOutput("policy_maps_oct")
                           )
                 )
             )
             ),
    
    tabPanel("Model",
             fluidPage(
               titlePanel("Effect of MA Newly Imposed Social Distancing Policies on Various COVID-19 Outcomes"),
               mainPanel(
                 h4("Increase in Daily Positive Cases"),
                 splitLayout(plotOutput("positiveincrease"),
                 DTOutput("did_posincr")),
                 h4("Increase in New Daily Tests"),
                 DTOutput("did_tests"),
                 h4("Increase in Total Deaths"),
                 DTOutput("did_deaths")
               )
             )),
    
     tabPanel("About",
                includeHTML(rmarkdown::render("about.Rmd"))
             ))


server <- function(input, output, session) {
 #COVID stats tab   
    output$state_message <- renderText({
       paste0("State: ", input$selected_state)
              
    })
    
 #death graph
  output$covid_stats_by_state <- renderPlotly({
    if(input$selected_variable == "deaths") {
        covid %>%
            filter(state == input$selected_state) %>%
            group_by(state) %>%
            
            ggplot(aes(x = date, y = deaths)) +
            geom_line(color = "#ef476f") +
            labs(title = "COVID-19 Related Deaths",
                 x = "Date",
                 y = "Number of Total Deaths") +
            scale_y_continuous(labels = scales::comma) +
        scale_x_date(date_breaks = "month", date_labels = "%b") +
            theme_classic()
    }
    
   #positive case graph 
    else if (input$selected_variable == "cases") {
        covid %>%
            filter(state == input$selected_state) %>%
            group_by(state) %>%
            
            ggplot(aes(x = date, y = cases)) +
            geom_line(color = "#edae49") +
            labs(title = "COVID-19 Total Cases",
                 x = "Date",
                 y = "Number of Total Cases") +
        scale_y_continuous(labels = scales::comma) +
        scale_x_date(date_breaks = "month", date_labels = "%b") +
            theme_classic()
    }
    
    # positive Increase graph
    else if(input$selected_variable == "positiveIncrease") {
      covid %>%
        filter(state == input$selected_state) %>%
        group_by(state) %>%
        
        ggplot(aes(x = date, y = positiveIncrease)) +
        geom_line(color = "#219EBC") +
        labs(title = "COVID-19 New Daily Positive Cases",
             x = "Date",
             y = "Number of New Daily Positive Cases") +
        scale_y_continuous(labels = scales::comma) +
        scale_x_date(date_breaks = "month", date_labels = "%b") +
        theme_classic()
    }
    
  # testing graph 
    else if (input$selected_variable == "totalTestResults") {
        covid %>%
            filter(state == input$selected_state) %>%
            group_by(state) %>%
             
            ggplot(aes(x = date, y = totalTestResults)) +
            geom_line(color = "#FB8500") +
            labs(title = "COVID-19 Total Tests",
                 x = "Date",
                 y = "Number of New Tests") +
        scale_y_continuous(labels = scales::comma) +
        scale_x_date(date_breaks = "month", date_labels = "%b") +
            theme_classic()
    }
 

})


# covid total cases map
output$covid_cases_map <- renderPlot({
  plot_usmap(data = map_covid, values = "cases") +
    theme(legend.position = "right", plot.title = element_text(size = 15, face = "plain" )) +
    scale_fill_continuous(name = "Number of Cases") +
    labs(title = "Total COVID-19 Cases as of November 22nd, 2020",
         caption = "Source: The New York Times")
})

# covid total deaths map
output$covid_deaths_map <- renderPlot({
  plot_usmap(data = map_covid, values = "deaths") +
    theme(legend.position = "right", plot.title = element_text(size = 15, face = "plain" )) +
    scale_fill_continuous(name = "Number of Deaths") +
    labs(title = "Total COVID-19 Deaths as of November 22nd, 2020",
         caption = "Source: The New York Times")
})
  
# Social Distancing Tab
  
  #  status of reopening map   
  output$policy_maps <- renderPlot({
    if(input$selected_policy == "reopening_status") {
      #november status of reopening map
       if(input$selected_month == "November"){
      plot_usmap(data = map_policy, values = "reopening_status") +
        theme(legend.position = "bottom", plot.title = element_text(size = 15, face = "plain"))+
        scale_fill_manual(name = "Policy", 
                          labels = c("New Restrictions Imposed", "Paused", "Proceeding with Reopening", "Reopened"), 
                          values = c("#8ECAE6", "#023047", "#219EBC", "#FFB703")) +
        labs(title = "Current Status of Reopening",
             caption = "Source: Kaiser Family Foundation")
       }
      #october status of reopening map
         else if (input$selected_month == "October"){
           plot_usmap(data = map_policy_oct, values = "reopening_status") +
             theme(legend.position = "bottom", plot.title = element_text(size = 15, face = "plain"))+
             scale_fill_manual(name = "Policy", 
                               labels = c("New Restrictions Imposed", "Paused", "Proceeding with Reopening", "Reopened"), 
                               values = c("#8ECAE6", "#023047", "#219EBC", "#FFB703")) +
             labs(title = "Current Status of Reopening",
                  caption = "Source: Kaiser Family Foundation")
         }
      
    }
    
    #  restaurant limits map
    else if (input$selected_policy == "restaurant_limits") { 
      #november restaurant limits map
      if(input$selected_month == "November") {
      plot_usmap(data = map_policy, values = "restaurant_limits") +
        theme(legend.position = "bottom", plot.title = element_text(size = 15, face = "plain" )) +
        scale_fill_manual(name = "Policy", 
                          labels = c("No Data", "New Service Limits", "Reopened to Dine-in Service", "Reopened to Dine-in Service with Capacity Limits"), 
                          values = c("#8ECAE6", "#023047", "#FFB703", "#219EBC")) +
        labs(title = "Current Restaurant Limit Policies",
             caption = "Source: Kaiser Family Foundation")
      }
      #october restaurant limits map
      else if (input$selected_month == "October") {
        plot_usmap(data = map_policy_oct, values = "restaurant_limits") +
          theme(legend.position = "bottom", plot.title = element_text(size = 15, face = "plain" )) +
          scale_fill_manual(name = "Policy", 
                            labels = c("No Data", "New Service Limits", "Reopened to Dine-in Service", "Reopened to Dine-in Service with Capacity Limits"), 
                            values = c("#8ECAE6", "#023047", "#FFB703", "#219EBC")) +
          labs(title = "Current Restaurant Limit Policies",
               caption = "Source: Kaiser Family Foundation")
      }
    }
    
    #  large gathering ban map  
    else if (input$selected_policy == "gathering_ban") {
      # november large gathering ban map
      if(input$selected_month == "November") {
      plot_usmap(data = map_policy, values = "gathering_ban") +
        theme(legend.position = "bottom", plot.title = element_text(size = 15,
                                                                    face = "plain")) +
        scale_fill_manual(name = "Policy",
                          labels = c(">10 People Prohibited", "All Gatherings Prohibited", "Expanded Limit to 25 or Fewer", "Expanded Limit to Greater Than 25", "Lifted", "New Limit on Large Gatherings in Place"),
                          values = c("#FB8500", "#E73462", "#023047", "#FFB703", "#219EBC", "#8ECAE6")) +
        labs(title = "Current Large Gathering Ban Policies",
             caption = "Source: Kaiser Family Foundation")
      }
      # october large gathering ban map
      else if (input$selected_month == "October") {
        plot_usmap(data = map_policy_oct, values = "gathering_ban") +
          theme(legend.position = "bottom", plot.title = element_text(size = 15,
                                                                      face = "plain")) +
          scale_fill_manual(name = "Policy",
                            labels = c("No Data", ">10 People Prohibited", "All Gatherings Prohibited", "Expanded Limit to 25 or Fewer", "Expanded Limit to Greater Than 25", "Lifted", "New Limit on Large Gatherings in Place"),
                            values = c("#FE5F55", "#FB8500", "#E73462", "#023047", "#FFB703", "#219EBC", "#8ECAE6")) +
          labs(title = "Current Large Gathering Ban Policies",
               caption = "Source: Kaiser Family Foundation")
      }
    }
    
    # mask requirement map   
    else if(input$selected_policy == "mask_req") {
      # november mask requirement map
      if(input$selected_month == "November") {
      plot_usmap(data = map_policy, values = "mask_req") +
        theme(legend.position = "bottom", 
              legend.direction = "horizontal", 
              plot.title = element_text(size = 15, face = "plain"), 
              legend.title = element_text(size = 10), 
              legend.text = element_text(size = 6)) +
        scale_fill_manual(name = "Policy", 
                          labels = c("No Data", "Allows Local Officials to \n Require for General Public", "Required for Certain \n Employees", "Required for Certain Employees; \n Allows Local Officials \n to Require for General Public","Required for Certain Employees; \n Required for General Public", "Required for General Public"), 
                          values = c("#FB8500", "#8ECAE6", "#023047", "#FFB703", "#E73462", "#219EBC")) +
        labs(title = "Current Face Covering Requirements",
             caption = "Source: Kaiser Family Foundation")
      }
      # october mask requirement map
      else if(input$selected_month == "October") {
        plot_usmap(data = map_policy_oct, values = "mask_req") +
          theme(legend.position = "bottom", 
                legend.direction = "horizontal", 
                plot.title = element_text(size = 15, face = "plain"), 
                legend.title = element_text(size = 10), 
                legend.text = element_text(size = 6)) +
          scale_fill_manual(name = "Policy",
                            labels = c("No Data", "Allows Local Officials to \n Require for General Public", "Required for Certain \n Employees", "Required for Certain Employees; \n Allows Local Officials \n to Require for General Public", "Required for General Public"),
                            values = c("#FB8500", "#8ECAE6", "#023047", "#FFB703", "#219EBC")) +
          labs(title = "Current Face Covering Requirements",
               caption = "Source: Kaiser Family Foundation")
      }
    }
  })

# difference in differences regression tables and graphs
  # Positive increase graph
  output$positiveincrease <- renderPlot({
    ggplot(model_dta, aes(x = date, y = positive_increase, color = state)) +
      geom_line()
  })
  
  # Positive increase regression
  output$did_posincr <- renderDT({
    posincr_model <- lm(positive_increase ~ time + treated + did + state,
                        data = model_dta) %>%
      tidy(conf.int = TRUE) %>%
      select(term, estimate, p.value, conf.low, conf.high) %>%
      rename(Coefficient = estimate) %>%
      rename(`Lower End` = conf.low) %>%
      rename(`Upper End` = conf.high) %>%
      rename(`P-Value` = p.value) %>%
      filter(term %in% c("(Intercept)", "did"))
  })
  
  # Testing increase regression
  output$did_tests <- renderDT({
    testincr_model <- lm(total_test_results_increase ~ time + treated + did + state,
                         data = model_dta) %>%
      tidy(conf.int = TRUE) %>%
      select(term, estimate, p.value, conf.low, conf.high) %>%
      rename(Coefficient = estimate) %>%
      rename(`Lower End` = conf.low) %>%
      rename(`Upper End` = conf.high) %>%
      rename(`P-Value` = p.value) %>%
      filter(term %in% c("(Intercept)", "did"))
  })
  
  # Deaths regression
  output$did_deaths <- renderDT({
    deaths_model <- lm(deaths ~ time + treated + did + state,
                       data = model_dta) %>%
      tidy(conf.int = TRUE) %>%
      select(term, estimate, p.value, conf.low, conf.high) %>%
      rename(Coefficient = estimate) %>%
      rename(`Lower End` = conf.low) %>%
      rename(`Upper End` = conf.high) %>%
      rename(`P-Value` = p.value) %>%
      filter(term %in% c("(Intercept)", "did"))
  })

  
}
shinyApp(ui, server)