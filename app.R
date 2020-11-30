library(shiny) 
library(tidyverse)
library(ggthemes)
library(DT)
library(shinythemes)
library(usmap)
library(plotly)
library(plm)
library(naniar)
library(lubridate)
library(janitor)
library(leaflet)
library(broom)

#Reading in covid data

covid <- readRDS("coviddata.RDS") %>%
  mutate(positiveIncrease = replace(positiveIncrease, which(positiveIncrease < 0), 0)) %>%
  mutate(totalTestResultsIncrease = replace(totalTestResultsIncrease, which(totalTestResultsIncrease < 0), 0))

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
            br(),
            br(),
            p("Here are maps displaying the number of cases and deaths on the most recent day.
              These maps help visualize the current severeity of the pandemic in each state
              and helps us easily compare which states have been hit the hardest by the virus."),
            splitLayout(plotOutput("covid_cases_map"), plotOutput("covid_deaths_map"))
        ),
        
    ),
    tabPanel("Social Distancing Policies",
             fluidPage(
                 titlePanel("Social Distancing Data"),
                 p("In October, many states were progressing towards reopening and loosening certain 
                   social distancing policies. However, as November came and the second wave hit much
                   of the country, many states have started to roll back their reopenings and impose new
                   restrictions. Some of the biggest changes involved new gathering limits and restaurant limits, areas 
                   where the spread of COVID-19 is huge. Data on state-level social distancing policies was collected from The 
                   Kaiser Family Foundation."),
                 br(),
                 strong("Use the selectors below to select a policy and month to view and compare social distancing policies
                        in each state"),
                 br(),
                 br(),
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
                   plotOutput("policy_maps", width = "100%", height = "500px"),
                   plotOutput("policy_maps_oct", width = "100%", height = "500px")
                           )
                 )
             )
             ),
    
    tabPanel("Model",
             fluidPage(
               titlePanel("Effect of Massachusetts' Newly Imposed Social Distancing Policies on Various COVID-19 Outcomes"),
               tabsetPanel(type = "tabs",
                    tabPanel("Summary of Model",
                h4("Introduction"),
                p("Effective November 6th, 2020, Governor Charlie Baker of Massachusetts announced new statewide social distancing policies.
                 These policies were enacted in efforts to contain the rising COVID-19 cases. In comparison, Colorado has had consistently through
                 the months of October and November rather lenient social distnacing policies and has not imposed new restrictions despite also 
                 experiencing a rise in COVID-19 cases. New York has followed a very similar pattern to Massachusetts and as of the time Governor Baker's
                 new policies were enacted, New York has yet to impose any new restrictions in November."),
               h4("Description of the Model"),
               p("My model is a difference-in-differences analysis to analyze the effects of the new November Massachusetts social distancing
                 policies on COVID-19 new daily positive cases, new daily tests, and total deaths. In order for the model to be executed, the 
                 data must follow the parallel trends assumption, where the trends before the treatment date (in this case, the treatment date
                 is Nov 6th) follow parallel trends. The first date of the model is Oct 1st, which is assumed that the October social distancing policies
                 apply during the entire month up until the new November policy data was collected, which was on Nov 11th. 
                 This model uses New York and Colorado as controls and the daily positve case rate, daily tests,
                 and total deaths for each of the three states are shown on the graphs in the next tab. We can see that the parallel trends assumption is 
                 followed fairly well for new daily positive cases and total deaths. The pre-trends for new tests are not quite parallel and there is 
                 not much variation between the states after the treatment date, and therefore the results of this regression may not be very accurate. Additionally,
                 although the pre-trends for the total death rate are parallel, there is not much variation after the treatment date, therefore it is not assumed
                 that the new social distancing policies have an affect on the total death rate. Nevertheless, for the daily positive case rate, which is the rate
                 these policies are mostly aiming to contain, have relatively close parallel pre-trends and the positive case rate in Massachusetts rises but by not as much
                 as it does in Colorado and New York where new social distancing policies were not imposed by the treatment date. Therefore, it is assumed that
                 the new social distancing policies have an effect on containing the positive case rate in Massachusetts and that we can apply this model to analyze that effect."),
               h4("Model Specification"),
               p("The model will be setup according to this specification: "),
               uiOutput('equation'),
               h4("Limitations"),
               p("There are some limitations to this model that must be considered: One is that this diff-in-diff model is a simple model and contains
                 no extra covariates. Other factors such as income, population density, race, and gender can affect these COVID-19 outcomes and should be considered
                 for a more robust model.")
               ),
               tabPanel("Results", column(width = 12, mainPanel(
                 h4("Daily Positive Cases"),
                 splitLayout(plotOutput("positiveincrease"), cellWidths = c("75%", "75%"),
                             DTOutput("did_posincr")),
                 br(),
                 p("The difference-in-differences coefficient is -1487.76. The p-value is close to 0. This means that the effect of the new social distancing policies
                   in Massachusetts on the number of new positive cases is around -1,487 cases more than in the control states. Because the p-value is less than 0.05, the 
                   result is statistically significant at the 95% confidence level. This is the most important factor to consider when thinking about the effectiveness of social
                   distancing policies. Keeping in mind that the model is simple, it shows that the newly imposed restrictions in MA had a negative effect on the new daily positive 
                   case rate compared to the control states, which is a good sign that the policies are effective at slowing the spread of the virus."),
                 br(),
                 h4("New Daily Tests"),
                 splitLayout(plotOutput("newtests"), cellWidths = c("75%", "75%"),
                             DTOutput("did_tests")),
                 br(),
                 p("The difference-in-differences coefficient is -14933.8. The p-value is 0.03. This means that the effect of the new social distancing policies
                   in Massachusetts on the number of new tests is around -14,933 tests more than in the control states. Because the p-value is less than 0.05, the 
                   result is statistically significant at the 95% confidence level. Keep in mind that some people get tested regardless of the
                   social distancing policies in place in their state or their health status. Some reasons my include travel, precautionary measures, general curiosity, etc.
                   Therefore, it is difficult to isolate the effect of just the policies on the number of tests conducted. Despite the result showing less tests being done in MA 
                   after the treatment compared to the other states, this could also just be that MA has a smaller population and therefore fewer tests would be conducted."),
                 br(),
                 h4("Total Deaths"),
                 splitLayout(plotOutput("deaths"), cellWidths = c("75%", "75%"),
                             DTOutput("did_deaths")),
                 br(),
                 p("The difference-in-differences coefficient is 110.94. The p-value is 0.04 This means that the effect of the new social distancing policies
                   in Massachusetts on the number of deaths is around 111 cases more than in the control states. Because the p-value is less than 0.05, the 
                   result is statistically significant at the 95% confidence level. Keep in mind that despite these results, there might not be a significant 
                   relationship between social distancing policies and total deaths. Many factors can contribute to the number of deaths, such as the positive case rate, 
                   race, gender, pre-existing conditions, level of care, etc. The result may be positive in this model, but after considering other factors, the result of the diff-in-diff 
                   may change.")
               )))
               )
             )),
    
     tabPanel("About",
              fluidPage(
                h1("About my Project"),
                p("My project is on the effect of COVID-19 social distancing policies on various outcomes such as positive test rate, deaths, and new administered tests. 
                I chose this project because the pandemic is extremely relevant in the world right now and there are disparities 
                in how different states have been handling social distancing policies. Some states have stricter policies and impose restrictions if the state of
                the pandemic in their state getting worse. Other states continue to proceed with reopening despite the current second wave."), 
                p("My model focuses on the state of Massachusetts, a state that has imposed strict social distancing policies and recently imposed new restrictions on Nov 6th, 2020. I conduct 
                a difference-in-difference analysis, using the states of Colorado and New York as controls, to assess the effect of these new restrictions on the daily positve test rate, daily new tests, and total number of deaths."),
                p("I got my data on state social distancing policies from the",  a("Kaiser Family Foundation", href = "https://www.kff.org/coronavirus-covid-19/issue-brief/state-data-and-policy-actions-to-address-coronavirus/"), "This data is updated regularly to reflect updated policies. 
                Data from October was collected on October 20, 2020 and data from November was collected on November 11th, 2020. I got data on COVID-19 stats from", a("The COVID Tracking Project", href = "https://covidtracking.com/"), 
                  "and", a("The New York Times.", href = "https://github.com/nytimes/covid-19-data"), "These data sets provide various metrics on COVID-19 for every state and is also updated daily as new information comes in. 
                  For my final project, I plan on using the most updated data sets to reflect the current situation."),
                h1("About Me"),
                p("My name is Sreya Sudireddy. I am a senior at Harvard College studying Economics with a secondary in 
                  Global Health and Health Policy. You can reach me at", a("sreyasudireddy@college.harvard.edu.", 
                                                                           href = "mailto: sreyasudireddy@college.harvard.edu"), 
                  "The URL to my Github Repo with the code to this project is", a("here.", 
                                                                                  href = "https://github.com/sreyasudireddy/social-distancing-covid-stats"))
              )
              
             ))


server <- function(input, output, session) {
#COVID stats tab 
  # outputting message indicating selected state
    output$state_message <- renderText({
       paste0("State: ", input$selected_state)
              
    })
    
 # death graph
  output$covid_stats_by_state <- renderPlotly({
    if(input$selected_variable == "deaths") {
        covid %>%
            filter(state == input$selected_state) %>%
            group_by(state) %>%
            rename(Deaths = "deaths") %>%
            rename(Date = "date") %>%
            
            ggplot(aes(x = Date, y = Deaths)) +
            geom_line(color = "#ef476f") +
            labs(title = "COVID-19 Related Deaths",
                 x = "Date",
                 y = "Number of Total Deaths") +
            scale_y_continuous(labels = scales::comma) +
        scale_x_date(date_breaks = "month", date_labels = "%b") +
            theme_classic()
    }
    
   # total positive case graph 
    else if (input$selected_variable == "cases") {
        covid %>%
            filter(state == input$selected_state) %>%
            group_by(state) %>%
            rename(Cases = "cases") %>%
            rename(Date = "date") %>%
            
            ggplot(aes(x = Date, y = Cases)) +
            geom_line(color = "#edae49") +
            labs(title = "COVID-19 Total Cases",
                 x = "Date",
                 y = "Number of Total Cases") +
        scale_y_continuous(labels = scales::comma) +
        scale_x_date(date_breaks = "month", date_labels = "%b") +
            theme_classic()
    }
    
    # positive increase graph
    else if(input$selected_variable == "positiveIncrease") {
      covid %>%
        filter(state == input$selected_state) %>%
        group_by(state) %>%
        rename(`New Cases` = "positiveIncrease") %>%
        rename(Date = "date") %>%
        
        ggplot(aes(x = Date, y = `New Cases`)) +
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
            rename(`Total Tests` = "totalTestResults") %>%
            rename(Date = "date") %>%
             
            ggplot(aes(x = Date, y = `Total Tests`)) +
            geom_line(color = "#FB8500") +
            labs(title = "COVID-19 Total Tests",
                 x = "Date",
                 y = "Number of Total Tests") +
        scale_y_continuous(labels = scales::comma) +
        scale_x_date(date_breaks = "month", date_labels = "%b") +
            theme_classic()
    }
 

})


# current covid total cases map
output$covid_cases_map <- renderPlot({
  plot_usmap(data = map_covid, values = "cases") +
    theme(legend.position = "right", plot.title = element_text(size = 15, face = "plain" )) +
    scale_fill_continuous(name = "Number of Cases") +
    labs(title = "Total COVID-19 Cases as of November 22nd, 2020",
         caption = "Source: The New York Times")
})

# current covid total deaths map
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
      # november status of reopening map
       if(input$selected_month == "November"){
      plot_usmap(data = map_policy, values = "reopening_status") +
        theme(legend.position = "bottom", plot.title = element_text(size = 20, face = "plain"), plot.title.position = "panel",
              legend.text = element_text(size = 13), legend.title = element_text(size = 15)) +
        scale_fill_manual(name = "Policy", 
                          labels = c("New Restrictions Imposed", "Paused", "Proceeding with Reopening", "Reopened"), 
                          values = c("#8ECAE6", "#023047", "#219EBC", "#FFB703")) +
        labs(title = "November Status of Reopening",
             caption = "Source: Kaiser Family Foundation")
       }
      # october status of reopening map
         else if (input$selected_month == "October"){
           plot_usmap(data = map_policy_oct, values = "reopening_status") +
             theme(legend.position = "bottom", plot.title = element_text(size = 20, face = "plain"), plot.title.position = "panel",
                   legend.text = element_text(size = 13), legend.title = element_text(size = 15)) + 
             scale_fill_manual(name = "Policy", 
                               labels = c("New Restrictions Imposed", "Paused", "Proceeding with Reopening", "Reopened"), 
                               values = c("#8ECAE6", "#023047", "#219EBC", "#FFB703")) +
             labs(title = "October Status of Reopening",
                  caption = "Source: Kaiser Family Foundation")
         }
      
    }
    
    #  restaurant limits map
    else if (input$selected_policy == "restaurant_limits") { 
      # november restaurant limits map
      if(input$selected_month == "November") {
      plot_usmap(data = map_policy, values = "restaurant_limits") +
          theme(legend.position = "bottom", plot.title = element_text(size = 20, face = "plain"), plot.title.position = "panel",
                legend.text = element_text(size = 13), legend.title = element_text(size = 15)) +
        scale_fill_manual(name = "Policy", 
                          labels = c("No Data", "New Service Limits", "Reopened to Dine-in Service", "Reopened to Dine-in Service with Capacity Limits"), 
                          values = c("#8ECAE6", "#023047", "#FFB703", "#219EBC")) +
        labs(title = "November Restaurant Limit Policies",
             caption = "Source: Kaiser Family Foundation")
      }
      # october restaurant limits map
      else if (input$selected_month == "October") {
        plot_usmap(data = map_policy_oct, values = "restaurant_limits") +
          theme(legend.position = "bottom", plot.title = element_text(size = 20, face = "plain"), plot.title.position = "panel",
                legend.text = element_text(size = 13), legend.title = element_text(size = 15)) +
          scale_fill_manual(name = "Policy", 
                            labels = c("No Data", "New Service Limits", "Reopened to Dine-in Service", "Reopened to Dine-in Service with Capacity Limits"), 
                            values = c("#8ECAE6", "#023047", "#FFB703", "#219EBC")) +
          labs(title = "October Restaurant Limit Policies",
               caption = "Source: Kaiser Family Foundation")
      }
    }
    
    #  large gathering ban map  
    else if (input$selected_policy == "gathering_ban") {
      # november large gathering ban map
      if(input$selected_month == "November") {
      plot_usmap(data = map_policy, values = "gathering_ban") +
          theme(legend.position = "bottom", plot.title = element_text(size = 20, face = "plain"), plot.title.position = "panel",
                legend.text = element_text(size = 13), legend.title = element_text(size = 15)) +
        scale_fill_manual(name = "Policy",
                          labels = c(">10 People Prohibited", "All Gatherings Prohibited", "Expanded Limit to 25 or Fewer", "Expanded Limit to Greater Than 25", "Lifted", "New Limit on Large Gatherings in Place"),
                          values = c("#FB8500", "#E73462", "#023047", "#FFB703", "#219EBC", "#8ECAE6")) +
        labs(title = "November Large Gathering Ban Policies",
             caption = "Source: Kaiser Family Foundation")
      }
      # october large gathering ban map
      else if (input$selected_month == "October") {
        plot_usmap(data = map_policy_oct, values = "gathering_ban") +
          theme(legend.position = "bottom", plot.title = element_text(size = 20, face = "plain"), plot.title.position = "panel",
                legend.text = element_text(size = 13), legend.title = element_text(size = 15)) +
          scale_fill_manual(name = "Policy",
                            labels = c("No Data", ">10 People Prohibited", "All Gatherings Prohibited", "Expanded Limit to 25 or Fewer", "Expanded Limit to Greater Than 25", "Lifted", "New Limit on Large Gatherings in Place"),
                            values = c("#FE5F55", "#FB8500", "#E73462", "#023047", "#FFB703", "#219EBC", "#8ECAE6")) +
          labs(title = "October Large Gathering Ban Policies",
               caption = "Source: Kaiser Family Foundation")
      }
    }
    
    # mask requirement map   
    else if(input$selected_policy == "mask_req") {
      # november mask requirement map
      if(input$selected_month == "November") {
      plot_usmap(data = map_policy, values = "mask_req") +
          theme(legend.position = "bottom", plot.title = element_text(size = 20, face = "plain"), plot.title.position = "panel",
                legend.text = element_text(size = 13), legend.title = element_text(size = 15)) +
        scale_fill_manual(name = "Policy", 
                          labels = c("No Data", "Allows Local Officials to \n Require for General Public", "Required for Certain \n Employees", "Required for Certain Employees; \n Allows Local Officials \n to Require for General Public","Required for Certain Employees; \n Required for General Public", "Required for General Public"), 
                          values = c("#FB8500", "#8ECAE6", "#023047", "#FFB703", "#E73462", "#219EBC")) +
        labs(title = "November Face Covering Requirements",
             caption = "Source: Kaiser Family Foundation")
      }
      # october mask requirement map
      else if(input$selected_month == "October") {
        plot_usmap(data = map_policy_oct, values = "mask_req") +
          theme(legend.position = "bottom", plot.title = element_text(size = 20, face = "plain"), plot.title.position = "panel",
                legend.text = element_text(size = 13), legend.title = element_text(size = 15)) +
          scale_fill_manual(name = "Policy",
                            labels = c("No Data", "Allows Local Officials to \n Require for General Public", "Required for Certain \n Employees", "Required for Certain Employees; \n Allows Local Officials \n to Require for General Public", "Required for General Public"),
                            values = c("#FB8500", "#8ECAE6", "#023047", "#FFB703", "#219EBC")) +
          labs(title = "October Face Covering Requirements",
               caption = "Source: Kaiser Family Foundation")
      }
    }
  })

# Model tab: diff-in-diff graphs and tables
  # outputting diff-in-diff specification
  output$equation <- renderUI({
    withMathJax( "$$Y_i = \\beta_0 + \\beta_1 time_i + \\beta_2 treated_i + \\beta_3 (time*treated)_i + \\epsilon_i$$
                Where \\(Y_i\\) is the outcome variable (either new positive cases, new daily tests, or total deaths), \\(\\beta_0\\) is the 
                 intercept term, time indicates whether the date is after the treatment date (11/06), treated indicates if the state is Massachusetts,
                 and time * treated is the difference-in-differences interaction term. We are most interested in the coefficient on the interaction term, therefore
                 only the \\(\\beta_3\\) and \\(\\beta_0\\) are displayed in the regression tables in the next tab." )
  })
  
  # Positive increase graph
  output$positiveincrease <- renderPlot({
    ggplot(model_dta, aes(x = date, y = positive_increase, color = state)) +
      geom_line() +
      scale_color_manual(name = "State", 
                         labels = c("Colorado", "Massachusetts", "New York"),
                         values = c("#ef476f", "#edae49", "#219EBC")) +
      geom_vline(xintercept = as.numeric(as.Date("2020-11-05")), linetype = "dashed") +
      theme_classic() +
      labs(x = "Date", y = "Number of New Positive Cases") 
      
  })
  
  # New Tests graph
  output$newtests <- renderPlot({
    ggplot(model_dta, aes(x = date, y = total_test_results_increase, color = state)) +
      geom_line() +
      scale_color_manual(name = "State", 
                         labels = c("Colorado", "Massachusetts", "New York"),
                         values = c("#ef476f", "#edae49", "#219EBC")) +
      geom_vline(xintercept = as.numeric(as.Date("2020-11-05")), linetype = "dashed") +
      theme_classic() +
      labs(x = "Date", y = "Number of New Daily Tests") 
    
  })
  
  # Deaths graph
  output$deaths <- renderPlot({
    ggplot(model_dta, aes(x = date, y = deaths, color = state)) +
      geom_line() +
      scale_color_manual(name = "State", 
                         labels = c("Colorado", "Massachusetts", "New York"),
                         values = c("#ef476f", "#edae49", "#219EBC")) +
      geom_vline(xintercept = as.numeric(as.Date("2020-11-05")), linetype = "dashed") +
      theme_classic() +
      labs(x = "Date", y = "Number of Deaths")
    
  })
  
  # Positive increase regression table
  output$did_posincr <- renderDT({
    lm(positive_increase ~ time + treated + did + state,
                        data = model_dta) %>%
      tidy(conf.int = TRUE) %>%
      select(term, estimate, p.value, conf.low, conf.high) %>%
      rename(Coefficient = estimate) %>%
      rename(`Lower End` = conf.low) %>%
      rename(`Upper End` = conf.high) %>%
      rename(`P-Value` = p.value) %>%
      filter(term %in% c("(Intercept)", "did")) %>%
      mutate(Coefficient = round(Coefficient, digits = 2)) %>%
      mutate(`P-Value` = round(`P-Value`, digits = 2)) %>%
      mutate(`Lower End` = round(`Lower End`, digits = 2)) %>%
      mutate(`Upper End` = round(`Upper End`, digits = 2))
  })
  
  # Testing increase regression table
  output$did_tests <- renderDT({
   lm(total_test_results_increase ~ time + treated + did + state,
                         data = model_dta) %>%
      tidy(conf.int = TRUE) %>%
      select(term, estimate, p.value, conf.low, conf.high) %>%
      rename(Coefficient = estimate) %>%
      rename(`Lower End` = conf.low) %>%
      rename(`Upper End` = conf.high) %>%
      rename(`P-Value` = p.value) %>%
      filter(term %in% c("(Intercept)", "did")) %>%
      mutate(Coefficient = round(Coefficient, digits = 2)) %>%
      mutate(`P-Value` = round(`P-Value`, digits = 2)) %>%
      mutate(`Lower End` = round(`Lower End`, digits = 2)) %>%
      mutate(`Upper End` = round(`Upper End`, digits = 2))
  })
  
  # Deaths regression table
  output$did_deaths <- renderDT({
    lm(deaths ~ time + treated + did + state,
                       data = model_dta) %>%
      tidy(conf.int = TRUE) %>%
      select(term, estimate, p.value, conf.low, conf.high) %>%
      rename(Coefficient = estimate) %>%
      rename(`Lower End` = conf.low) %>%
      rename(`Upper End` = conf.high) %>%
      rename(`P-Value` = p.value) %>%
      filter(term %in% c("(Intercept)", "did")) %>%
      mutate(Coefficient = round(Coefficient, digits = 2)) %>%
      mutate(`P-Value` = round(`P-Value`, digits = 2)) %>%
      mutate(`Lower End` = round(`Lower End`, digits = 2)) %>%
      mutate(`Upper End` = round(`Upper End`, digits = 2))
  })

  
}
shinyApp(ui, server)