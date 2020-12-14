#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
pop_prop <- read_csv("population_propotion")

covid_race <- read_csv("covidrace.csv")

covid_race_cases_deaths <- covid_race %>%
    select(Date, State, caseper_white:caseperunkn) %>%
    filter(State != c("AS", "GU")) %>%
    pivot_longer(cols = caseper_white:caseperunkn, 
                 names_to = "race_c", values_to = "cases_r")

covid_race_cases_deaths <- readRDS("covid_race_cases_deaths.RDS")

# To simplify the manner for now, I pivoted in the ShinyApp. In the future,
# this will hopefully be done in the Markdown section.

# Define UI for application that draws a histogram
ui <- navbarPage(
    "COVID Deaths as a Factor of Race and Ethnicity",
    tabPanel("Data",
             fluidPage(
                 titlePanel("COVID Cases Proportionalized"),
                 sidebarLayout(
                     sidebarPanel(
                         selectInput(
                             "state",
                             "State",
                             c("AK", "AL", "AR", "AZ", "CA", "CO", "CT",
                               "DC", "DE", "FL", "GA", "HI", "IA", "ID",
                               "IL", "IN", "KS", "KY", "LA", "MA", "MD", "ME", 
                               "MI", "MN", "MO", "MP", "MS", "MT", "NC", "ND",
                               "NE", "NH", "NJ", "NM", "NV", "NY", "OH", "OK",
                               "OR", "PA", "PR", "RI", "SC", "SD", "TN", "TX",
                               "UT", "VA", "VI", "VT", "WA", "WI", "WV", "WY")),
                         dateInput("date", label = h3("Date Input"), 
                                   value = "2020-10-11")),
                     
# Is this annoying to type out? Yes. Is it necessary to be able to select all
# 56 territories? Yes.
                     
                 mainPanel(
                     plotOutput("casesprop")
                 )))),
    tabPanel("Model",
             titlePanel("Predictive Model"),
             p("This is the place where the predictive model will go")),
    tabPanel("Discussion",
             titlePanel("Discussion"),
             p("Right now, we're using a basic plot of the percentage of each 
               state's COVID cases. The graph isn't really working but I want to
               turn something in anyway.")),
    tabPanel("About", 
             titlePanel("About"),
             h3("Project Background and Motivations"),
             p("My goal is to look at COVID cases, now that we have a fuller 
               understanding of them, through the lens of their racial and 
               economic breakdown based on the data from the COVID Racial
               Data project, a collaboration between the COVID Tracking
               Project and the Boston University Center for Antiracist
               Research. I also take data from the 2010 Census to find the
               proportion, per state, of the different racial make-up of each
               state. Here's hoping to find more recent data, considering it's 
               10 years old!"),
               p("I expect to find a substantial difference not only in cases to
               population proportions, but an even more dramatic one when
               comparing cases to deaths, or deaths per population. We should
               also seek to expand this data collection beyond race and into
               economic indicators, and try to statistically isolate each one
               by finding the covariance of race and income levels, but that 
               is for later!"),
               p("You can find the repository with the data and ShinyApp 
                 here, https://github.com/RuyMartini/milestone4gov50."),
             h3("About Me"),
             p("My name is Ruy Martinez and I study Government at Harvard 
             You can reach me at ruymartinez@college.harvard.edu.")))

# Define server logic required to draw a bar graph
server <- function(input, output) {

    output$casesprop <- renderPlot({
        covid_race_cases_deaths %>%
            filter(Date == input$date & State == input$state) %>%
            ggplot(aes(x = race, y = number, fill = caseordeath)) +
                geom_col(position = "dodge") +
                scale_fill_manual(name = "Type",
                                  labels = c("Case", "Death"),
                                  values = c("lightblue", "salmon")) +
                scale_x_discrete(labels = c("Asian", "Black", "Latino", "White",
                                            "AIAN", "Multiracial", "NHPI", 
                                            "Other", "Unknown")) +
                labs(title = "COVID Cases and Deaths Proportionalized",
                     subtitle = "People of color have worse outcomes. 
Missing data makes it hard to tell.",
                     x = "Race",
                     y = "Percent of Total",
                     caption = "Source: COVID Tracking Project at The Atlantic") +
                theme_classic() +
                scale_y_continuous(labels = scales::percent) }
        )}
            
        
# Don't forget to do input$whatever to indicate that it's inside the app! Also
# used col since a vector exists for y as opposed to an after_stat.
        
    

# Run the application 
shinyApp(ui = ui, server = server)
