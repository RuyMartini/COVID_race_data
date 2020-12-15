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
pop_prop <- readRDS("population_proportions.RDS") %>%
    mutate(race = as_factor(race) %>%
               fct_relevel("asianpercent", "blackpercent", "latinopercent", 
                           "whitepercent", "aianpercent", "mixedpercent", 
                           "nhpipercent", "otherpercent"))

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
                                       "IL", "IN", "KS", "KY", "LA", "MA", "MD", 
                                       "ME", "MI", "MN", "MO", "MS", "MT",
                                       "NC", "ND", "NE", "NH", "NJ", "NM", "NV",
                                       "NY", "OH", "OK", "OR", "PA", "RI",
                                       "SC", "SD", "TN", "TX", "UT", "VA",
                                       "VT", "WA", "WI", "WV", "WY"),
                                     selected = "WY"),
                         dateInput("date", label = h3("Date Input"), 
                                   value = "2020-10-11"),
                         selectInput("statepop",
                                     "STATE",
                                     c("AK", "AL", "AR", "AZ", "CA", "CO", "CT",
                                       "DC", "DE", "FL", "GA", "HI", "IA", "ID",
                                       "IL", "IN", "KS", "KY", "LA", "MA", "MD", 
                                       "ME", "MI", "MN", "MO", "MS", "MT",
                                       "NC", "ND", "NE", "NH", "NJ", "NM", "NV",
                                       "NY", "OH", "OK", "OR", "PA", "RI",
                                       "SC", "SD", "TN", "TX", "UT", "VA",
                                       "VT", "WA", "WI", "WV", "WY"),
                                     selected = "WY")),
                     
# Is this annoying to type out? Yes. Is it necessary to be able to select all
# 56 territories? Yes.
                     
                 mainPanel(plotOutput("casesprop"),
                           plotOutput("popprop"))),
                 p("It should be noted that Guam, American Samoa, the Mariana 
                 Islands, Puerto Rico, and the Virgin Islands, were taken out 
                 from the presented data due to lacking adequate data. 
                 Furthermore, there were issues with a prevalence of \"Unknown\"
                 data which may muddy results. However, I feel it would be 
                 misrepresenting the given data to omit or try to mathematically
                 resolve these unknown numbers. If 40% of the cases did not 
                 specify a race, then that is what it is, and making assumptions
                 about that is not helpful, and may in fact mislead those 
                 seeking to draw conclusions. If anything, this should speak to 
                 the dearth of data being collected of this modern pandemic that 
                has killed hundreds of thousands of people in the US alone."))),
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
                scale_y_continuous(labels = scales::percent)})
    output$popprop <- renderPlot({
        pop_prop %>%
            filter(STATE == input$statepop) %>%
            ggplot(aes(x = race, y = proportion)) +
                geom_col(fill = "lightblue") +
                theme_classic() +
                scale_y_continuous(labels = scales::percent) +
                scale_x_discrete(labels = c("Asian", "Black", "Latino", "White",
                                            "AIAN", "Multiracial", "NHPI", 
                                            "Other")) +
                labs(title = "Race as a Proportion of the Population (2010)",
                     x = "Race",
                     y = "Percent of Total",
                     caption = "Source: United States Census Bureau (2010)")
            
    })}
            
        
# Don't forget to do input$whatever to indicate that it's inside the app! Also
# used col since a vector exists for y as opposed to an after_stat.
        
    

# Run the application 
shinyApp(ui = ui, server = server)
