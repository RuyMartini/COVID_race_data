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
library(rstanarm)
library(broom.mixed)
library(gt)
library(gtsummary)

pop_prop <- readRDS("population_proportions.RDS") %>%
    mutate(race = as_factor(race) %>%
               fct_relevel("asianpercent", "blackpercent", "latinopercent", 
                           "whitepercent", "aianpercent", "mixedpercent", 
                           "nhpipercent", "otherpercent"))

covid_race_cases_deaths <- readRDS("covid_race_cases_deaths.RDS")

covid_rcd_wy <- covid_race_cases_deaths %>%
    filter(State == "WY" & caseordeath == "deaths_r") %>%
    drop_na()

fit_1_d_wy <- stan_glm(number ~ race,
                       data = covid_rcd_wy,
                       refresh = 0) 

tbl1 <- tbl_regression(fit_1_d_wy, intercept = TRUE) %>%
    as_gt() %>%
    tab_header(title = "Regression of Deaths to COVID in WY by Race",
               subtitle = "From April to October race factored into COVID") %>%
    tab_source_note("Source: Data was from the COVID Tracking Project")


# Define UI for application that draws a histogram
ui <- navbarPage(
    "COVID Deaths as a Factor of Race",
    tabPanel("Data",
             fluidPage(
                 titlePanel("COVID Cases Proportionalized"),
                 sidebarLayout(
                     sidebarPanel(
                         selectInput("state",
                                     "COVID Data: State",
                                     c("AK", "AL", "AR", "AZ", "CA", "CO", "CT",
                                       "DC", "DE", "FL", "GA", "HI", "IA", "ID",
                                       "IL", "IN", "KS", "KY", "LA", "MA", "MD", 
                                       "ME", "MI", "MN", "MO", "MS", "MT",
                                       "NC", "ND", "NE", "NH", "NJ", "NM", "NV",
                                       "NY", "OH", "OK", "OR", "PA", "RI",
                                       "SC", "SD", "TN", "TX", "UT", "VA",
                                       "VT", "WA", "WI", "WV", "WY"),
                                     selected = "WY"),
                         dateInput("date", label = "Date Input", 
                                   value = "2020-10-11"),
                         selectInput("statepop",
                                     "Population Data: State",
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
                 h1("Notes"),
                 p("The dates are seperated in periods of three.
                   The default is 10-11, and you can go backwards up to 04-12.
                   As you go further back into the data, you may notice that 
                   it becomes more or less spotty with time. This reflects the
                   capacity to collect the data at different dates."),
                 p("It should be noted that Guam, American Samoa, the Mariana 
                   Islands, Puerto Rico, and the Virgin Islands, were taken out 
                   from the presented data due to lacking adequate data. 
                   Furthermore, there were issues with a prevalence of 
                   \"Unknown\" data which may muddy results. However, I feel it 
                   would be misrepresenting the given data to omit or try to 
                   mathematically resolve these unknown numbers. If 40% of the 
                   cases did not specify a race, then that is what it is, and 
                   making assumptions about that is not helpful, and may in fact
                   mislead those seeking to draw conclusions. If anything, this 
                   should speak to the dearth of data being collected of this 
                   modern pandemic that has killed hundreds of thousands of 
                   people in the US alone."))),

    tabPanel("Model",
             titlePanel("Predictive Model"),
             p("Below is a table of a predictive model that takes into account
               race in general in Wyoming to predict deaths. Wyoming is the 
               clearest model we have for a one state analysis. 
               Below there is a general model that examines race as a predictor 
               of death to COVID across the US."),
             gt_output("wydtable"),
             p("This tell us that our intercept, being Asian-American is
               such a minor predictor on whether or not you'll die of COVID.
               But it also goes on to show that in general, in Wyoming, if
               one were to imagien the same conditions repeated, the ratio
               of the dead would come out something like is shown. Considering
               that the population of Wyoming is 75% white, that is incredibly
               odd, and considering that the AIAN population is around 5%,
               we should be incredibly alarmed.")),

    tabPanel("Discussion",
             titlePanel("Discussion"),
             h1("Data Visualization"),
             p("There is a reason when this ShinyApp is opened, it defaults to
               what might be the most egregious state with the clearest data:
               Wyoming. Wyoming has some egregious numbers, with American
               Indians facing incredibly high death rates for their case to 
               proportion of the population. The Latino minority is likewise
               under immense stress due to the pandemic."),
             p("However, this data is less clear among all states. As mentioned
               in the notes of the Data Visualization section, there were many
               cases that simply had no race data. While one might have gotten
               rid of this data or manipulatd it to evenly proportion itself,
               this would be a massive mistake. There is no evidence to support
               the claim that the unknown racial data would spread evenly — or
               unevenly. False conclusions could easily be drawn. Furthermore,
               there was a complication within the \"Latino\" and ethnic
               categories that resulted in many states not having data on their
               Latino or Hispanic population, with whatever varying metric used.
               So what do we make of this?"),
             p("There is clearly a racial disparity, with marginalized groups,
               especially Black and American Indians, having disproportionately
               high amounts of deaths and cases compared to the population.
               This alone should alarm us. However, the data becomes more 
               apparent upon examining the predictive model.")),
    tabPanel("About", 
             titlePanel("About"),
             h3("Project Background and Motivations"),
             p("My goal is to look at COVID cases, now that we have a fuller 
               understanding of them, through the lens of their racial breakdown 
               based on the data from the COVID Racial Data project, a 
               collaboration between the COVID Tracking Project with The 
               Atlantic and the Boston University Center for Antiracist
               Research. I also take data from the 2010 Census to find the
               proportion, per state, of the different racial make-up of each
               state."),
             p("I have found a substantial difference not only in cases to
               population proportions, but an even more dramatic one when
               comparing cases to deaths, or deaths per population. However,
               alarmingly there is a lack of consistency in the data collection
               methodology, resulting in mixups with Latino as a race versus
               Hispanic as an ethnicity, along with dearths of cases and deaths
               with no racial data at all. This muddies the waters."),
             p("You can find the repository with the data and ShinyApp 
               here, https://github.com/RuyMartini/COVID_race_data"),
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
                     caption = "Source: United States Census Bureau (2010)")})
    output$wydtable <- render_gt(expr = tbl1,
                                 height = px(600),
                                 width = px(600))
    }
            
        
# Don't forget to do input$whatever to indicate that it's inside the app! Also
# used col since a vector exists for y as opposed to an after_stat.
        
    

# Run the application 
shinyApp(ui = ui, server = server)
