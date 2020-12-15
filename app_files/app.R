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

# When we load up the population proportion data, we need to be able to move
# the columns around when we graph it. We need to turn the race names into
# factors.
               
               fct_relevel("asianpercent", "blackpercent", "latinopercent", 
                           "whitepercent", "aianpercent", "mixedpercent", 
                           "nhpipercent", "otherpercent"))

# Now we manually relevel them to what is below.

covid_race_cases_deaths <- readRDS("covid_race_cases_deaths.RDS")

# Grab our incredibly long data from the data.Rmd file, all tidy for us in an 
# RDS format!

covid_rcd_wy <- covid_race_cases_deaths %>%
    filter(State == "WY" & caseordeath == "deaths_r") %>%
    drop_na()

# Our model will show COVID deaths in Wyoming, but we don't want to get rid of
# our wonderful data to visualize, so we make a new object and do the necessary
# wrangling.

fit_1_d_wy <- stan_glm(number ~ race,
                       data = covid_rcd_wy,
                       refresh = 0) 

# Make a fitted Bayesian linear model that gets the number, or proportion
# of the dead based on race if we were to repeat this scenario again.

tbl1 <- tbl_regression(fit_1_d_wy, intercept = TRUE) %>%
    as_gt() %>%
    tab_header(title = "Regression of Deaths to COVID in WY by Race",
               subtitle = "From April to October race factored into COVID") %>%
    tab_source_note("Source: Data was from the COVID Tracking Project")

# Turn this into a table for us to visualize in our model section using 
# gtsummary!


# Define UI for application that draws a histogram
ui <- navbarPage(
    "COVID Cases and Deaths as a Factor of Race",
    tabPanel("Data",
             fluidPage(
                 titlePanel("COVID Cases Proportionalized"),
                 sidebarLayout(
                     sidebarPanel(
                         selectInput("state",
                                     "COVID Data: State",
                
# We want to make it clear that this is the COVID visualization that you are
# selecting the state for.
                                     
                                     c("AK", "AL", "AR", "AZ", "CA", "CO", "CT",
                                       "DC", "DE", "FL", "GA", "HI", "IA", "ID",
                                       "IL", "IN", "KS", "KY", "LA", "MA", "MD", 
                                       "ME", "MI", "MN", "MO", "MS", "MT",
                                       "NC", "ND", "NE", "NH", "NJ", "NM", "NV",
                                       "NY", "OH", "OK", "OR", "PA", "RI",
                                       "SC", "SD", "TN", "TX", "UT", "VA",
                                       "VT", "WA", "WI", "WV", "WY"),
                                     selected = "WY"),

# As mentioned below, the absolute worst offenders or those with no data
# have been removed because it's just impossible to tell what is going on when
# 100% of the data is unknown. Perhaps in an update it'll be put back 
# just show it so the world can see how bad the collection is! Wyoming is chosen
# as mentioned in the notes because it is a wonderful example that should
# pique any viewer.

                         dateInput("date", label = "Date Input", 
                                   value = "2020-10-11"),

# Here one can choose the date in a neat calender.

                         selectInput("statepop",
                                     "Population Data: State",
                                     
# Once again, being very clear that now you are chosing the general population
# data!
                                     
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
# 51 states and DC? Yes.
                     
                 mainPanel(plotOutput("casesprop"),
                           plotOutput("popprop"))),

# This will output both graphs below.

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

# This closes the first section with all of my commentary.

    tabPanel("Model",
             titlePanel("Predictive Model"),
             p("Below is a table of a predictive model that takes into account
               race in general in Wyoming to predict deaths. Wyoming is the 
               clearest model we have for a one state analysis."),
             gt_output("wydtable"),
             
# This puts the graph in the middle with some analysis below.
             
             p("Our intercept - being Asian-American is
               such a minor predictor on whether or not you'll die of COVID.
               Our linear regression model shows that in general, in Wyoming, if
               one were to imagine the same conditions repeated, the ratio
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
        
# This loads up the first plot, casesprop. Because shiny reads the server
# function as an array with many columns, we have to specify it using the $
# sign. In this case, we would have an output array with all of our output
# graphs and plots and then choose which one we want. 
        
# renderPlot is Shiny's specific command for rendering the kinds of ggplots
# we usually think of.
        
        covid_race_cases_deaths %>%
            filter(Date == input$date & State == input$state) %>%
            
# When we select for something in the UI, we are creating an array called input,
# and it has all of the things we select for, the date and state in the COVID
# graph, the state in the population graph, and you could imagine more if we
# get more and more things.
            
            ggplot(aes(x = race, y = number, fill = caseordeath)) +
                geom_col(position = "dodge") +
            
# This seperates the columns so we can compare the cases to deaths and also
# visualize them as both being a part of the number 1.
            
                scale_fill_manual(name = "Type",
                                  labels = c("Case", "Death"),
                                  values = c("lightblue", "salmon")) +
                scale_x_discrete(labels = c("Asian", "Black", "Latino", "White",
                                            "AIAN", "Multiracial", "NHPI", 
                                            "Other", "Unknown")) +
            
# The fill scales command lets us visualize the cases or deaths in a more nice
# way. The x scales command lets us rename the labels without resorting to
# factors when it's not needed.
            
                labs(title = "COVID Cases and Deaths Proportionalized",
                     subtitle = "People of color have worse outcomes. 
Missing data makes it hard to tell.",
                     x = "Race",
                     y = "Percent of Total",
                   caption = "Source: COVID Tracking Project at The Atlantic") +
            
# Strange formatting was necessary here to make sure everything fits if one were
# to print out the code for a paper analysis. While it's not gorgeous, 
# I believe it's intelligible enough.
            
                theme_classic() +
                scale_y_continuous(labels = scales::percent)
    
# Simple edit that makes the decimals come out as a percentage.
        
        })
    
# Now we've closed this object, output$casesprop, but we're still inside 
# defining the server function, and we have more graphs to go. Be careful
# with your brackets and parenthesis'.
    
    output$popprop <- renderPlot({
        pop_prop %>%
            
# Same as above in terms of calling the vector, but another note, notice that
# we gave a very specific name to our output in the UI that we now call here.
# Don't forget what you named things above has to be repeated below!
            
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
        
# We've seen this before, it's the same formatting and whatnot.
        
        })
    
    output$wydtable <- render_gt(expr = tbl1,
                                 height = px(600),
                                 width = px(600))
    
# Now we want to output a gt table, which is a little different. We still
# call the array output and a specific column, but we use the render_gt function
# to call our table. We use expr to define the object that we made way up top,
# and then we have aesthetic things we can mess around with. To change the gt
# table though, that happens above.
    
    }
    

# Run the application. Notice how it now calls the UI and server functions!

shinyApp(ui = ui, server = server)
