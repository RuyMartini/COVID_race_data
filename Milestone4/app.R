#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- navbarPage(
    "COVID Deaths as a Factor of Race and Ethnicity",
    tabPanel("Model",
             fluidPage(
                 titlePanel("COVID Cases Proportionalized"),
                 sidebarLayout(
                     sidebarPanel(
                         selectInput(
                             "plot_type",
                             "Plot Type",
                             c("Option A" = "a", "Option B" = "b")
                         )),
                     mainPanel(plotOutput("line_plot")))
             )),
    tabPanel("Discussion",
             titlePanel("Discussion Title"),
             p("Tour of the modeling choices you made and 
              an explanation of why you made them")),
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

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
