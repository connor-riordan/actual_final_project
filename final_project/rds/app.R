

library(shiny)
library(tidyverse)
library(markdown)



pres_res <- readRDS("pres_results.RDS")
    
options(scipen = 999)


ui <- navbarPage(
    
    "How Separate Variables Affect Voting Patterns",
    
    tabPanel("Presidential Election Data Since 2000",
             fluidPage(
                 sidebarLayout(
                 sidebarPanel(
                     helpText("Examine Presidential Election Data."),
                     
                     fluidRow(selectInput("year", "Choose a year:", choices = pres_res$year),
                              selectInput("county", "Choose a county:", choices = pres_res$county),
                              selectInput("y_var", "Choose what you want to measure:", choices = c(colnames(pres_res)[-c(1:6)]))
                              )),
                 
                 mainPanel(
                     plotOutput("voting_data"),
                     br(), br(),
                     tableOutput("results")))
    )),
    
    tabPanel("Running Regressions and Discussion",
             titlePanel("Discussion Title"),
             p("Tour of the modeling choices you made and 
              an explanation of why you made them")),
    
    tabPanel("About",
             titlePanel("About"),
             h3("Project Background and Motivations"),
             p("I think that, now more than ever, it is important to understand how and why voting patterns occur. While none of this is new information, and it may be intuitive for some, it is still important to lay it out in an easily digestible form so people of every background can understand the severity of the situation we face in this country."),
             h3("About Me"),
             p("My name is Connor Riordan and I am looking to study Government and TDM. 
             You can reach me at criordan@college.harvard.edu."))
)

server <- function(input, output) {
    output$voting_data <- renderPlot({
        # w <- pres_res[pres_res$year == input$year]
        x <- seq(1, nrow(pres_res[pres_res$county == input$county, ]), 1)
        y <- pres_res[pres_res$county == input$county, input$y_var]
        dat <- as.data.frame(cbind(x, y))
        dat %>%
            # filter(year == w) %>%
            ggplot(aes(x, y)) + 
            scale_x_discrete(name = 'county',
                             breaks = x,
                             labels = paste(x),
                             limits = x) + 
            geom_bar(stat = "identity") + theme_classic() + labs(
                title = "Presidential Election Data"
            )
    })
}


shinyApp(ui = ui, server = server)

# Questions for Amy for pres_res:
# How do I filter by year and have year specified by input$year?
# How can I choose a different color for different inputs? Ex: having bars colored by party when they select the party variable, or
# having different colors for each candidate on a single bar?
# How should I clean up the data? (specific on other page)
# How to I make my outputs correspond with my inputs with multiple tabs/graphs?
