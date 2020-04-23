

library(shiny)
library(tidyverse)
library(markdown)
library(ggplot2)



pres_res <- readRDS("pres_results.RDS")

presasvector <- pres_res$abb_county %>% unique()

age_income_2016 <- readRDS("age_income_2016.RDS")
    
ai_18_over <- readRDS("age_income_2016_18_over.RDS")

ai_18_24 <- readRDS("age_income_2016_18_24.RDS")

ai_25_44 <- readRDS("age_income_2016_25_44.RDS")

ai_45_64 <- readRDS("age_income_2016_45_64.RDS")

ai_65_74 <- readRDS("age_income_2016_65_74.RDS")

ai_75_over <- readRDS("age_income_2016_75_over.RDS")

education <- readRDS("education.RDS")

edasvector <- education$name_of_area %>% unique()

options(scipen = 999)


ui <- navbarPage(
    
    "How Separate Variables Affect Voting Patterns",
    
    tabPanel("Presidential Election Data Since 2000",
             
             fluidPage(
                 
                 sidebarLayout(
                 
                     sidebarPanel(
                     
                         helpText("Examine Presidential Election Data."),
                     
                     fluidRow(selectInput("year", "Choose a year:", choices = pres_res$year),
                              
                              selectizeInput("county", "Choose a county:", choices = presasvector, options = list("actions-box" = TRUE), multiple = TRUE),
                              
                              selectInput("y_var", "Choose what you want to measure:", choices = c(colnames(pres_res)[-c(1:6)]))
                              
                              )),
                 
                 mainPanel(
                     
                     plotOutput("voting_data"),
                     
                     br(), br(),
                     
                     tableOutput("results"),
                     
                     plotOutput("lpd"),
                     
                     br(), br(),
                     
                     tableOutput("results2")))
    )),
   
    tabPanel("Income Statistics for Various Age Groups",
            
             fluidPage(
                
                 sidebarLayout(
                    
                     sidebarPanel(
                        
                         helpText("Examine Income Data."),
                    
                         fluidRow(selectizeInput("age", "Choose an age group:", choices = c(`Aged 18 or older` = "ai_18_over", ai_18_24 = "ai_18_24", ai_25_44 = "ai_25_44", ai_45_64 = "ai_45_64", ai_65_74 = "ai_65_74", ai_75_over = "ai_75_over"), options = list("actions-box" = TRUE), multiple = TRUE))),
                
                 mainPanel(
                     
                     plotOutput("income"),
                     
                     br(), br(),
                     
                     tableOutput("results3"))
                    )
            )
    ),
    
    tabPanel("Education Data",
        fluidPage(
            sidebarLayout(
                helpText("Examine Education Data."),
                fluidRow(selectizeInput("state", "Choose a sate:", choices = edasvector, options = list("actions-box" = TRUE), multiple = TRUE),
                         selectInput("y-var", "Choose a variable to measure:", choices = c(colnames(education)[-c(1:2)]))),
            ),
            mainPanel(
                plotOutput("education"),
                br(), br(),
                tableOutput("results4")
            )
        )
    ),
    
    
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
        
        pres_res %>%
            
            filter(abb_county %in% input$county) %>%
            
            filter(year == input$year) %>%
            
            ggplot(aes(candidate, candidatevotes, fill = county)) + 
            
            geom_bar(stat = "identity", position = position_dodge()) +
            
            ggtitle(paste("Presidential Election Results by County, Year = ",input$year))
        
    })
    
    output$lpd <- renderPlot({
        pres_res %>%
            
            filter(abb_county %in% input$county) %>%
            
            ggplot(aes(year, totalvotes, fill = abb_county)) +
            
            geom_line(stat = "identity", position = position_dodge())
    })
    
    output$income <- renderPlot({
        
        test <- get(input$age)
        test  %>% 
            
            ggplot(aes(total_income, reported_voted)) +
            
            geom_bar(stat = "identity", position = position_dodge())
        
        #if(input$age == "18+"){
        #    
        #    ai_18_over %>% 
        #        
        #        ggplot(aes(total_income, reported_voted)) +
        #        
        #        geom_bar(stat = "identity", position = position_dodge())
        #} 
    })
    
    output$education <- renderPlot({
        
        #education %>% 
        #    
        #    filter(qualifying_name %in% input$state) %>%
        #        
        #    ggplot(aes(qualifying_name, get(input$y_var))) +
        #        
        #    geom_bar(stat = "identity", position = position_dodge())
        
        education %>% 
            
            filter(qualifying_name %in% input$state) %>%
            
            ggplot(aes(qualifying_name, get(input$`y-var`))) +
            
            geom_bar(stat = "identity", position = position_dodge())
        
    })
}


shinyApp(ui = ui, server = server)

# Questions for Amy for pres_res:
# How do I filter by year and have year specified by input$year?
# How can I choose a different color for different inputs? Ex: having bars colored by party when they select the party variable, or
# having different colors for each candidate on a single bar?
# How should I clean up the data? (specific on other page)
# How to I make my outputs correspond with my inputs with multiple tabs/graphs?
