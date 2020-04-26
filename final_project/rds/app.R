

library(shiny)
library(tidyverse)
library(markdown)
library(ggplot2)
library(base)

# I needed to use this space in order to read in all of my RDS data so I can
# use it all in my shiny app.

# This is for all the presidential election data from 2000 to 2016.

pres_res <- readRDS("pres_results.RDS")

# I turned all of the selections from the abb_county column to vectors to make
# the selection process easier in selectizeInput.

presasvector <- pres_res$abb_county %>% unique()

# This is the data that has all of the different age groups and income levels
# and how each of them voted durng the election. "ai" stands for age and 
# income.

age_income_2016 <- readRDS("age_income_2016.RDS")
    
ai_18_over <- readRDS("age_income_2016_18_over.RDS")

ai_18_24 <- readRDS("age_income_2016_18_24.RDS")

ai_25_44 <- readRDS("age_income_2016_25_44.RDS")

ai_45_64 <- readRDS("age_income_2016_45_64.RDS")

ai_65_74 <- readRDS("age_income_2016_65_74.RDS")

ai_75_over <- readRDS("age_income_2016_75_over.RDS")

# While I haven't done anything with it yet, this is education data for schools
# across the country.

education <- readRDS("education.RDS")

# Like I did with the presidential results, I had to put the education column
# as vectors to make it easier to select the input.

edasvector <- education$name_of_area %>% unique()

# This breaks down the income for each individual county.

income_county <- readRDS("income_county.RDS")

# A trend is certainly appearing. I wanted to make all of the income brackets
# as vectors as well.

incomeasvector <- income_county$county

# I used options in order to properly load in my data, because otherwise my
# data wouldn't be loaded in.

options(scipen = 999)

# For this first part of my shiny, I wanted to allow people to compare how 
# their county voted against everyone else's county, and also how the total
# votes of counties changed over the course of the five presidential elections.
# I used a comparative bar plot and a line plot in order to best represent
# this.

ui <- navbarPage(
  
    "How Separate Variables Affect Voting Patterns",
    
    tabPanel("Presidential Election Data Since 2000",
             
             fluidPage(
                 
                 sidebarLayout(
                 
                     sidebarPanel(
                     
                         helpText("Examine Presidential Election Data."),
                     
                     fluidRow(selectInput("year", "Choose a year:", choices = pres_res$year),
                              
                              selectizeInput("county", "Choose a county:", choices = presasvector, options = list("actions-box" = TRUE), multiple = TRUE)
                              
                              )),
                 
                 mainPanel(
                     
                     plotOutput("voting_data"),
                     
                     br(), br(),
                     
                     tableOutput("results"),
                     
                     plotOutput("lpd"),
                     
                     br(), br(),
                     
                     tableOutput("results2")))
    )),
    
# For this next tab, I wanted to look at income data for different age groups
# for the nation and also by county. The second part does not work yet, but for
# the first part I had to save each age group as a separate dataframe, and then
# when the inputs change it switches between data frames. What I plan to do for
# the rest of the data is to allow people to choose both which county they want
# to measure and also what y-variable they would like to examine on the graph.
# So far, I have had no luck, but I will have luck soon.
    
    tabPanel("Income",
             tabsetPanel(
                 tabPanel("Income Statistics for Various Age Groups",
                          
                          fluidPage(
                              
                              sidebarLayout(
                                  
                                  sidebarPanel(
                                      
                                      helpText("Examine Income Data."),
                                      
                                      fluidRow(selectInput("age", "Choose an age group:", choices = c(`Aged 18 or older` = "ai_18_over", `Aged 18 to 24` = "ai_18_24", `Aged 25 to 44` = "ai_25_44", `Aged 45 to 64` = "ai_45_64", `Aged 65 to 74` = "ai_65_74", `Aged 75 and older` = "ai_75_over")),
                                               
                                               selectizeInput("total_income", "Choose an income bracket:", choices = age_income_2016$total_income, options = list("actions-box" = TRUE), multiple = TRUE))),
                                  
                                  mainPanel(
                                      
                                      plotOutput("income"),
                                      
                                      br(), br(),
                                      
                                      tableOutput("results3"))
                              )
                          )
                 ),
                 
                 tabPanel("More Income Statistics",
                          
                          fluidPage(
                              
                              sidebarLayout(
                                  
                                  sidebarPanel(
                                      
                                      helpText("Examine Income Data."),
                                      
                                      fluidRow(selectizeInput("county", "Choose a county:", choices = incomeasvector, multiple = TRUE),
                                               
                                               selectInput("ed_y_var", "Choose a variable to measure:", choices = c(colnames(income_county)[-c(1)])))
                                  ),
                                  
                                  mainPanel(
                                      
                                      plotOutput("income2"),
                                      
                                      br(), br(),
                                      
                                      tableOutput("results4"))
                              )))
             )),
    
# This education data is not fantastic, so I have downloaded some more to make
# up for that. My new education data focuses mainly on the graduation rates
# for school districts across the country, which I think will be invaluable
# to compare with income and health to inform how people voted in elections.

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
                
                tableOutput("results5")
            )
        )
    ),
    
# This is the discussion part and where I am going to run my regression. I
# haven't had a chance to actually finish running my regression and put it in
# my data because I had to spend all night cleaning it up in order to actually
# join the presidential election data and the income data, which are the two
# sets that I want to join for the regression.
    
    tabPanel("Running Regressions and Discussion",
             
             titlePanel("Discussion Title"),
             
             p("Tour of the modeling choices you made and 
              an explanation of why you made them")),
    
    mainPanel(
        
        tableOutput("regression")
    
        ),
    
    
    tabPanel("About",
             
             titlePanel("About"),
             
             h3("Project Background and Motivations"),
             
             p("I think that, now more than ever, it is important to understand how and why voting patterns occur. While none of this is new information, and it may be intuitive for some, it is still important to lay it out in an easily digestible form so people of every background can understand the severity of the situation we face in this country."),
             
             h3("About Me"),
             
             p("My name is Connor Riordan and I am looking to study Government and TDM. 
             You can reach me at criordan@college.harvard.edu."),
             
             mainPanel(imageOutput("image"))
             
             )
)

# These are all the outputs for my data.

server <- function(input, output) {
    
    output$voting_data <- renderPlot({
        
        pres_res %>%
            
            filter(abb_county %in% input$county) %>%
            
            filter(year == input$year) %>%
            
            ggplot(aes(candidate, candidatevotes, fill = county)) + 
            
            geom_bar(stat = "identity", position = position_dodge()) +
            
            ggtitle(paste("Presidential Election Results by County, Year = ",input$year)) + 
            
            theme_bw() + labs(
                
                x = "County",
                
                y = "Number of Votes for Each Candidate"
            )
        
    })
    
    output$lpd <- renderPlot({
        pres_res %>%
            
            filter(abb_county %in% input$county) %>%
            
            ggplot(aes(year, totalvotes, color = abb_county)) +
            
            geom_line(stat = "identity", position = position_dodge()) + 
            
            theme_bw() + labs(
                
                title = "Change in Total Votes by County, 2000-2016",
                
                color = "County",
                
                x = "County",
                
                y = "Total Votes"
            )
    })
    
    output$income <- renderPlot({
        
        test <- get(input$age)
        
        test  %>% 
            
            filter(total_income %in% input$total_income) %>%
            
            ggplot(aes(total_income, reported_voted)) +
            
            geom_bar(stat = "identity", position = position_dodge()) +
            
            theme_bw() + labs(
                
                title = "Measuring Income Statistics for Different Groups",
                
                x = "Income Bracket",
                
                y = "Number Reported Voted"
            )
        
    })
    
    output$income2 <- renderPlot({
        
        income_county %>% 
            
            filter(county %in% c("Alabama")) %>%
            
            ggplot(aes(county, personal_income_2017)) +
            
            geom_bar(stat = "identity", position = position_dodge()) +
            
            theme_bw()
        
    })
    
    output$regression <- renderTable({
        
    })
    
# I wanted to insert an image because I haven't made a video yet!
  
    output$image <- renderImage({
        
        filename <- normalizePath(file.path('.', paste('image', '.jpg', sep='')))
        
        list(src = filename)
        
    }, deleteFile = FALSE)

    
  
}


shinyApp(ui = ui, server = server)


