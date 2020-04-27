

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

incomeasvector <- income_county_ns$abb_county

income_county_ns <- readRDS("income_county_ns.RDS")

ic_2016 <- readRDS("ic_2016.RDS")

ic_2017 <- readRDS("ic_2017.RDS")

ic_2018 <- readRDS("ic_2018.RDS")

ic_rank_2018 <- readRDS("ic_rank_2018.RDS")

# I used options in order to properly load in my data, because otherwise my
# data wouldn't be loaded in.

grad_ed <- readRDS("grad_ed.RDS")

districtasvector <- grad_ed$school_district %>% unique()

options(scipen = 999)

# For this first part of my shiny, I wanted to allow people to compare how 
# their county voted against everyone else's county, and also how the total
# votes of counties changed over the course of the five presidential elections.
# I used a comparative bar plot and a line plot in order to best represent
# this.

ui <- navbarPage(
  
    "How Separate Variables Affect Voting Patterns",
  
    tabPanel("Voting",
             
             tabsetPanel(tabPanel("Presidential Election Data Since 2000",
                                  
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
                                        
                                        tableOutput("results2"))))),
                         
                         tabPanel("Explanation"),
                         
                         h3("Why these graphs?"),
                         
                         p("The first of these two graphs offers people the opportunity to compare the voting patterns of different counties
                           across the United States. Later on, I examine how income, education, and health each impact the total number of votes
                           cast in 2016.
                           
                           The secong graph is a line graph charting the change in the total number of votes cast by the selected counties for the
                           last five presidential elections, which is useful for informing us how outside variables have impacted voting growth
                           (or lack thereof).")
                         
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
                                      
                                      fluidRow(selectizeInput("in_county", "Choose a county:", choices = incomeasvector, multiple = TRUE),
                                               
                                               selectInput("in_y_var", "Choose a variable to measure:", choices = c(`Personal Income 2016` = "ic_2016", `Personal Income 2017` = "ic_2017", `Personal Income 2018` = "ic_2018", `Personal Income Rank 2018` = "ic_rank_2018")))
                                  ),
                                  
                                  mainPanel(
                                      
                                      plotOutput("income2"),
                                      
                                      br(), br(),
                                      
                                      tableOutput("results4"))
                              
                                  ))),
               
               tabPanel("Explanation",
                        
                        p("It is a fact that people of a higher socioeconomic status are more likely to vote in elections. There are several possible
                          reasons for this, including a lack of reliable transportation, a lack of education, and needing to work long
                          hours. In this section of my project, I produced two graphs. The first looks at income brackets for the United States as a
                          whole, and how each of these classes votes. The second graph gives us the opportunity to examine income on a per-county basis,
                          which is useful for determining why certain counties may have higher turnout than others."
                          
                        )))),
    
# This education data is not fantastic, so I have downloaded some more to make
# up for that. My new education data focuses mainly on the graduation rates
# for school districts across the country, which I think will be invaluable
# to compare with income and health to inform how people voted in elections.



    tabPanel("Education",
             
             tabsetPanel(
               
               tabPanel("Education Data",
                                  
                                  fluidPage(
                                    
                                    sidebarLayout(
                                      
                                      helpText("Examine Education Data."),
                                      
                                      fluidRow(selectInput("year2", "Choose a year:", choices = grad_ed$year),
                                               
                                               selectizeInput("school_district", "Choose a school district:", choices = districtasvector, options = list("actions-box" = TRUE), multiple = TRUE),
                                      )),
                                    
                                    mainPanel(
                                      
                                      plotOutput("education"),
                                      
                                      br(), br(),
                                      
                                      tableOutput("results5"),
                                      
                                      plotOutput("education2"),
                                      
                                      br(), br(),
                                      
                                      tableOutput("results6")
                                    ))),
               tabPanel("Explanation",
                        p("Education is the gateway to becoming more sophisticated, smarter, and learning how to play your part as an American citizen
                          and a citizen of the world. If a particular school district suffers due to a lack of education funding, then people are likely
                          to assume that students that graduate from (or don't graduate from) that district will be less likely to vote in elections,
                          thereby decreasing voter turnout for that particular state. This particular set of education data gives us the graduation rate of
                          every school district across the country. I wanted to give people another opportunity to compare different school districts in
                          different states/counties so we can see how education may impact voting."
                          
                        ))
    )),

    tabPanel("Regression",
             
             p("Unfortunately, I am still working on cleaning the data for the regression. It has taken me hours and hours and I am still not done, so please
               forgive me :(
               
               What I plan to do for the regression is examine income and total votes in 2016, since I am able to combine those two data sets. If I can do something
               similar with health and education, I will do my best to. I predict that the income will show a positive correlation with total votes (i.e. the more
               money you have, the more likely you are to vote, which tuus increases the number of total votes.")
             
             ),
    
    tabPanel("Discussion",
             
             titlePanel("Connor, enlighten us."),
             
             p("Most of the models I used for my project were interactive bar graphs. The thing I had in mind the most during
               the creation of this project was how to let people compare different areas. I think it's really
               great to be able to compare different counties against one another, so you can look not just at a
               per-state basis but on an even more specific level. Using selectizeInput allows you to select which
               counties you want to compare with one another.
               
               The data I chose to use was presidential election results, education, and income. None of this data is
               new, but I think presenting it in an interactive way helps illustrate how disparities in education and 
               income affects voting patterns across states and counties.")),
    
    
    tabPanel("About",
             
             titlePanel("About"),
             
             h3("Project Background and Motivations"),
             
             p("I think that, now more than ever, it is important to understand how and why voting patterns occur. 
               While none of this is new information, and it may be intuitive for some, it is still important to 
               lay it out in an easily digestible form so people of every background can understand the severity 
               of the situation we face in this country."),
             
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
            
            ggplot(aes(total_income, reported_voted, fill = total_income)) +
            
            geom_bar(stat = "identity", position = position_dodge()) +
            
            theme_bw() + labs(
                
                title = "Measuring Income Statistics for Different Groups",
                
                x = "Income Bracket",
                
                y = "Number Reported Voted"
            )
        
    })
    
    output$income2 <- renderPlot({
      
      incomevars <- get(input$in_y_var)
        
        incomevars %>%
          
          filter(county %in% input$in_county) %>%
          
          ggplot(aes(county, y, fill = county)) +
          
          geom_bar(stat = "identity", position = position_dodge()) +
          
          theme_bw() + labs(
            
            title = "Income Statistics by County",
            
            x = "County",
            
            y = "Personal Income/Ranking"
          )
      
    })
    
    output$education <- renderPlot({
      
      grad_ed %>%
        
        filter(school_district %in% input$school_district) %>%
        
        filter(year == input$year2) %>%
        
        ggplot(aes(school_district, percent, fill = school_district)) + 
        
        geom_bar(stat = "identity", position = position_dodge()) + theme_bw() +
        
        ggtitle(paste("Graduation Rate Percentage by School District, Year = ", input$year2)) +
        
        labs(
          
          x = "School District",
          
          y = "Percent"
        )
        
    })
    
    output$education2 <- renderPlot({
      
      grad_ed %>%
        
        filter(school_district %in% input$school_district) %>%
        
        ggplot(aes(year, percent, color = school_district)) + 
        
        geom_line(stat = "identity", position = position_dodge()) + theme_bw() +
        
        ggtitle(paste("Graduation Rate Percentage Change by School District, 2006-2016")) +
        
        labs(
          
          x = "Year",
          
          y = "Percent"
        )
          
    })
    
# I wanted to insert an image because I haven't made a video yet!
  
    output$image <- renderImage({
        
        filename <- normalizePath(file.path('.', paste('image', '.jpg', sep='')))
        
        list(src = filename)
        
    }, deleteFile = FALSE)

    
  
}


shinyApp(ui = ui, server = server)


