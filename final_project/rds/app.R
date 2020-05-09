
library(shiny)
library(readr)
library(tidyverse)
library(janitor)
library(gt)
library(skimr)
library(broom)
library(patchwork)
library(lubridate)
library(markdown)
library(shinythemes)

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

income_county_ns <- readRDS("income_county_ns.RDS")

incomeasvector <- income_county_ns$abb_county

ic_2016 <- readRDS("ic_2016.RDS")

ic_2017 <- readRDS("ic_2017.RDS")

ic_2018 <- readRDS("ic_2018.RDS")

ic_rank_2018 <- readRDS("ic_rank_2018.RDS")

# I used options in order to properly load in my data, because otherwise my
# data wouldn't be loaded in.

grad_ed <- readRDS("grad_ed.RDS")

districtasvector <- grad_ed$school_district %>% unique()

regression <- readRDS("regression.RDS")

regression_log <- readRDS("regression_log.RDS")

health <- readRDS("health.RDS")

health_18_munhealthy <- readRDS("health_18_munhealthy.RDS")

health_18_punhealthy <- readRDS("health_18_punhealthy.RDS")

helath_child_mortality <- readRDS("health_child_mortality.RDS")

health_costs_adjusted_medicare <- readRDS("health_costs_adjusted_medicare.RDS")

health_dentists <- readRDS("health_dentists.RDS")

health_dentists_rate <- readRDS("health_dentists_rate.RDS")

health_food_environmental_index <- readRDS("health_food_environmental_index.RDS")

health_infant_mortality <- readRDS("health_infant_mortality.RDS")

health_pct_drinking <- readRDS("health_pct_drinking.RDS")

health_pct_smokers <- readRDS("health_pct_smokers.RDS")

health_percent_woinsurance_18_64 <- readRDS("health_percent_woinsurance_18_64.RDS")

health_percent_woinsurance_under19 <- readRDS("health_percent_woinsurance_under19.RDS")

health_primary_phys <- readRDS("health_primary_phys.RDS")

health_primary_phys_rate <- readRDS("health_primary_phys_rate.RDS")

health_18_pctfairpoor <- readRDS("helath_18_pctfairpoor.RDS")

options(scipen = 999)

# For this first part of my shiny, I wanted to allow people to compare how 
# their county voted against everyone else's county, and also how the total
# votes of counties changed over the course of the five presidential elections.
# I used a comparative bar plot and a line plot in order to best represent
# this.

ui <- navbarPage(theme = shinytheme("cosmo"),
  
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
                                        
                                        fluidRow(
                                          
                                          splitLayout(
                                            
                                            style = "border: 1px solid silver:", cellWidths = c(600, 600),
                                            
                                            plotOutput("voting_data"),
                                            
                                            plotOutput("lpd"))))))),
                         
                      tabPanel("Explanation"),
                         
                         h3("Why these graphs?"),
                         
                         p("The first of these two graphs offers people the opportunity to compare the voting patterns of different counties
                           across the United States. Later on, I examine how income has an impact the total number of votes
                           cast in 2016. The first graph allows you to see how many votes each county cast for each presidential candidate for
                           the last five presidential elections."),
                           
                          p("The secong graph is a line graph charting the change in the total number of votes cast by the selected counties for the
                           last five presidential elections. It is useful to be able to compare counties to see if voter turnout is increasing or
                           decreasing, and where that is happening. Being able to examine each county individually is a useful tool to see where we
                           need to focus our efforts in order to bolster voter participation."),
                           
                          p("When we combine these two graphs, this offers some insight into both 1) how each county voted in the last five presidential
                           elections, 2) how many votes were cast for each candidate, and 3) if the county is seeing increased or decreased voter
                           turnout over time. This can be used as a resource for both democratic and republican strategists to determine where they should
                           focus their efforts come election season.")
                         
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
                        
                        h3("Why include income?"),
                        
                        p("The first set of income statistics looks at the United States as a whole, and specifically on how many people 
                          of each income class (in their sample) voted in 2016. Comparing the data side-by-side seems to indicate that
                          people who are of a higher income bracket were more likely to voe in the 2016 presidential election."),
                          
                        p("The second set of income statistics gives up the opportunity to examine individual personal income by county.
                          Apart from voting, we can use these statistics to see where poverty hits hardest in the United States. In terms of
                          voting, this graph can be combined with the other set of income statistics because we can see which counties fall
                          into which income bracket, and we can extrapolate from there which counties had higher voter turnouts."),
                          
                         p("Judging by these two sets of data, we can reasonably predict that 
                          people of a higher socioeconomic status are more likely to vote in elections. There could be several possible
                          reasons for this, including a lack of reliable transportation, a lack of education, and needing to work long
                          hours.")
                          
                        ))),
    
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
                                               
                                               selectizeInput("school_district", "Choose a school district:", choices = districtasvector, options = list("actions-box" = TRUE), multiple = TRUE)
                                      )),
                                    
                                    mainPanel(
                                      
                                      fluidRow(
                                        
                                        splitLayout(
                                          
                                          style = "border: 1px solid silver:", cellWidths = c(700, 700),
                                          
                                          plotOutput("education"),
                                          
                                          #tableOutput("results5"),
                                          
                                          plotOutput("education2")
                                          
                                          #tableOutput("results6")
                                          
                                        )
                                        
                                      )
                                      
                                    ))),
               
               tabPanel("Explanation",
                        
                        h3("Why include education?"),
                        
                        p("Attending school gives us the ability to broaden our horizons, become more educated on various issues, and learn how to play our part as an American citizen
                          and a citizen of the world. If students never get the opportunity to learn how to be an American citizen, then it is much less likely that they will vote.
                          This is why I decided to examine the graduation rate of school districts across the country to determine where we need to focus our efforts. I think that it will
                          be incredibly useful to afford people the opportunity to compare different school districts across the country."
                          
                        ))
    )),

    tabPanel("Health",
             
             tabsetPanel(
               
               tabPanel("Health Data",
                        
                        fluidPage(
                          
                          sidebarLayout(
                            
                            helpText("Examine Health Data."),
                            
                            fluidRow(selectizeInput("county_state", "Choose a county:", choices = health$county_state, options = list("actions-box" = TRUE), multiple = TRUE),
                                     selectInput("health_select", "Choose a variable to measure:", choices = c(`18+ Mentally Unhealthy Days per Month Percent` = "health_18_munhealthy",
                                                                                                               `18+ Physically Unhealthy Days per Month Percent` = "health_18_punhealthy",
                                                                                                               `Child Mortality Rate` = "health_child_mortality",
                                                                                                               `Infant Mortality Rate` = "health_infant_mortality",
                                                                                                               `Health Costs Adjusted for Medicare Payments` = "health_costs_adjusted_medicare",
                                                                                                               `Number of Dentists` = "health_dentists",
                                                                                                               `Food Environmental Index` = "health_food_environmental_index",
                                                                                                               `18+ Drinking Rate Percent` = "health_pct_drinking",
                                                                                                               `18+ Smoking Rate Percent` = "health_pct_smokers",
                                                                                                               `Percent Without Insurance 18-64` = "health_percent_woinsurance_18_64",
                                                                                                               `Percent Without Insurance Under 19` = "health_percent_woinsurance_under19",
                                                                                                               `Primary Physicians` = "health_primary_phys",
                                                                                                               `Percent of 18+ Population reporting either fair or poor health` = "health_18_pctfairpoor")
                                                 ))),
                          sidebarLayout(
                            
                            helpText("Population per County (For Reference)."),
                            
                            fluidRow(selectizeInput("select_county_state", "Choose a County:", choices = regression$county_state, options = list("actions-box" = TRUE), multiple = TRUE))
                          
                            ),
                          
                          mainPanel(
                              
                              fluidRow(
                                
                                splitLayout(
                                  
                                  style = "border: 1px solid silver:", cellWidths = c(800, 600),
                                  
                                  plotOutput("health"),
                                  
                                  tableOutput("population_stats")
                              ))
                            )
                          
                          
                        )),
               
               tabPanel("Explanation",
                        
                        h3("Why include health?"),
                        
                        p("Health is another factor of life that helps determine if someone will be able to vote come election time.
                          Feeling secure in your health insurance provider is essential, and every American should have access to 
                          health insurance. This set of data allows you to (once again) take a look at any county in the United States
                          across multiple different avenues of health. It gives people the opportunity to compare counties and again see
                          where we need to focus our efforts."
                          
                          
                        ))
               
             )),

    tabPanel("Regression",
             
             tabsetPanel(
               
               tabPanel("Regression Visualizations - Income on Voting",
                        
                        sliderInput("population", "Population Range:",
                                    min = 117, max = 10105708,
                                    value = c(1000,10000)),
                        
                        mainPanel(
                          
                          fluidRow(
                            
                            splitLayout(
                              
                              style = "border: 1px solid silver:", cellWidths = c(600, 600, 600),
                              
                              plotOutput("regression"),
                              
                              plotOutput("regression_log"),
                              
                              tableOutput("reg_table")
                              
                            )
                          )
                        )),
               
               tabPanel("Explanation",
                        
                        h3("Explanation for the first graph"),
                        
                        p("For each of these two graphs, I wanted to see how the regression would change depending on how big the population
                          was, since county populations vary extremely widely. The first graph shows the relationship between total personal income
                          for 2016 plotted against the total number of votes cast in the 2016 election. I colored each of the points by the percentage
                          of the people that voted in each county. The data shown in this graph is extremely clustered to one side, so I decided to log
                          both the independent and the dependent variable to make the graph easier to comprehend."),
                        
                        h3("Explanation for the second graph"),
                        
                        p("The second graph is the first graph, but the independent and dependent variables are both logged. Now that the graph is much
                          easier to read, we can look at the regression line. When we move the sliders, we can see that for the vast majority of the population
                          sizes, the regression line has a positive slope. What this indicates is that as a person's individual income increases, the total
                          number of votes cast increases as well, something that is demonstrated in the regression table as well. This is essentially what
                          we have seen earlier on in the project, where a higher income level is generally equated to casting more votes. However, when you look
                          at just counties with lower populations, the regression line can actually become negative. However, this is only when you look at a small
                          subset of counties, and it doesn't discount the finding that greater income is equated with a greater number of votes."),
                        
                        h3("Explanation for the regression table"),
                        
                        p("Like the graphs, the values of this regression table change when you move the slider to select the population. As shown in the graphs,
                          the effect of personal income on voting almost always has a positive slope. For the sake of our collective sanity, I will explain the 
                          outcome of the regression table when we look at the population of the United States as a whole. For this regression table, obviously we can't
                          have negative votes in an election. However, moving from that point onwards we can say that for every one unit increase in personal income,
                          the total votes will increase by 17.37 votes. Every time the percent vote in the US increases by one unit, the total votes will increase
                          by 792,491.30 votes. Interestingly, when we look at the interplay between personal income for 2016 and the percentage vote for 2016, we can see
                          that there is a negative relationship between the two, meaning that as the personal income of an area increases, the percentage of the population
                          that votes actually decreases. This is not what I was expecting, and it is a very interesting outcome considering the estimate was positive for
                          both personal income and percent votes on their own."),
                        
                        h3("Is there anything else we should know?"),
                        
                        p("Unfortunately, I was unable to combine my dataset on health with my dataset containing the votes for presidential elections and income statistics
                          for each county, so I was unable to run a regression on that data. It is something I hope to fix in the future, since I believe running a
                          regression on that information could yield some very interesting and insightful results.")
                        
               ))),
    
    tabPanel("About",
             
             titlePanel("About"),
             
             h3("Project Background and Motivations"),
             
             p("I think that, now more than ever, it is important to understand how and why voting patterns occur. 
               While none of this is new information, and it may be intuitive for some, it is still important to 
               lay it out in an easily digestible form so people of every background can understand the severity 
               of the situation we face in this country."),
             
             p("Most of the models I used for my project were interactive bar graphs. I knew that when I made this
               project I really wanted to let people compare different areas of the United States against one another,
               which I think is incredibly valuable since you can look at different kinds of data not just on a
               per-state basis but on an even more specific level. I think that added specificity helps narrow the focus
               so we can truly see what areas need help and what areas are doing ok."),
             
             p("Since I was not able to compare all the data I found with voting statistics, the purpose of this project
               has another function: to examine areas of the United Sates that are suffering in terms of voter participation,
               income, education, and health. I think that while the central idea of this project is to see how these areas
               possibly impact voting, I grew to thinking that this project is a good method for exploring and comparing 
               different counties to see where we can improve in all of these areas and where we need to focus our attention
               in the future."),
             
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
                
                y = "Number of Votes for Each Candidate",
                
                fill = "County Color"
            )
        
    })
    
    output$lpd <- renderPlot({
        
      pres_res %>%
            
            filter(abb_county %in% input$county) %>%
            
            ggplot(aes(year, totalvotes, color = abb_county)) +
            
            geom_line(stat = "identity", position = position_dodge()) + 
            
            theme_bw() + labs(
                
                title = "Change in Total Votes by County, 2000-2016",
                
                x = "County",
                
                y = "Total Votes"
                
            ) + theme(legend.position = "none")
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
                
                y = "Number Reported Voted",
                
                fill = "Income Bracket Color"
            )
        
    })
    
    output$income2 <- renderPlot({
      
      incomevars <- get(input$in_y_var)
        
        incomevars %>%
          
          filter(abb_county %in% input$in_county) %>%
          
          ggplot(aes(abb_county, y, fill = abb_county)) +
          
          geom_bar(stat = "identity", position = position_dodge()) +
          
          theme_bw() + labs(
            
            title = "Income Statistics by County",
            
            x = "County",
            
            y = "Personal Income/Ranking",
            
            fill = "County Color"
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
          
          y = "Percent",
          
          fill = "School District Color"
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
          
        ) + theme(legend.position = "none")
          
    })
    
    output$health <- renderPlot({
      
      health_select <- get(input$health_select)
      
      health_select %>%
        
        filter(county_state %in% input$county_state) %>%
        
        ggplot(aes(county_state, y, fill = county_state)) + geom_bar(stat = "identity", position = position_dodge()) +
        
        labs(
          
          title = "Health Data for Counties Across the United States",
          
          x = "County",
          
          y = "Health Variable",
          
          fill = "County Color"
          
        )
      
    })
    
    output$population_stats <- renderTable({
      
      regression %>%
        
        filter(county_state %in% input$select_county_state) %>%
        
        select(c(county_state, popestimate2016)) %>%
        
        dplyr::rename("County" = county_state,
               
               "Population Estimate 2016" = popestimate2016)
      
    })
    
    output$regression_log <- renderPlot({
      
      regression_log %>%
        
        filter(popestimate2016 >= input$population[1],
               
               popestimate2016 <= input$population[2]) %>%
        
        ggplot(aes(log_personal_income_2016, log_totalvotes, color = percent_votes)) + geom_point() +
        
        geom_smooth(method = "lm") + labs(
          title = "How Personal Income Affects the Total Number of Votes Cast in the 2016 Presidential Election",
          subtitle = "Both IV and DV Are Logged",
          x = "Personal Income 2016",
          y = "Total Votes 2016",
          color = "Percent of Population that Voted"
        )
      
    })
    
    output$regression <- renderPlot({
      
      regression %>%
        
        filter(popestimate2016 >= input$population[1],
               
               popestimate2016 <= input$population[2]) %>%
        
        ggplot(aes(personal_income_2016, totalvotes, color = percent_votes)) + geom_point() +
        
        geom_smooth(method = "lm") + labs(
          title = "How Personal Income Affects the Total Number of Votes Cast in the 2016 Presidential Election",
          x = "Personal Income 2016",
          y = "Total Votes 2016",
          color = "Percent of Population that Voted"
        )
      
    })
    
    output$reg_table <- renderTable({
      
      regression %>%
        
        filter(popestimate2016 >= input$population[1],
               
               popestimate2016 <= input$population[2]) %>%
        
        lm(formula = totalvotes ~ personal_income_2016 * percent_votes) %>%
        
        tidy(conf.int = TRUE) %>%
        
        select(term, estimate, conf.low, conf.high) %>%
        
        mutate(estimate = round(estimate, digits = 2),
               
               conf.low = round(conf.low, digits = 2),
               
               conf.high = round(conf.high, digits = 2))
      
    })
    
# I wanted to insert an image because I haven't made a video yet!
  
    output$image <- renderImage({
        
        filename <- normalizePath(file.path('.', paste('image', '.jpg', sep='')))
        
        list(src = filename)
        
    }, deleteFile = FALSE)

    
  
}


shinyApp(ui = ui, server = server)


