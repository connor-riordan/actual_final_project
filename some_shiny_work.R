
health <- readRDS("health.RDS")

options(scipen = 999)


fluidPage(
             sidebarLayout(
               sidebarPanel(
                 helpText("Examine Health Data."),
                 
                 fluidRow(selectInput("county", "Choose a county:", choices = health$name_of_area, multiple = TRUE),
                          selectInput("y_var", "Choose what you want to measure:", choices = c(colnames(health)[-c(1:2)]))
                 )),
               
               mainPanel(
                 plotOutput("health_data"),
                 br(), br(),
                 tableOutput("results")))
           )


server <- function(input, output) {
  output$voting_data <- renderPlot({
    x <- seq(1, nrow(health[health$name_of_area == input$county, ]), 1)
    y <- health[health$name_of_area == input$county, input$y_var]
    dat <- as.data.frame(cbind(x, y))
    dat %>%
      ggplot(aes(x, y)) + 
      scale_x_discrete(name = 'county',
                       breaks = x,
                       labels = paste(x),
                       limits = x) + 
      geom_bar(stat = "identity") + theme_classic() + labs(
        title = "Health Data"
      )
  })
}


shinyApp(ui = ui, server = server)




education <- readRDS("education.RDS")

options(scipen = 999)


fluidPage(
  sidebarLayout(
    sidebarPanel(
      helpText("Examine Education Data."),
      
      fluidRow(selectInput("state", "Choose a state:", choices = education$name_of_area, multiple = TRUE),
               selectInput("y_var", "Choose what you want to measure:", choices = c(colnames(education)[-c(1:2)]))
      )),
    
    mainPanel(
      plotOutput("education_data"),
      br(), br(),
      tableOutput("results")))
)


server <- function(input, output) {
  output$voting_data <- renderPlot({
    x <- seq(1, nrow(education[education$name_of_area == input$state, ]), 1)
    y <- education[education$name_of_area == input$state, input$y_var]
    dat <- as.data.frame(cbind(x, y))
    dat %>%
      ggplot(aes(x, y)) + 
      scale_x_discrete(name = 'state',
                       breaks = x,
                       labels = paste(x),
                       limits = x) + 
      geom_bar(stat = "identity") + theme_classic() + labs(
        title = "Education Data"
      )
  })
}


shinyApp(ui = ui, server = server)





age_inc_16 <- readRDS("age_income_2016.RDS")

options(scipen = 999)


fluidPage(
  sidebarLayout(
    sidebarPanel(
      helpText("Examine Health Data."),
      
      fluidRow(selectInput("income", "Choose an income level:", choices = age_inc_16$total_income, multiple = TRUE),
               selectInput("y_var", "Choose what you want to measure:", choices = c(colnames(age_inc_16)[-c(1:2)]))
      )),
    
    mainPanel(
      plotOutput("age_income_data"),
      br(), br(),
      tableOutput("results")))
)


server <- function(input, output) {
  output$voting_data <- renderPlot({
    x <- seq(1, nrow(age_inc_16[age_inc_16$total_income == input$income, ]), 1)
    y <- age_inc_16[age_inc_16$total_income == input$income, input$y_var]
    dat <- as.data.frame(cbind(x, y))
    dat %>%
      ggplot(aes(x, y)) + 
      scale_x_discrete(name = 'income',
                       breaks = x,
                       labels = paste(x),
                       limits = x) + 
      geom_bar(stat = "identity") + theme_classic() + labs(
        title = "Income Data"
      )
  })
}


shinyApp(ui = ui, server = server)

