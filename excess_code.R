server <- function(input, output) {
  output$voting_data <- renderPlot({
    w <- pres_res$year
    x <- seq(1, nrow(pres_res[pres_res$county == input$county, ]), 1)
    y <- pres_res[pres_res$county == input$county, input$y_var]
    dat <- as.data.frame(cbind(w, x, y))
    dat %>%
      filter(w == 2000) %>%
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

output$education <- renderPlot({
  
  education %>% 
    
    filter(qualifying_name %in% input$state) %>%
    
    ggplot(aes(qualifying_name, get(input$`y-var`))) +
    
    geom_histogram(stat = "identity", position = position_dodge())
  
})