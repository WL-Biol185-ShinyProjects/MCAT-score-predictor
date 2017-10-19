library(shiny)
library(ggplot2)
library(tidyverse)

#function(input, output) {
  
  #output$distPlot <- renderPlot({
    
 #   x <- faithful[,2]
 #   bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
  #  hist(x, breaks = bins, col = 'darkgray', border = 'white')
#  })
#}"""
#CP distribution plot
function(input, output) {
  
  output$cpPlot <- renderPlot({
    
    
    #filtering input score, plotting dist of real score
    MCAT_clean_data %>%
      filter(FL1.CP == input$cpScore) %>%
      ggplot(aes(Real.CP)) + geom_density()
    
  })
    output$carsPlot <- renderPlot({
      #filtering input score, plotting dist of real score
      MCAT_clean_data %>%
        filter(FL1.CARS == input$carsScore) %>%
        ggplot(aes(Real.CARS)) + geom_density()

    })
    
    output$bbPlot <- renderPlot({
      MCAT_clean_data %>%
        filter(FL1.BB == input$bbScore) %>%
        ggplot(aes(Real.BB)) + geom_density()
    })
    
    output$psPlot <- renderPlot({
      MCAT_clean_data %>%
        filter(FL1.PS == input$psScore) %>%
        ggplot(aes(Real.PS)) + geom_density()
    })
    
    output$tsText <- renderText({
      filtertable <- MCAT_clean_data %>%
        filter(FL1.CP == input$cpScore) %>%
        filter(FL1.CARS == input$carsScore)
      median(filtertable$Real.CP) + median(filtertable$Real.CARS)
        
    })
    
}