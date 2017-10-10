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

function(input, output) {
  
  output$densityPlot <- renderPlot({
    
    
    #filtering input score, plotting dist of real score
    MCAT_clean_data %>%
      filter(FL1.CP == input$cpScore) %>%
      ggplot(aes(Real.CP)) + geom_density()

  })
}