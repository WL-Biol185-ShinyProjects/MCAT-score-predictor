library(shiny)
library(ggplot2)
library(tidyverse)
library(dplyr)
#source("score predictor.R")


#CP distribution plot
function(input, output) {
  
  output$cpPlot <- renderPlot({
    
    
    #filtering input score, plotting dist of real score
    #MCAT_clean_data %>%
     # filter(FL1.CP == input$cpScore) %>%
      #gather("subsection", "score", 2:5) %>%
      #ggplot(aes(subsection, score)) + geom_boxplot()
   
  
      bind_rows(
        CPtable <- transmute(MCAT_clean_data, Real.CP = Real.CP, FL1.CP = FL1.CP) %>%
        filter(FL1.CP == input$cpScore) %>%
        gather("subsection", "score", 1))
        
        CPtable$subsectionreal <- factor(CPtable$subsection)
       
        
    
      #ggplot(aes(CPtable$subsectionreal, CPtable$score)) + geom_boxplot()
      ggplot(CPtable, aes(subsectionreal, score)) + geom_boxplot()
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
      filterTablecp <- MCAT_clean_data %>%
        filter(FL1.CP == input$cpScore)
      filterTableCARS <- MCAT_clean_data %>%
        filter(FL1.CARS == input$carsScore)
      filterTablebb <- MCAT_clean_data %>%
        filter(FL1.BB == input$bbScore)
      filterTableps <- MCAT_clean_data %>%
        filter(FL1.PS == input$psScore)
      median(filterTablecp$Real.CP) + median(filterTableCARS$Real.CARS) + median(filterTablebb$Real.BB) + median(filterTableps$Real.PS)
    })   
    
}