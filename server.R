library(shiny)
library(ggplot2)
library(tidyverse)
library(dplyr)
#source("score predictor.R")


#CP distribution plot
function(input, output) {
  
  output$BoxPlot <- renderPlot({
    
  
      CP.CARS.Table <-bind_rows(
        CPtable <- transmute(MCAT_clean_data, Real.CP = Real.CP, FL1.CP = FL1.CP) %>%
        filter(FL1.CP == input$cpScore) %>%
        gather("subsection", "score", 1), 
    
        CARStable <- transmute(MCAT_clean_data, Real.CARS = Real.CARS, FL1.CARS = FL1.CARS) %>%
        filter(FL1.CARS == input$carsScore) %>%
        gather("subsection", "score", 1))
      
      BB.PS.Table <- bind_rows(
        BBtable<- transmute(MCAT_clean_data, Real.BB = Real.BB, FL1.BB = FL1.BB) %>%
        filter(FL1.BB == input$bbScore) %>%
        gather("subsection", "score", 1),
        
        PStable <- transmute(MCAT_clean_data, Real.PS = Real.PS, FL1.PS = FL1.PS) %>%
        filter(FL1.PS == input$psScore) %>%
        gather("subsection", "score", 1))
      
        
 MainTable <- bind_rows(CP.CARS.Table, BB.PS.Table)
 
 MainTable$Section <- factor(MainTable$subsection)
 ggplot(MainTable, aes(Section, score)) + geom_boxplot() + ylim(118, 132)
    
    
        
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