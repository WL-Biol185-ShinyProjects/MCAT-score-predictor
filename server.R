library(shiny)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(shinythemes)
library(readxl)
source("score predictor.R")

MCAT_clean_data <- read_excel("~/MCAT-score-predictor/MCAT clean data.xlsx")


#CP distribution plot
function(input, output) {
  
boxplotCreator <- function(examType)
{
  examCP <- paste0(examType, ".CP")
  examCARS <- paste0(examType, ".CARS")
  examBB <- paste0(examType, ".BB")
  examPS <- paste0(examType, ".PS")
  sliderCP <- paste0(examType, "cp")
  sliderCARS <- paste0(examType, "cars")
  sliderBB <- paste0(examType, "bb")
  sliderPS <- paste0(examType, "ps")

  #CP.CARS.Table <-bind_rows(
    CPtable <- transmute(MCAT_clean_data, Real.CP = Real.CP, examCP = MCAT_clean_data[[examCP]]) %>%
      na.omit(examCP) %>%
      filter(CPtable[examCP] == input[[sliderCP]]) #%>%
      #gather("subsection", "score", 1)
     # if (nrow(CPtable) == 0) {
     #   CPtable <- add_row(CPtable, "subsection" = "Real.CP", "score" = input[[sliderCP]])}

  #    CARStable <- transmute(MCAT_clean_data, Real.CARS = Real.CARS, examCARS = MCAT_clean_data[[examCARS]]) %>%
  #     na.omit(examCP) %>%
  #      filter(examCARS == input[[sliderCARS]]) %>%
  #      gather("subsection", "score", 1),
  #    if (nrow(CARStable) == 0) {
  #      CARStable <- add_row(CARStable, "subsection" = "Real.CARS", "score" = input[[sliderCP]])}
  # )

  #  BB.PS.Table <- bind_rows(
  #    BBtable<- transmute(MCAT_clean_data, Real.BB = Real.BB, examBB = MCAT_clean_data[[examBB]]) %>%
  #      na.omit(examCP) %>%
  #      filter(examBB == input[[sliderBB]]) %>%
  #     gather("subsection", "score", 1),
  #   if (nrow(BBtable) == 0) {
  #     BBtable <- add_row(BBtable, "subsection" = "Real.BB", "score" = input[[sliderBB]])},
  # 
  #   PStable <- transmute(MCAT_clean_data, Real.PS = Real.PS, examPS = MCAT_clean_data[[examPS]]) %>%
  #     na.omit(examCP) %>%
  #     filter(examPS == input[[sliderPS]]) %>%
  #     gather("subsection", "score", 1),
  #   if (nrow(PStable) == 0) {
  #     PStable <- add_row(PStable, "subsection" = "Real.PS", "score" = input[[sliderPS]])}
  # )
  # 
  # 
  #   MainTable <- bind_rows(CP.CARS.Table, BB.PS.Table)

    # cleanup <- theme(panel.grid.major = element_blank(),
    #                  panel.grid.minor = element_blank(),
    #                  panel.background = element_blank(),
    #                  axis.line = element_line(color = "black"))
    # 
    # MainTable$Section <- factor(MainTable$subsection,  levels = c("Real.CP", "Real.CARS", "Real.BB", "Real.PS"))
    # 
    # median2 <- aggregate(score ~ Section, MainTable, median)
    # 
    # ggplot(MainTable, aes(Section, score, fill = Section)) +
    #   geom_boxplot() +
    #   ylim(118, 132) +
    #   xlab("Subsection") +
    #   ylab("Scaled Score") +
    #   cleanup +
    #   scale_x_discrete(labels = c("CP", "CARS", "BB", "PS")) +
    # 
    #   geom_text(data = median2, aes(label = score, y = score - 0.3)) +
    #   guides(fill = FALSE)
  }
  
  practiceScorePredictor <- function(examType, medianValues = FALSE)
    
  {
    errorMessage <<- FALSE
    CPtest <<- paste0(examType, ".CP")
    useSliderCp <- paste0(examType, "cp")
    filterTablecp <<- MCAT_clean_data %>%
      filter(MCAT_clean_data[CPtest] == input[[useSliderCp]])
    if (nrow(filterTablecp) == 0) {
      errorMessage <<- TRUE
      filterTablecp <- add_row(filterTablecp, Real.CP = input[[useSliderCp]])
      
    }
    
    carstest <- paste0(examType, ".CARS")
    useSliderCars <- paste0(examType, "cars")
    filterTablecars <- MCAT_clean_data %>%
      filter(MCAT_clean_data[carstest] == input[[useSliderCars]])
    if (nrow(filterTablecars) == 0) {
      errorMessage <<- TRUE
      filterTablecars <- add_row(filterTablecars, Real.CARS = input[[useSliderCars]])
    }
    
    bbtest <- paste0(examType, ".BB")
    useSliderBb <- paste0(examType, "bb")
    filterTablebb <- MCAT_clean_data %>%
      filter(MCAT_clean_data[bbtest] == input[[useSliderBb]])
    if (nrow(filterTablebb) == 0) {
      errorMessage <<- TRUE
      filterTablebb <- add_row(filterTablebb, Real.BB = input[[useSliderBb]])
    }
    
    pstest <- paste0(examType, ".PS")
    useSliderPs <- paste0(examType, "ps")
    filterTableps <- MCAT_clean_data %>%
      filter(MCAT_clean_data[pstest] == input[[useSliderPs]])
    if (nrow(filterTableps) == 0) {
      errorMessage <<- TRUE
      filterTableps <- add_row(filterTableps, Real.PS = input[[useSliderPs]])
    }
    
    medianCP <<- median(filterTablecp$Real.CP)
    medianCARS <<- median(filterTablecars$Real.CARS)
    medianBB <<- median(filterTablebb$Real.BB)
    medianPS <<- median(filterTableps$Real.PS)
    median(filterTablecp$Real.CP) + median(filterTablecars$Real.CARS) + median(filterTablebb$Real.BB) + median(filterTableps$Real.PS)
    
  }
  
  inputSlider <- function(examType, subsection, subName){
    sliderName <- paste0(examType, subsection)
    sliderInput(sliderName,
                subName,
                min = 118,
                max = 132,
                value = 125)
     
  }
  
  output$BoxPlot1 <- renderPlot({
    
  
      CP.CARS.Table <-bind_rows(
        CPtable <- transmute(MCAT_clean_data, Real.CP = Real.CP, FL1.CP = FL1.CP) %>%
        filter(FL1.CP == input$FL1cp) %>%
          gather("subsection", "score", 1),
         if (nrow(CPtable) == 0) {
           CPtable <- add_row(CPtable, "subsection" = "Real.CP", "score" = input$FL1cp)},
    
        CARStable <- transmute(MCAT_clean_data, Real.CARS = Real.CARS, FL1.CARS = FL1.CARS) %>%
        filter(FL1.CARS == input$FL1cars) %>%
          gather("subsection", "score", 1),
        if (nrow(CARStable) == 0) {
          CARStable <- add_row(CARStable, "subsection" = "Real.CARS", "score" = input$FL1cars)}
        
        )
      
      BB.PS.Table <- bind_rows(
        BBtable<- transmute(MCAT_clean_data, Real.BB = Real.BB, FL1.BB = FL1.BB) %>%
        filter(FL1.BB == input$FL1bb) %>%
        gather("subsection", "score", 1),
        if (nrow(BBtable) == 0) {
          BBtable <- add_row(BBtable, "subsection" = "Real.BB", "score" = input$FL1bb)},
        
        PStable <- transmute(MCAT_clean_data, Real.PS = Real.PS, FL1.PS = FL1.PS) %>%
        filter(FL1.PS == input$FL1ps) %>%
        gather("subsection", "score", 1),
        if (nrow(PStable) == 0) {
          PStable <- add_row(PStable, "subsection" = "Real.PS", "score" = input$FL1ps)}
        )
      
        
 MainTable <- bind_rows(CP.CARS.Table, BB.PS.Table)
 
 MainTable$Section <- factor(MainTable$subsection)
 
 
 median1 <<- aggregate(score ~ Section, MainTable, median)
 
 
 cleanup <- theme(panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(),
                  axis.line = element_line(color = "black"))

 
 MainTable$Section <- factor(MainTable$subsection, levels = c("Real.CP", "Real.CARS", "Real.BB", "Real.PS"))
 ggplot(MainTable, aes(Section, score, fill = Section)) + 
   geom_boxplot() + 
   ylim(118, 132) +
   xlab("Subsection") + 
   ylab("Scaled Score") + 
   cleanup +
   scale_x_discrete(labels = c("CP", "CARS", "BB", "PS")) +

   geom_text(data = median1, aes(label = score, y = score - 0.3)) +
   guides(fill = FALSE)

 


  })
  
  output$BoxPlot2 <- renderPlot({
    
    
    CP.CARS.Table <-bind_rows(
      CPtable <- transmute(MCAT_clean_data, Real.CP = Real.CP, FL2.CP = FL2.CP) %>%
        filter(FL2.CP == input$FL2cp) %>%
        gather("subsection", "score", 1),
      if (nrow(CPtable) == 0) {
        CPtable <- add_row(CPtable, "subsection" = "Real.CP", "score" = input$FL2cp)},
      
      CARStable <- transmute(MCAT_clean_data, Real.CARS = Real.CARS, FL2.CARS = FL2.CARS) %>%
        filter(FL2.CARS == input$FL2cars) %>%
        gather("subsection", "score", 1),
      if (nrow(CARStable) == 0) {
        CARStable <- add_row(CARStable, "subsection" = "Real.CARS", "score" = input$FL2cars)}
    )
    
    BB.PS.Table <- bind_rows(
      BBtable<- transmute(MCAT_clean_data, Real.BB = Real.BB, FL2.BB = FL2.BB) %>%
        filter(FL2.BB == input$FL2bb) %>%
        gather("subsection", "score", 1),
      if (nrow(BBtable) == 0) {
        BBtable <- add_row(BBtable, "subsection" = "Real.BB", "score" = input$FL2bb)},
      
      PStable <- transmute(MCAT_clean_data, Real.PS = Real.PS, FL2.PS = FL2.PS) %>%
        filter(FL2.PS == input$FL2ps) %>%
        gather("subsection", "score", 1),
      if (nrow(PStable) == 0) {
        PStable <- add_row(PStable, "subsection" = "Real.PS", "score" = input$FL2ps)}
    )
    
    
    MainTable <- bind_rows(CP.CARS.Table, BB.PS.Table)
    
    cleanup <- theme(panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(),
                     panel.background = element_blank(),
                     axis.line = element_line(color = "black"))

    MainTable$Section <- factor(MainTable$subsection,  levels = c("Real.CP", "Real.CARS", "Real.BB", "Real.PS"))
    
    median2 <- aggregate(score ~ Section, MainTable, median)
    
    ggplot(MainTable, aes(Section, score, fill = Section)) + 
      geom_boxplot() + 
      ylim(118, 132) +
      xlab("Subsection") + 
      ylab("Scaled Score") + 
      cleanup +
      scale_x_discrete(labels = c("CP", "CARS", "BB", "PS")) +

      geom_text(data = median2, aes(label = score, y = score - 0.3)) +
      guides(fill = FALSE) 


                       
      
    
    
    

  })
   
    

    output$tsText <- renderText({
        practiceScorePredictor("FL1")

    })   
    output$tsTextFL2 <- renderText({
      practiceScorePredictor("FL2")
      
    })
    
    output$Error <- renderText({
      if (CPtest == "FL1.CP"){
        practiceScorePredictor("FL1")
      }
      else if (CPtest == "FL2.CP") {
        practiceScorePredictor("FL2")
      }
       if (errorMessage == TRUE){
       "One or more of your subsection scores did not have any associated data. Our algorithim just returns your input score during its calculation"}
    })
    
    output$test1UI <- renderUI({
      
      
      if (input$`Practice Test` == "AAMC Full Length Test #1"){
          fluidRow(
            column(3,
             inputSlider("FL1", "cp", "Chem and Phys Score"),
             inputSlider("FL1", "cars", "CARS Score"),
             inputSlider("FL1", "bb", "Biology Score"),
             inputSlider("FL1", "ps", "Psych and Sociology Score")
           ),
           
          column(6,
            #boxplotCreator("FL1"),
            plotOutput("BoxPlot1"),
            strong("Predicted Score based on AAMC Full Length #1:"),
            textOutput("tsText")
            ),
          column(3,
                 
            tableOutput("table")
            #textOutput("Error")
            ))
        

       }
      else if (input$`Practice Test` == "AAMC Full Length Test #2"){
          fluidRow(
            column(3,
                   inputSlider("FL2", "cp", "Chem and Phys Score"),
                   inputSlider("FL2", "cars", "CARS Score"),
                   inputSlider("FL2", "bb", "Biology Score"),
                   inputSlider("FL2", "ps", "Psych and Sociology Score")
            ),
            
            column(6,
                   plotOutput("BoxPlot2"),
                   strong("Predicted Score based on AAMC Full Length #2:"),
                   textOutput("tsTextFL2")),
            column(3,
                   
                   tableOutput("table2")
                   #textOutput("Error")
        ))
        
      }
      
    })
    
    
    output$secondDrop <- renderUI({
      if (input$'Practice Test' == "None"){}
      
      else if (input$'Practice Test' == "AAMC Full Length Test #1"){

        selectInput("PT2", h2("Please Select a Practice Test:"), c("None", "AAMC Full Length Test #2"))
      }
      
      else if (input$`Practice Test` == "AAMC Full Length Test #2"){
        selectInput("PT2", h2("Please Select a Practice Test:"), c("None", "AAMC Full Length Test #1"))
      }
   
 
    })
    
    output$test2UI <- renderUI({
      if (is.null(input$'PT2') == TRUE){
      }
        
        
      else if (input$'PT2' == "AAMC Full Length Test #2"){
        fluidRow(
          column(3,
                 inputSlider("FL2", "cp", "Chem and Phys Score"),
                 inputSlider("FL2", "cars", "CARS Score"),
                 inputSlider("FL2", "bb", "Biology Score"),
                 inputSlider("FL2", "ps", "Psych and Sociology Score")
          ),
          
          column(6,
                 plotOutput("BoxPlot2"),
                 strong("Predicted Score based on AAMC Full Length #2:"),
                 textOutput("tsTextFL2")
                 ),
          column(3,
                 
                 tableOutput("table2")
          ))
        
      }
        else if (input$'PT2' == "AAMC Full Length Test #1"){
          fluidRow(
            column(3,
                   inputSlider("FL1", "cp", "Chem and Phys Score"),
                   inputSlider("FL1", "cars", "CARS Score"),
                   inputSlider("FL1", "bb", "Biology Score"),
                   inputSlider("FL1", "ps", "Psych and Sociology Score")
            ),
            
            column(6,
                   plotOutput("BoxPlot1"),
                   strong("Predicted Score based on AAMC Full Length #1:"),
                   textOutput("tsText")),
            column(3,
                   
                   tableOutput("table")))
        }
    })
    
     output$table <- renderTable({
       medianTable <- tibble("Section" = c("CP", "CARS", "BB", "PS", "Total"), "Score" = c(medianCP, medianCARS, medianBB, medianPS, practiceScorePredictor("FL1")))

    },
    striped = TRUE, bordered = TRUE, digits = 1)
    
     output$table2 <- renderTable({
       medianTable <- tibble("Section" = c("CP", "CARS", "BB", "PS", "Total"), "Score" = c(medianCP, medianCARS, medianBB, medianPS, practiceScorePredictor("FL2")))
       
     },
     striped = TRUE, bordered = TRUE, digits = 1)
    
}