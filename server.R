library(shiny)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(shinythemes)
library(readxl)
source("score predictor.R")

MCAT_clean_data <- read_csv("MCAT clean data.csv")


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
  
  practiceScorePredictor <- function(examType, sampleTest = FALSE)
    
  {
    errorMessage <<- FALSE
    CPtest <<- paste0(examType, ".CP")
    useSliderCp <- paste0(examType, "cp")
    filterTablecp <<- MCAT_clean_data %>%
      filter(MCAT_clean_data[CPtest] == input[[useSliderCp]])
    if (nrow(filterTablecp) == 0 & sampleTest == TRUE) {
      filterTablecp <- add_row(filterTablecp, Real.CP = 125)
    }
    else if (nrow(filterTablecp) == 0) {
      filterTablecp <- add_row(filterTablecp, Real.CP = input[[useSliderCp]])
    }
    
    carstest <- paste0(examType, ".CARS")
    useSliderCars <- paste0(examType, "cars")
    filterTablecars <<- MCAT_clean_data %>%
      filter(MCAT_clean_data[carstest] == input[[useSliderCars]])
    if (nrow(filterTablecars) == 0 & sampleTest == TRUE) {
      filterTablecars <- add_row(filterTablecars, Real.CARS = 125)
    }
    else if (nrow(filterTablecars) == 0) {
      filterTablecars <- add_row(filterTablecars, Real.CARS = input[[useSliderCars]])
    }
    
    bbtest <- paste0(examType, ".BB")
    useSliderBb <- paste0(examType, "bb")
    filterTablebb <<- MCAT_clean_data %>%
      filter(MCAT_clean_data[bbtest] == input[[useSliderBb]])
    if (nrow(filterTablebb) == 0 & sampleTest == TRUE) {
      filterTablebb <- add_row(filterTablebb, Real.BB = 125)
    }
    else if (nrow(filterTablebb) == 0) {
      filterTablebb <- add_row(filterTablebb, Real.BB = input[[useSliderBb]])
    }
    
    pstest <- paste0(examType, ".PS")
    useSliderPs <- paste0(examType, "ps")
    filterTableps <<- MCAT_clean_data %>%
      filter(MCAT_clean_data[pstest] == input[[useSliderPs]])
    if (nrow(filterTableps) == 0 & sampleTest == TRUE) {
      filterTableps <- add_row(filterTableps, Real.PS = 125)
    }
    else if (nrow(filterTableps) == 0) {
      filterTableps <- add_row(filterTableps, Real.PS = input[[useSliderPs]])
    }
    
    medianCP <<- median(filterTablecp$Real.CP)
    medianCARS <<- median(filterTablecars$Real.CARS)
    medianBB <<- median(filterTablebb$Real.BB)
    medianPS <<- median(filterTableps$Real.PS)
    numberData <<- nrow(filterTablecp) + nrow(filterTablecars) + nrow(filterTablebb) + nrow(filterTableps)
    median(filterTablecp$Real.CP) + median(filterTablecars$Real.CARS) + median(filterTablebb$Real.BB) + median(filterTableps$Real.PS)
     
  }
  
  inputSlider <- function(examType, subsection, subName){
    sliderName <- paste0(examType, subsection)
    sliderInput(
                sliderName,
                subName,
                min = 118,
                max = 132,
                value = 125
                )
     
  }
  
  sliderSet <- function(examType, sample = FALSE) {
    if (sample == FALSE){
     fluidRow(
     inputSlider(examType, "cp", "Chemsitry and Physics Score"),
     inputSlider(examType, "cars", "Critical Analysis and Reading Score"),
     inputSlider(examType, "bb", "Biology and Biochemsitry Score"),
     inputSlider(examType, "ps", "Psychology and Sociology Score")
    )
    }
    else if (sample == TRUE){
      sliderCP <- paste0(examType, "cp")
      sliderCARS <- paste0(examType, "cars")
      sliderBB <- paste0(examType, "bb")
      sliderPS <- paste0(examType, "ps")
      fluidRow(
                  sliderInput(sliderCP, "Chemistry and Physics Score",
                              min = 0,
                              max = 100,
                              value = 75),
                  sliderInput(sliderCARS, "Critical Analysis and Reading",
                              min = 0,
                              max = 100,
                              value = 75),
                  sliderInput(sliderBB, "Biology and Biochemistry Score",
                              min = 0,
                              max = 100,
                              value = 75),
                  sliderInput(sliderPS, "Psychology and Sociology Score",
                              min = 0,
                              max = 100,
                              value = 75))
    }
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
   theme(text = element_text(size=15)) +
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
      theme(text = element_text(size=15)) +
      geom_text(data = median2, aes(label = score, y = score - 0.3)) +
      guides(fill = FALSE) 


                       
      
    
    
    

  })
  
  output$BoxPlot3 <- renderPlot({
    
    
    CP.CARS.Table <-bind_rows(
      CPtable <- transmute(MCAT_clean_data, Real.CP = Real.CP, ST.CP = ST.CP) %>%
        filter(ST.CP == input$STcp) %>%
        gather("subsection", "score", 1),
      if (nrow(CPtable) == 0) {
        CPtable <- add_row(CPtable, "subsection" = "Real.CP", "score" = 125)},
      
      CARStable <- transmute(MCAT_clean_data, Real.CARS = Real.CARS, ST.CARS = ST.CARS) %>%
        filter(ST.CARS == input$STcars) %>%
        gather("subsection", "score", 1),
      if (nrow(CARStable) == 0) {
        CARStable <- add_row(CARStable, "subsection" = "Real.CARS", "score" = 125)}
      
    )
    
    BB.PS.Table <- bind_rows(
      BBtable<- transmute(MCAT_clean_data, Real.BB = Real.BB, ST.BB = ST.BB) %>%
        filter(ST.BB == input$STbb) %>%
        gather("subsection", "score", 1),
      if (nrow(BBtable) == 0) {
        BBtable <- add_row(BBtable, "subsection" = "Real.BB", "score" = 125)},
      
      PStable <- transmute(MCAT_clean_data, Real.PS = Real.PS, ST.PS = ST.PS) %>%
        filter(ST.PS == input$STps) %>%
        gather("subsection", "score", 1),
      if (nrow(PStable) == 0) {
        PStable <- add_row(PStable, "subsection" = "Real.PS", "score" = 125)}
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
      theme(text = element_text(size=15)) +
      geom_text(data = median1, aes(label = score, y = score - 0.3)) +
      guides(fill = FALSE)
    
    
    
    
    
  })
   
    

    output$tsText <- renderText({
        practiceScorePredictor("FL1")

    })   
    output$tsTextFL2 <- renderText({
      practiceScorePredictor("FL2")
      
    })
    
    output$tsTextST <- renderText({
      practiceScorePredictor("ST", sampleTest = TRUE)
      
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
            column(2,
             sliderSet("FL1")
           ),
           
          column(6,
            #boxplotCreator("FL1"),
            plotOutput("BoxPlot1"),
            strong("Predicted Score based on AAMC Full Length #1:"),
            textOutput("tsText")
            ),
          column(4,
                 
            tableOutput("table")
            #textOutput("Error")
            ))
        

       }
      else if (input$`Practice Test` == "AAMC Full Length Test #2"){
          fluidRow(
            column(2,
                   sliderSet("FL2")
            ),
            
            column(6,
                   plotOutput("BoxPlot2"),
                   strong("Predicted Score based on AAMC Full Length #2:"),
                   textOutput("tsTextFL2")),
            column(4,
                   
                   tableOutput("table2")))

        
      }
      else if (input$`Practice Test` == "Sample Test"){
        fluidRow(
          column(2,
                 sliderSet("ST", sample = TRUE)
                 ),
          column(6,
                 plotOutput("BoxPlot3"),
                 textOutput("tsTextST")
                 ),
          column(4,
                 tableOutput("table3"))
          
        )
      }
      
    })
    
    
    output$secondDrop <- renderUI({
      if (input$'Practice Test' == "None"){}
      
      else if (input$'Practice Test' == "AAMC Full Length Test #1"){

        selectInput("PT2", h2("Please Select a Practice Test:"), c("None", "AAMC Full Length Test #2", "Sample Test"))
      }
      
      else if (input$`Practice Test` == "AAMC Full Length Test #2"){
        selectInput("PT2", h2("Please Select a Practice Test:"), c("None", "AAMC Full Length Test #1", "Sample Test"))
      }
      
      else if (input$`Practice Test` == "Sample Test"){
        selectInput("PT2", h2("Please Select a Practice Test:"), c("None", "AAMC Full Length Test #1", "AAMC Full Length Test #2"))
      }
   
 
    })
    
    output$test2UI <- renderUI({
      if (is.null(input$'PT2') == TRUE){
      }
        
        
      else if (input$'PT2' == "AAMC Full Length Test #2"){
        fluidRow(
          column(2,
                 sliderSet("FL2")
          ),
          
          column(6,
                 plotOutput("BoxPlot2"),
                 strong("Predicted Score based on AAMC Full Length #2:"),
                 textOutput("tsTextFL2")
                 ),
          column(4,
                 
                 tableOutput("table2")
          ))
        
      }
        else if (input$'PT2' == "AAMC Full Length Test #1"){
          fluidRow(
            column(2,
                   sliderSet("FL1")
            ),
            
            column(6,
                   plotOutput("BoxPlot1"),
                   strong("Predicted Score based on AAMC Full Length #1:"),
                   textOutput("tsText")),
            column(4,
                   
                   tableOutput("table")))
        }
      else if (input$'PT2' == "Sample Test"){
          fluidRow(
            column(2,
                   sliderSet("ST", sample = TRUE)
            ),
            column(6,
                   plotOutput("BoxPlot3"),
                   textOutput("tsTextST")
            ),
            column(4,
                   tableOutput("table3"))
          )
      }
    })

    
     output$table <- renderTable({
       medianTable <- tibble("Test Subsection" = c("Chemistry and Physics", "Critical Analysis and Reading", "Biology and Biochemistry", "Psychology and Sociology", "Total"), "Score" = c(medianCP, medianCARS, medianBB, medianPS, practiceScorePredictor("FL1")), "Number of Data Points" = c(nrow(filterTablecp), nrow(filterTablecars), nrow(filterTablebb), nrow(filterTableps), numberData))

    },
    striped = TRUE, bordered = TRUE, digits = 1)
    
     output$table2 <- renderTable({
       medianTable <- tibble("Test Subection" = c("Chemistry and Physics", "Critical Analysis and Reading ", "Biology and Biochemistry", "Psychology and Sociology", "Total"), "Score" = c(medianCP, medianCARS, medianBB, medianPS, practiceScorePredictor("FL2")), "Number of Data Points" = c(nrow(filterTablecp), nrow(filterTablecars), nrow(filterTablebb), nrow(filterTableps), numberData))
       
     },
     striped = TRUE, bordered = TRUE, digits = 1)
     
     output$table3 <- renderTable({
       medianTable <- tibble("Test Subection" = c("Chemistry and Physics", "Critical Analysis and Reading ", "Biology and Biochemistry", "Psychology and Sociology", "Total"), "Score" = c(medianCP, medianCARS, medianBB, medianPS, practiceScorePredictor("ST", sampleTest = TRUE)), "Number of Data Points" = c(nrow(filterTablecp), nrow(filterTablecars), nrow(filterTablebb), nrow(filterTableps), numberData))
       
     },
     striped = TRUE, bordered = TRUE, digits = 1)
    
}