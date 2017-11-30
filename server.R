library(shiny)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(shinythemes)
source("score predictor.R")


#CP distribution plot
function(input, output) {
  
  practiceScorePredictor <- function(examType, medianValues = FALSE)
    
  {
    CPtest <- paste0(examType, ".CP")
    useSliderCp <- paste0(examType, "cp")
    filterTablecp <- MCAT_clean_data %>%
      filter(MCAT_clean_data[CPtest] == input[[useSliderCp]])
    
    carstest <- paste0(examType, ".CARS")
    useSliderCars <- paste0(examType, "cars")
    filterTablecars <- MCAT_clean_data %>%
      filter(MCAT_clean_data[carstest] == input[[useSliderCars]])
    
    bbtest <- paste0(examType, ".BB")
    useSliderBb <- paste0(examType, "bb")
    filterTablebb <- MCAT_clean_data %>%
      filter(MCAT_clean_data[bbtest] == input[[useSliderBb]])
    
    pstest <- paste0(examType, ".PS")
    useSliderPs <- paste0(examType, "ps")
    filterTableps <- MCAT_clean_data %>%
      filter(MCAT_clean_data[pstest] == input[[useSliderPs]])
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
    
        CARStable <- transmute(MCAT_clean_data, Real.CARS = Real.CARS, FL1.CARS = FL1.CARS) %>%
        filter(FL1.CARS == input$FL1cars) %>%
        gather("subsection", "score", 1))
      
      BB.PS.Table <- bind_rows(
        BBtable<- transmute(MCAT_clean_data, Real.BB = Real.BB, FL1.BB = FL1.BB) %>%
        filter(FL1.BB == input$FL1bb) %>%
        gather("subsection", "score", 1),
        
        PStable <- transmute(MCAT_clean_data, Real.PS = Real.PS, FL1.PS = FL1.PS) %>%
        filter(FL1.PS == input$FL1ps) %>%
        gather("subsection", "score", 1))
      
        
 MainTable <- bind_rows(CP.CARS.Table, BB.PS.Table)
 
 MainTable$Section <- factor(MainTable$subsection)
 
 
 median1 <- aggregate(score ~ Section, MainTable, median)
 
 
 cleanup <- theme(panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(),
                  axis.line = element_line(color = "black"))
<<<<<<< HEAD

 
 
=======
 
                
>>>>>>> b129e392b06d5536275f79ae11b0976d5dd45978
 
 MainTable$Section <- factor(MainTable$subsection, levels = c("Real.CP", "Real.CARS", "Real.BB", "Real.PS"))
 ggplot(MainTable, aes(Section, score)) + 
   geom_boxplot() + 
   ylim(118, 132) +
   xlab("Subsection") + 
   ylab("Scaled Score") + 
   cleanup +
<<<<<<< HEAD
   scale_x_discrete(labels = c("CP", "CARS", "BB", "PS")) +
   geom_text(data = median1, aes(label = score, y = score + 0.3))
=======
   scale_x_discrete(labels = c("CP", "CARS", "BB", "PS"))+
   geom_text( aes(x = Section, y = score, label = score, 
             size = 3, vjust = -1.5))
>>>>>>> b129e392b06d5536275f79ae11b0976d5dd45978
 
 

  })
  
  output$BoxPlot2 <- renderPlot({
    
    
    CP.CARS.Table <-bind_rows(
      CPtable <- transmute(MCAT_clean_data, Real.CP = Real.CP, FL2.CP = FL2.CP) %>%
        filter(FL2.CP == input$FL2cp) %>%
        gather("subsection", "score", 1), 
      
      CARStable <- transmute(MCAT_clean_data, Real.CARS = Real.CARS, FL2.CARS = FL2.CARS) %>%
        filter(FL2.CARS == input$FL2cars) %>%
        gather("subsection", "score", 1))
    
    BB.PS.Table <- bind_rows(
      BBtable<- transmute(MCAT_clean_data, Real.BB = Real.BB, FL2.BB = FL2.BB) %>%
        filter(FL2.BB == input$FL2bb) %>%
        gather("subsection", "score", 1),
      
      PStable <- transmute(MCAT_clean_data, Real.PS = Real.PS, FL2.PS = FL2.PS) %>%
        filter(FL2.PS == input$FL2ps) %>%
        gather("subsection", "score", 1))
    
    
    MainTable <- bind_rows(CP.CARS.Table, BB.PS.Table)
    
    cleanup <- theme(panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(),
                     panel.background = element_blank(),
                     axis.line = element_line(color = "black"))

    MainTable$Section <- factor(MainTable$subsection,  levels = c("Real.CP", "Real.CARS", "Real.BB", "Real.PS"))
    
    median2 <- aggregate(score ~ Section, MainTable, median)
    
    ggplot(MainTable, aes(Section, score)) + 
      geom_boxplot() + 
      ylim(118, 132) +
      xlab("Subsection") + 
      ylab("Scaled Score") + 
      cleanup +
      scale_x_discrete(labels = c("CP", "CARS", "BB", "PS")) +
      geom_text(data = median2, aes(label = score, y = score + 0.3))

                       
      
    
    
    
    
  })
   
    

    output$tsText <- renderText({
      practiceScorePredictor("FL1")
       # filter(FL1.CP == input$cpScore)
    #  filterTableCARS <- MCAT_clean_data %>%
     #   filter(FL1.CARS == input$carsScore)
    #  filterTablebb <- MCAT_clean_data %>%
    #    filter(FL1.BB == input$bbScore)
    #  filterTableps <- MCAT_clean_data %>%
    #    filter(FL1.PS == input$psScore)
   #   median(filterTablecp$Real.CP) + median(filterTableCARS$Real.CARS) + median(filterTablebb$Real.BB) + median(filterTableps$Real.PS)
    })   
    output$tsTextFL2 <- renderText({
      practiceScorePredictor("FL2")
      
    })
    
    output$Error <- renderText({
      "Please select a test to see a plot"
    })
    
    output$slider <- renderUI({
      
      
      if (input$`Practice Test` == "AAMC Full Length Test #1"){
        sidebarPanel(
          inputSlider("FL1", "cp", "Chem and Phys Score"),
          inputSlider("FL1", "cars", "CARS Score"),
          inputSlider("FL1", "bb", "Biology Score"),
          inputSlider("FL1", "ps", "Psych and Sociology Score")
        )

       }
      else if (input$`Practice Test` == "AAMC Full Length Test #2"){
        sidebarPanel(
          inputSlider("FL2", "cp", "Chem and Phys Score"),
          inputSlider("FL2", "cars", "CARS Score"),
          inputSlider("FL2", "bb", "Biology Score"),
          inputSlider("FL2", "ps", "Psych and Sociology Score")
          
        )
      
      }
      
    })
    
    output$Boxplot <- renderUI({
      
      if (input$`Practice Test` == "AAMC Full Length Test #1"){
        mainPanel(
          plotOutput("BoxPlot1"),
          strong("Predicted Score based on AAMC Full Length #1:"),
          textOutput("tsText")
        )}
      else if (input$`Practice Test` == "AAMC Full Length Test #2"){
        mainPanel(
          plotOutput("BoxPlot2"),
          strong("Predicted Score based on AAMC Full Length #2:"),
          textOutput("tsTextFL2")
        )
      }
      else
        mainPanel(
          textOutput("Error")
        )
        })
    output$secondData <- renderUI({
      if (input$'Practice Test' == "None"){}
      
      else if (input$'Practice Test' == "AAMC Full Length Test #1"){

        selectInput("PT2", h2("Please Select a Practice Test:"), c("None", "AAMC Full Length Test #2"))
      }
      
      else if (input$`Practice Test` == "AAMC Full Length Test #2"){
        selectInput("PT2", h2("Please Select a Practice Test:"), c("None", "AAMC Full Length Test #1"))
      }
   
 
    })
    
    output$secondSP <- renderUI({
      if (is.null(input$'PT2') == TRUE){
      }
        
        
      else if (input$'PT2' == "AAMC Full Length Test #2"){
        fluidRow(
        sidebarPanel(
          inputSlider("FL2", "cp", "Chem and Phys Score"),
          inputSlider("FL2", "cars", "CARS Score"),
          inputSlider("FL2", "bb", "Biology Score"),
          inputSlider("FL2", "ps", "Psych and Sociology Score")
        ),
         mainPanel(
           plotOutput("BoxPlot2"),
           strong("Predicted Score based on AAMC Full Length #2:"),
           textOutput("tsTextFL2")
         )
        )
      }
        else if (input$'PT2' == "AAMC Full Length Test #1"){
          fluidRow(
            sidebarPanel(
              inputSlider("FL1", "cp", "Chem and Phys Score"),
              inputSlider("FL1", "cars", "CARS Score"),
              inputSlider("FL1", "bb", "Biology Score"),
              inputSlider("FL1", "ps", "Psych and Sociology Score")
            ),
            mainPanel(
              plotOutput("BoxPlot1"),
              strong("Predicted Score based on AAMC Full Length #1:"),
              textOutput("tsText")
            )
          )
        }
    })
      
      

      #   selectInput("Practice Test", h4("Please Select a Practice Test:"), c("None", "AAMC Full Length Test #2"))
      # }
      # else if (input$'Practice Test' == "AAMC Full Length Test #2"){
      #   selectInput("Practice Test", h4("Please Select a Practice Test:"), c("None", "AAMC Full Length Test #1"))

      
      # else if (input$'PT2' == "AAMC Full Length Test #2"){
      #   sidebarPanel(
      #     inputSlider("FL2", "cp", "Chem and Phys Score"),
      #     inputSlider("FL2", "cars", "CARS Score"),
      #     inputSlider("FL2", "bb", "Biology Score"),
      #     inputSlider("FL2", "ps", "Psych and Sociology Score")
      #     
      #   )
      #   mainPanel(
      #     plotOutput("BoxPlot2"),
      #     strong("Predicted Score based on AAMC Full Length #2:"),
      #     textOutput("tsTextFL2")
      #   )
      #   
      # }
    
   
    
    
    
     output$median <- renderText({
      practiceScorePredictor("FL1")
    })
    
    
    
}