library(shiny)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(shinythemes)
library(readxl)

#Opens up our dataset
MCAT_clean_data <- read_csv("MCAT clean data.csv")

function(input, output) {
  
  #Boxplot generation function for different types of practice exams
  #Currently not working
  
  # boxplotCreator <- function(examType)
  # {
  #   examCP <- paste0(examType, ".CP")
  #   examCARS <- paste0(examType, ".CARS")
  #   examBB <- paste0(examType, ".BB")
  #   examPS <- paste0(examType, ".PS")
  #   sliderCP <- paste0(examType, "cp")
  #   sliderCARS <- paste0(examType, "cars")
  #   sliderBB <- paste0(examType, "bb")
  #   sliderPS <- paste0(examType, "ps")
  #   
  #   CP.CARS.Table <-bind_rows(
  #     CPtable <- transmute(MCAT_clean_data, Real.CP = Real.CP, examCP = MCAT_clean_data[[examCP]]) %>%
  #       na.omit(examCP) %>%
  #       filter(CPtable[examCP] == input[[sliderCP]]) #%>%
  #     gather("subsection", "score", 1)
  #     if (nrow(CPtable) == 0) {
  #       CPtable <- add_row(CPtable, "subsection" = "Real.CP", "score" = input[[sliderCP]])}
  #     
  #     CARStable <- transmute(MCAT_clean_data, Real.CARS = Real.CARS, examCARS = MCAT_clean_data[[examCARS]]) %>%
  #       na.omit(examCP) %>%
  #       filter(examCARS == input[[sliderCARS]]) %>%
  #       gather("subsection", "score", 1),
  #     if (nrow(CARStable) == 0) {
  #       CARStable <- add_row(CARStable, "subsection" = "Real.CARS", "score" = input[[sliderCP]])}
  #   )
  #   
  #   BB.PS.Table <- bind_rows(
  #     BBtable<- transmute(MCAT_clean_data, Real.BB = Real.BB, examBB = MCAT_clean_data[[examBB]]) %>%
  #       na.omit(examCP) %>%
  #       filter(examBB == input[[sliderBB]]) %>%
  #       gather("subsection", "score", 1),
  #     if (nrow(BBtable) == 0) {
  #       BBtable <- add_row(BBtable, "subsection" = "Real.BB", "score" = input[[sliderBB]])},
  #     
  #     PStable <- transmute(MCAT_clean_data, Real.PS = Real.PS, examPS = MCAT_clean_data[[examPS]]) %>%
  #       na.omit(examCP) %>%
  #       filter(examPS == input[[sliderPS]]) %>%
  #       gather("subsection", "score", 1),
  #     if (nrow(PStable) == 0) {
  #       PStable <- add_row(PStable, "subsection" = "Real.PS", "score" = input[[sliderPS]])}
  #   )
  #   
  #   
  #   MainTable <- bind_rows(CP.CARS.Table, BB.PS.Table)
  #   
  #   cleanup <- theme(panel.grid.major = element_blank(),
  #                    panel.grid.minor = element_blank(),
  #                    panel.background = element_blank(),
  #                    axis.line = element_line(color = "black"))
  #   
  #   MainTable$Section <- factor(MainTable$subsection,  levels = c("Real.CP", "Real.CARS", "Real.BB", "Real.PS"))
  #   
  #   median2 <- aggregate(score ~ Section, MainTable, median)
  #   
  #   ggplot(MainTable, aes(Section, score, fill = Section)) +
  #     geom_boxplot() +
  #     ylim(118, 132) +
  #     xlab("Subsection") +
  #     ylab("Scaled Score") +
  #     cleanup +
  #     scale_x_discrete(labels = c("CP", "CARS", "BB", "PS")) +
  #     
  #     geom_text(data = median2, aes(label = score, y = score - 0.3)) +
  #     guides(fill = FALSE)
  # }
  
  #Generates a predicted test score based on slider input values for subsections
  practiceScorePredictor <- function(examType, sampleTest = FALSE){
    
    #Looks at the CP section and associated slider
    CPtest <<- paste0(examType, ".CP")
    useSliderCp <- paste0(examType, "cp")
    
    #Filters data for CP section based on the input score
    filterTablecp <<- MCAT_clean_data %>%
      filter(MCAT_clean_data[CPtest] == input[[useSliderCp]])
    
    #If data point is missing returns input value for practice tests or 125 for sample test
    if (nrow(filterTablecp) == 0 & sampleTest == TRUE) {
      filterTablecp <- add_row(filterTablecp, Real.CP = 125)
    }
    
    else if (nrow(filterTablecp) == 0) {
      filterTablecp <- add_row(filterTablecp, Real.CP = input[[useSliderCp]])
    }
    
    #See comments above but now for CARS section
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
    
    #See CP section
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
    
    #See CP section
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
    
    #Collects the median values for the predicted score and assigns them as global variables for later use
    medianCP <<- median(filterTablecp$Real.CP)
    medianCARS <<- median(filterTablecars$Real.CARS)
    medianBB <<- median(filterTablebb$Real.BB)
    medianPS <<- median(filterTableps$Real.PS)
    
    #Collects the number of data points for use in the table
    numberData <<- nrow(filterTablecp) + nrow(filterTablecars) + nrow(filterTablebb) + nrow(filterTableps)
    
    #Return the predicted score as the sum of all subsections
    median(filterTablecp$Real.CP) + median(filterTablecars$Real.CARS) + median(filterTablebb$Real.BB) + median(filterTableps$Real.PS)
     
  }
  
  #Creates a slider function that generates sliders based on MCAT values
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
  
  #Creates all 4 sliders for all the subsections per exam
  sliderSet <- function(examType, sample = FALSE) {
    if (sample == FALSE){
     fluidRow(
     inputSlider(examType, "cp", "Chemsitry and Physics Score"),
     inputSlider(examType, "cars", "Critical Analysis and Reading Score"),
     inputSlider(examType, "bb", "Biology and Biochemsitry Score"),
     inputSlider(examType, "ps", "Psychology and Sociology Score")
    )
    }
    
    #Generates a different sliderset for sample tests
    else if (sample == TRUE){
      sliderCP <- paste0(examType, "cp")
      sliderCARS <- paste0(examType, "cars")
      sliderBB <- paste0(examType, "bb")
      sliderPS <- paste0(examType, "ps")
      
      #Generates the UI for the sliders
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
  
  #Creates a boxplot for practice exam one
  output$BoxPlot1 <- renderPlot({
    
      #Compiles the filtered data for CP and CARS
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
      
      #Compiles the data for BB and PS
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
      #Binds the previously compiled data making it tidy        
      MainTable <- bind_rows(CP.CARS.Table, BB.PS.Table)
      
      #Creates a subsection column for the subsections
      MainTable$Section <- factor(MainTable$subsection)
      median1 <<- aggregate(score ~ Section, MainTable, median)
      
      cleanup <- theme(panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(),
                  axis.line = element_line(color = "black"))
      
      #Generates GG plot with appropriate colors and axis labels along with showing the median score of each subsection on its boxplot
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
  
  #See BoxPlot1 but this time for practice exam 2 
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
  
  #See BoxPlot1 but this time for sample test
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
   
    #Prints the predicted exam score for FL1
    output$tsTextFL1 <- renderText({
        practiceScorePredictor("FL1")

    })
    
    #Prints the predicted exam score for FL2
    output$tsTextFL2 <- renderText({
      practiceScorePredictor("FL2")
      
    })
    
    #Prints the predicted exam score for sample test
    output$tsTextST <- renderText({
      practiceScorePredictor("ST", sampleTest = TRUE)
      
    })
    
    #Creates the UI for the first dropdown menu depending on which test is selected
    output$test1UI <- renderUI({
      
      #Checks to see if test 1 is selected and generates test 1 UI
      if (input$`Practice Test` == "AAMC Full Length Test #1"){
          #Builds sliders
          fluidRow(
            column(2,
             sliderSet("FL1")
           ),
           
          column(6,
            #Builds boxplot UI
            plotOutput("BoxPlot1"),
            strong("Predicted Score based on AAMC Full Length #1:"),
            textOutput("tsTextFL1")
            ),
          column(4,
            #Builds table UI
            tableOutput("tableFL1")
            ))
      }
      
      #Builds UI for test 2 if it is selected
      else if (input$`Practice Test` == "AAMC Full Length Test #2"){
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
                   tableOutput("tableFL2"))
                  )
        }
      
      #Builds UI for sample test if it is selected
      else if (input$`Practice Test` == "Sample Test"){
        fluidRow(
          column(2,
                 sliderSet("ST", sample = TRUE)
                 ),
          column(6,
                 plotOutput("BoxPlot3"),
                 strong("Predicted Score based on AAMC Sample Test:"),
                 textOutput("tsTextST")
                 ),
          column(4,
                 tableOutput("tableST"))
          
        )
      }
      
    })
    
    #Creates second dropdown menu with leftover items not selected in the first one
    output$secondDrop <- renderUI({
    
      if (input$'Practice Test' == "None"){}
      
      #if test 1 is selected dropdown menu has sample and test 2
      else if (input$'Practice Test' == "AAMC Full Length Test #1"){
        selectInput("PT2", h2("Please Select a Practice Test:"), c("None", "AAMC Full Length Test #2", "Sample Test"))
      }
      
      #if test 2 is selected dropdown menu has sample and test 1
      else if (input$`Practice Test` == "AAMC Full Length Test #2"){
        selectInput("PT2", h2("Please Select a Practice Test:"), c("None", "AAMC Full Length Test #1", "Sample Test"))
      }
      
      #if sample is selected dropdown menu has sample and test 1 and 2
      else if (input$`Practice Test` == "Sample Test"){
        selectInput("PT2", h2("Please Select a Practice Test:"), c("None", "AAMC Full Length Test #1", "AAMC Full Length Test #2"))
      }
   
 
    })
    
    #Builds UI for the second drop down menu depending on what is selected. See test1UI
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
                 
                 tableOutput("tableFL2")
                 )
        )
        }
        else if (input$'PT2' == "AAMC Full Length Test #1"){
          fluidRow(
            column(2,
                   sliderSet("FL1")
                   ),
            
            column(6,
                   plotOutput("BoxPlot1"),
                   strong("Predicted Score based on AAMC Full Length #1:"),
                   textOutput("tsTextFL1")
                   ),
            column(4,
                   
                   tableOutput("tableFL1")
                   )
          )
          }
      else if (input$'PT2' == "Sample Test"){
          fluidRow(
            column(2,
                   sliderSet("ST", sample = TRUE)
                   ),
            column(6,
                   plotOutput("BoxPlot3"),
                   strong("Predicted Score based on AAMC Sample Test:"),
                   textOutput("tsTextST")
                   ),
            column(4,
                   tableOutput("tableST")
                   )
          )
        }
    })
    
    #Builds a table for displaying predicted score values
    output$tableFL1 <- renderTable({
      #Creates a tibble using global variables from the practiceScorePredictor
      medianTable <- tibble("Test Subsection" = c("Chemistry and Physics", "Critical Analysis and Reading", "Biology and Biochemistry", "Psychology and Sociology", "Total"), "Predicted Score" = c(medianCP, medianCARS, medianBB, medianPS, practiceScorePredictor("FL1")), "Number of Data Points" = c(nrow(filterTablecp), nrow(filterTablecars), nrow(filterTablebb), nrow(filterTableps), numberData))
      },
      
      #Creates table borders and allows for one digit after decimel point
      striped = TRUE, bordered = TRUE, digits = 1, align = "c")
    
    #See table 
    output$tableFL2 <- renderTable({
      medianTable <- tibble("Test Subection" = c("Chemistry and Physics", "Critical Analysis and Reading ", "Biology and Biochemistry", "Psychology and Sociology", "Total"), "Predicted Score" = c(medianCP, medianCARS, medianBB, medianPS, practiceScorePredictor("FL2")), "Number of Data Points" = c(nrow(filterTablecp), nrow(filterTablecars), nrow(filterTablebb), nrow(filterTableps), numberData))
      },
      striped = TRUE, bordered = TRUE, digits = 1, align = "c")
    
    #See table
    output$tableST <- renderTable({
       medianTable <- tibble("Test Subection" = c("Chemistry and Physics", "Critical Analysis and Reading ", "Biology and Biochemistry", "Psychology and Sociology", "Total"), "Predicted Score" = c(medianCP, medianCARS, medianBB, medianPS, practiceScorePredictor("ST", sampleTest = TRUE)), "Number of Data Points" = c(nrow(filterTablecp), nrow(filterTablecars), nrow(filterTablebb), nrow(filterTableps), numberData))
       },
       striped = TRUE, bordered = TRUE, digits = 1, align = "c")
}