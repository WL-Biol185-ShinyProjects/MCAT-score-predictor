library(shiny)

sliderCreator <- function(examType) {
  inputSlider(examType, "CP", "Chem and Phys Score")
  inputSlider(examType, "CARS", "CARS Score")
  inputSlider(examType, "BB", "Bio and Biochem Score")
  inputSlider(examType, "PS", "Psych and Soc Score")
  
}

inputSlider <- function(examType, subsection, subName){
  sliderName <- paste0(examType, subsection)
  sliderInput(sliderName,
              subName,
              min = 118,
              max = 132,
              value = 125)
    
}

fluidPage(
  
  titlePanel("MCAT Score Predictor"),
  
  
    mainPanel(
      fluidRow(
        
        
  navbarPage("",
             
      tabPanel("Home", verbatimTextOutput("Home"),
      
               img(src = "mcat exam picture.png", height = 450, width = 450, align = "center"),
               br(),
               br(),
               p("Welcome to our MCAT Score Predictor! Our goal is to use a conglomeration of volunteered
                  scores from the internet community via Reddit(link). Our predictor takes your scores on the subsections
                  on a variety of practice tests and compares them to comparable reported scores. Then we provide a box 
                  plot of the actual test scores that correlate to provide you a predicted score within 2 points in either
                  direction."),
               br()
              ),
      
<<<<<<< HEAD
      
      tabPanel("How does this work?", verbatimTextOutput("How does this work?"),
      
              p("Our predictor works by comparing the subsection score that you input to the list of matching scores on the 
                same test and subsection that we have crowd-sourced. Then we took all of the real exam scores for that subsection
                (within plus or minus 2 points) and printed out a corresponding box plot. Use the data table below to see how we 
                filtered your scores to get your estimated real score."),
              br()
              ),

      tabPanel("Scores", verbatimTextOutput("Scores"),
  
               sidebarLayout(
=======
      tabPanel("Predict My Score", verbatimTextOutput("Predict My Score"),
>>>>>>> 4790d8635f0fefcc928e84e3dbfa2443a37fda4e
                 sidebarPanel(
                 #  sliderCreator("FL1")
                   sliderInput("cpScore",
                               "Chem and Phys Score",
                               min = 118,
                               max = 132,
                               value = 125),
                   sliderInput("carsScore",
                               "CARS Score",
                               min = 118,
                               max = 132,
                               value = 125),
                   sliderInput("bbScore",
                               "Biology Score",
                               min = 118,
                               max = 132,
                               value = 125),
                   sliderInput("psScore",
                               "Psych and Sociology Score",
                               min = 118,
                               max = 132,
                               value = 125)
             #      inputSlider("FL1", "CP", "Chem and Phys")
                   ),
               mainPanel(
                plotOutput("BoxPlot"),
                textOutput("tsText"),
                textOutput("tsTextFL2")
              #  sliderCreator("FL1"))
               )
      
          
      ),
      

      tabPanel("About", verbatimTextOutput("About"))
      
  
               
    ))
<<<<<<< HEAD
))
=======
)

)




>>>>>>> 4790d8635f0fefcc928e84e3dbfa2443a37fda4e
