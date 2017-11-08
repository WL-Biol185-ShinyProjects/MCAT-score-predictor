library(shiny)

fluidPage(
  
  titlePanel("MCAT Score Predictor"),
  
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
      
      
      tabPanel("How does this work?", verbatimTextOutput("How does this work?"),
      
              p("Our predictor works by comparing the subsection score that you input to the list of matching scores on the 
                same test and subsection that we have crowd-sourced. Then we took all of the real exam scores for that subsection
                (within plus or minus 2 points) and printed out a corresponding box plot. Use the data table below to see how we 
                filtered your scores to get your estimated real score."),
              br()
              ),

      tabPanel("Scores", verbatimTextOutput("Scores"),
  
               sidebarLayout(
                 sidebarPanel(
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
                 ),
                 
                 mainPanel(
                  fluidRow(
                     plotOutput("BoxPlot"),
                     textOutput("tsText")
                   ) 
                )
    ))
))
