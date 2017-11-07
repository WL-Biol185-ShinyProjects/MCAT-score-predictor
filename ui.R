library(shiny)

fluidPage(
  
  titlePanel("MCAT Score Predictor"),
  
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
      
      tabPanel("Predict My Score", verbatimTextOutput("Predict My Score"),
               br(),
               plotOutput("BoxPlot"),
               textOutput("tsText"),
               textOutput("tsTextFL2")
      
          
      ),
      

      tabPanel("About", verbatimTextOutput("About"))
      
  
               
    ))
)

)

)


