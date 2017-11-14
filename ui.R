library(shiny)
library(shinythemes)

inputSlider <- function(examType, subsection, subName){
  sliderName <- paste0(subsection, "Score")
  sliderInput(sliderName,
              subName,
              min = 118,
              max = 132,
              value = 125)
  
}

fluidPage(
  theme = shinytheme("superhero"),
  titlePanel("Welcome to our MCAT Score Predictor!"),
  
    mainPanel(
      fluidRow(
        
        navbarPage(
          "", 
          inverse = TRUE,
          header = tags$style(type = 'text/css', '.navbar { background-color: #8d8d9b;
                                                      font-family: Arial;
                                                      font-size: 18px;
                                                      color: #94939b; }'),
          
          
          tabPanel("Home", verbatimTextOutput("Home"), 
                   
                   br(),
                   img(src = "mcat exam picture.png", height = 450, width = 450, align = "center"),
                   br(),
                   br(),
                   p("We have created an interactive score predictor that uses crowdsourced data 
                  reported by members of the /r/MCAT community on reddit which can be found here:"), 
                   a("https://www.reddit.com/r/Mcat/"), 
                   p("Our App generates a predicted
                  score, by taking your practice test score, filtering the crowdsourced data for all the users that got the same practice 
                 test score, averaging the value of their actual score on the real test by section, and adding them together for a predicted total score."),
                   p("A link to the raw data can be accessed here:"),
                   a("https://docs.google.com/spreadsheets/d/1JDrmJ1Aa0uvpUovylNvelmBaX_fQj_bhcQ7SVbf5o-M/edit?usp=sharing"),
                   br()
                   
          ),
          
          tabPanel("Predict My Score", verbatimTextOutput("Predict My Score"),
                   
                   selectInput("Practice Test", "Select Practice Test:", c("AAMC Full Length Test #1", "AAMC Full Length Test #2")),
                   
                   sidebarPanel(
                     inputSlider("FL1", "cp", "Chem and Phys Score"),
                     inputSlider("FL1", "cars", "CARS Score"),
                     inputSlider("FL1", "bb", "Biology Score"),
                     inputSlider("FL1", "ps", "Psych and Sociology Score")
                   ),
                   
                   mainPanel(
                     plotOutput("BoxPlot"),
                     strong("Predicted Score based on AAMC Full Length #1:"),
                     textOutput("tsText"),
                     strong("Predicted Score based on AAMC Full Length #2:"),
                     textOutput("tsTextFL2")
                   )
                   
          ),
          
          tabPanel("Help us be more accurate", verbatimTextOutput("Help us be more accurate"),
                   
                   br(),
                   p("To help us make our score predictor even better, we encourage you to complete the score report AFTER you take the real MCAT and obtain your score."), 
                   tags$iframe(src = "https://docs.google.com/forms/d/e/1FAIpQLSdkZy9lZI2f8TuuA6GL9U2afXUrS1Segc2oYlJvExrxYbGqoQ/viewform?c=0&w=1&usp=send_??form",
                               width = 1000,
                               height = 1000),
                   br()
                   
          ),
          
          tabPanel("About The Creators", verbatimTextOutput("About The Creators"),
                   br(),
                   h3("About:"),
                   
                   p("Greetings from Alex, Ron, and Deepthi"),
                   p("We hope that you have found our score predictor app to be useful. Hundreds of hours of research, data analysis, and coding went into this project, and we are incredibly proud of the final product. We just wanted to provide a little background on what motivated us to build a score predictor. In our own preparation for the MCAT, Ron and I were incredibly frustrated with the lack of resources available for monitoring our study progress. What practice score is good enough to reach my target score? How much can I realistically improve on test day? The new MCAT is long, expensive, and weighs into Medical School admission decision heavily, so we thought these questions were worth answering."),
                   p("A quick word of caution: Predicting the future is a murky business. Sometimes the weatherman forecasts sunny skies and it rains. We don't want you to put too much faith in our score predictor. We did the best that we could, but human beings don't always behave predictably. Test anxiety, family emergencies and mood can all affect your performance on test day. We therefore ask that you proceed cautiously with our prediction. It's only our best guess as to what you will score, and ultimately the ball is in your court on test day."),
                   br(),
                   p("Finally, we want to thank `Dr. Whitworth. We completed this WebApp as our final project in his Bio-185 Big Data class, and he was integral in helping us learn R, trouble-shooting bugs in our app, and providing useful feedback along the way."), 
                   br()
          )
  
               
    ))

))

