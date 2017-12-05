library(shiny)
library(shinythemes)

inputSlider <- function(examType, subsection, subName)
  {
  sliderName <- paste0(subsection, "Score")
  sliderInput(sliderName,
              subName,
              min = 118,
              max = 132,
              value = 125)
  }
    mainPanel(
      titlePanel("Welcome to our MCAT Score Predictor!"),
      
      fluidRow(
        navbarPage(
          theme = shinytheme("superhero"),
          "", 
          inverse = TRUE,
          header = tags$style(type = 'text/css', '.navbar {background-color: #969090;
                                                      font-family: Arial;
                                                      font-size: 18px;
                                                      color: #f9f9f9; }'),
          
          tabPanel("Home", verbatimTextOutput("Home"), 
                   
                   br(),
                   img(src = "mcat exam picture.png", height = 450, width = 450, align = "center"),
                   br(),
                   br(),
                   
                   p("Our App generates a predicted score, by taking your practice test score, filtering the crowdsourced data for all the users that got the same practice 
                      test score, averaging the value of their actual score on the real test by section, and adding them together for a predicted total score."),
                   br(),

                   p("We have created an interactive score predictor that uses crowdsourced data 
                   reported by members of the /r/MCAT community on reddit which can be found here:"), 
                   a("https://www.reddit.com/r/Mcat/"), 
             
                   p("The raw data can be accessed here:"),
                   a("https://docs.google.com/spreadsheets/d/1JDrmJ1Aa0uvpUovylNvelmBaX_fQj_bhcQ7SVbf5o-M/edit?usp=sharing"),
                   br()
                   
          ),
          
          tabPanel("Predict My Score", verbatimTextOutput("Predict My Score"),
                   
                  selectInput("Practice Test", 
                              h2("Please select a test:"), 
                              c("None", "AAMC Full Length Test #1", "AAMC Full Length Test #2")),
                  
                  uiOutput("test1UI"),
                  uiOutput("secondDrop"),
                  uiOutput("test2UI")

          ),
          
          tabPanel("How to Read a Boxplot", verbatimTextOutput("How to Read a Boxplot"),
                   
                   mainPanel(
                     br(),
                     h2("How to Read a Boxplot:"),
                     
                      img(src = "boxplot-with-outliers.jpg", height = 450, width = 650, align = "center"),
                      p("The authors are aware that a boxplot is not the easiest plot to read if you have never seen such a plot before. Therefore we would like
                        to provide a brief tutorial to explain:"),
                     
                      fluidRow(
                       column(2,""),
                       column(10,
                         p("The very dark line in the middle of each box plot represents the median of the data set. The top boundary and bottom boundary
                         of the box represent the 75th and 25th percentile of the data set. The 'whiskers' of the box plot represent the minimum and
                         maximum of the given data set. Any points outside of the box represent outliers in the data set."))
                       )
                   
                   )),
          
          
          tabPanel("Help us be More Accurate", verbatimTextOutput("Help us be More Accurate"),
                   
                   h3("To help us make our score predictor even better, we encourage you to complete the score report AFTER you take the real MCAT and obtain your score."), 
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
                   p("Finally, we want to thank Dr. Whitworth. We completed this WebApp as our final project in his Bio-185 Big Data class, and he was integral in helping us learn R, trouble-shooting bugs in our app, and providing useful feedback along the way."), 
                   br()
          )
)

))

