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
               p("Welcome to our MCAT Score Predictor!"),
               br(),
               p("We have created an interactive score predictor that uses crowdsourced data 
                  reported by members of the /r/MCAT community on reddit which can be found here:"), 
               a("https://www.reddit.com/r/Mcat/"), 
               p("Our App generates a predicted
                  score, by taking your practice test score, filtering the crowdsourced data for all the users that got the same practice 
                 test score, averaging the value of their actual score on the real test by section, and adding them together for a predicted total score."),
               br()
        ),
      
      tabPanel("Predict My Score", verbatimTextOutput("Predict My Score"),
               
               br(),
               plotOutput("BoxPlot"),
               br(),
               strong("Predicted Score based on AAMC Full Length #1:"),
               textOutput("tsText"),
               br(),
               strong("Predicted Score based on AAMC Full Length #2:"),
               textOutput("tsTextFL2")
               
         ),
    
    
      tabPanel("About", verbatimTextOutput("About"),
               br(),
               h1("About"),

              p("Greetings from Alex, Ron, and Deepthie"),
              p("We hope that you have found our score predictor app to be useful. Hundreds of hours of research, data analysis, and coding went into this project, and we are incredibly proud of the final product. We just wanted to provide a little background on what motivated us to build a score predictor. In our own preparation for the MCAT, Ron and I were incredibly frustrated with the lack of resources available for monitoring our study progress. What practice score is good enough to reach my target score? How much can I realistically improve on test day? The new MCAT is long, expensive, and weighs into Medical School admission decision heavily, so we thought these questions were worth answering."),
              p("A quick word of caution: Predicting the future is a murky business. Sometimes the weatherman forecasts sunny skies and it rains. We don't want you to put too much faith in our score predictor. We did the best that we could, but human beings don't always behave predictably. Test anxiety, family emergencies and mood can all affect your performance on test day. We therefore ask that you proceed cautiously with our prediction. It's only our best guess as to what you will score, and ultimately the ball is in your court on test day."),
              br(),
              p("Finally, we want to thank Dr. Whitworth. We completed this WebApp as our final project in his Bio-185 Big Data class, and he was integral in helping us learn R, trouble-shooting bugs in our app, and providing useful feedback along the way."), 
              br()
      )
  
               
    ))
)

)
)





