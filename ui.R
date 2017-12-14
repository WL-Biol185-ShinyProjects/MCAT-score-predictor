library(shiny)
library(shinythemes)

# This allowed us to create the shell of the functionality of the entire app with the creation of
# the sliders that the visitors to the app would use to input their scores.
inputSlider <- function(examType, subsection, subName)
  {
  sliderName <- paste0(subsection, "Score")
  sliderInput(
              sliderName,
              subName,
              min = 118,
              max = 132,
              value = 125)
}
#We used a main panel to create the most exterior level of our app. This also then allowed us to
#continue to use panels to organize all the other pages in our app.
    mainPanel(
      titlePanel("Welcome to our MCAT Score Predictor!"),
      
        #Here we used the navbarPage to pick the shiny theme, font, and size of the panel itself
        # that would allow our visitors to traverse through the website.
        navbarPage(
          theme = shinytheme("darkly"),
          "", 
          inverse = TRUE,
          header = tags$style(type = 'text/css', '.navbar {background-color: #969090;
                                                      font-family: Arial;
                                                      font-size: 18px;
                                                      color: #f9f9f9; }'),
          
          #This is the first panel of the page and it functions as the first page that visitors to our
          #page will see. It shows which test we are predicting scores for, explains a little about how
          # we are actually making the predictions, and allows the users to actually access the Reddit
          # page and the raw data that we are using to construct this app. 
          tabPanel("Home", verbatimTextOutput("Home"), 
                   
                   img(src = "mcat exam picture.png", height = 450, width = 450, align = "center"),
                   br(),
                   br(),
                   
                   p("Our App generates a predicted score, by taking your practice test score, filtering the crowdsourced data for all the users that got the same practice 
                      test score, averaging the value of their actual score on the real test by section, and adding them together for a predicted total score."),
                   br(),

                   p("We have created an interactive score predictor that uses crowdsourced data 
                   reported by members of the /r/MCAT community on reddit which can be found here:"), 
                   a("The Reddit page", 
                     href="https://www.reddit.com/r/Mcat/", 
                     target = "_blank"), 
                   br(),
                   br(),
                   a("The raw data can be accessed here",     
                     href="https://docs.google.com/spreadsheets/d/1JDrmJ1Aa0uvpUovylNvelmBaX_fQj_bhcQ7SVbf5o-M/edit?usp=sharing",
                     target = '_blank'),
                   br(),
                   br(),
                   br(),
                   br(),
                   br(),
                   p("Cover image credit to: www.brainscape.com")
                   
                   
          ),
          
          
          #This page is the one where the visitors will actually input the scores they recieved on one
          # of the practice tests and received a predicted actual exam score.
          tabPanel("Predict My Score", verbatimTextOutput("Predict My Score"),
                   
                  selectInput("Practice Test", 
                              h3("Please select a test:"), 
                              c("None", "AAMC Full Length Test #1", "AAMC Full Length Test #2", "Sample Test")),
                  
                  #The UI output here allows for the first slider, according to the scores that the visitor
                  # inputed, to output the corresponding box plot and table.  
                  uiOutput("test1UI"),
                  #Now we can add a second Dropdrown to determine if the visitory took more than one test.
                  uiOutput("secondDrop"),
                  #The UI output here allows for the second slider, according to the scores that the visitor
                  # inputed, to output the corresponding box plot and table. It also ensures that the visitor
                  #doesn't repeat the test that was previously selected.
                  uiOutput("test2UI")


          ),
          
          #This panel provides a short pictoral and word description of how to read a Boxplot
          # because we know that some people may not know how to read boxplots or may be 
          # intimidated by them. 
          tabPanel("How to Read a Boxplot", verbatimTextOutput("How to Read a Boxplot"),
                   
                   mainPanel(
                     h3("How to Read a Boxplot:"),
                     
                      img(src = "boxplot-with-outliers.jpg", height = 450, width = 650, align = "center"),
                      p("The authors are aware that a boxplot is not the easiest plot to read if you have never seen such a plot before. Therefore we would like
                        to provide a brief tutorial to explain:"),
                     br(),
                      p("The very dark line in the middle of each box plot represents the median of the data set. The top boundary and bottom boundary
                      of the box represent the 75th and 25th percentile of the data set. The 'whiskers' of the box plot represent the minimum and
                      maximum of the given data set. Any points outside of the box represent outliers in the data set."),
                     br(),
                     br(),
                     br(),
                     br(),
                     br(),
                     p("Image credit to: https://www.mathbootcamps.com/how-to-read-a-boxplot/")
                   
                   )),
          
          #This panel includes an HTML inset of the google form that users can use to add their information to 
          # the data set that we are using to pull the data to create the predictions that our visitors are looking
          # for. This will also allow for the visitors to the site to input their data if they aren't already in
          # the database.
          tabPanel("Help us be More Accurate", verbatimTextOutput("Help us be More Accurate"),
                   
                   h3("To help us make our score predictor even better, we encourage you to complete the score report AFTER you take the real MCAT and obtain your score."), 
                   tags$iframe(src = "https://docs.google.com/forms/d/e/1FAIpQLSdkZy9lZI2f8TuuA6GL9U2afXUrS1Segc2oYlJvExrxYbGqoQ/viewform?c=0&w=1&usp=send_??form",
                               width = 1000,
                               height = 1000),
                   br()
                   
          ),
          
          #This panel allows for a brief explanation for the reason for the creation of this app. It also provides
          # a disclaimer that as much as we can attempt to have an accurate prediction for the MCAT score, we are
          # still very much not guaranteed to be right everytime.
          tabPanel("About The Creators", verbatimTextOutput("About The Creators"),
                   h3("About:"),
                   
                   p("Greetings from Alex, Ron, and Deepthi"),
                   p("We hope that you have found our score predictor app to be useful. Hundreds of hours of research, data analysis, and coding went into this project, and we are incredibly proud of the final product. We just wanted to provide a little background on what motivated us to build a score predictor. In our own preparation for the MCAT, Ron and I were incredibly frustrated with the lack of resources available for monitoring our study progress. What practice score is good enough to reach my target score? How much can I realistically improve on test day? The new MCAT is long, expensive, and weighs into Medical School admission decision heavily, so we thought these questions were worth answering."),
                   p("A quick word of caution: Predicting the future is a murky business. Sometimes the weatherman forecasts sunny skies and it rains. We don't want you to put too much faith in our score predictor. We did the best that we could, but human beings don't always behave predictably. Test anxiety, family emergencies and mood can all affect your performance on test day. We therefore ask that you proceed cautiously with our prediction. It's only our best guess as to what you will score, and ultimately the ball is in your court on test day."),
                   br(),
                   p("Finally, we want to thank Dr. Whitworth. We completed this WebApp as our final project in his Bio-185 Big Data class, and he was integral in helping us learn R, trouble-shooting bugs in our app, and providing useful feedback along the way."), 
                   br()
          )
)

)

