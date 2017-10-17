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
      sliderInput("bins3",
                  "Biology Score",
                  min = 118,
                  max = 132,
                  value = 125),
      sliderInput("bins4",
                  "Psych and Sociology Score",
                  min = 118,
                  max = 132,
                  value = 125)
    ),
    mainPanel(
      fluidRow(
        plotOutput("cpPlot"),
        plotOutput("carsPlot")

    )
  )
))