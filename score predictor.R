
practiceScorePredictor <- function(input, output)
  filterTablecp <- MCAT_clean_data %>%
    filter(examType.CP == input$cpScore)
  filterTableCARS <- MCAT_clean_data %>%
    filter(examType.CARS == input$carsScore)
  filterTablebb <- MCAT_clean_data %>%
    filter(examType.BB == input$bbScore)
  filterTableps <- MCAT_clean_data %>%
    filter(examType.PS == input$psScore)
  median(filterTablecp$Real.CP) + median(filterTableCARS$Real.CARS) + median(filterTablebb$Real.BB) + median(filterTableps$Real.PS)