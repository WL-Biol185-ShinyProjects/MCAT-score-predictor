practiceScorePredictor2 <- function(input, output, session, examType)
  
{
  
  CPtest <- paste0(examType, ".CP")
  filterTablecp <- MCAT_clean_data %>%
    filter(MCAT_clean_data[CPtest] == input$cpScore)
  # filtercol <- MCAT_clean_data[CPtest]
  #filterTablecp <- filter(filtercol == input$cpScore)
  
  carstest <- paste0(examType, ".CARS")
  filterTablecars <- MCAT_clean_data %>%
    filter(MCAT_clean_data[carstest] == input$carsScore)
  
  bbtest <- paste0(examType, ".BB")
  filterTablebb <- MCAT_clean_data %>%
    filter(MCAT_clean_data[bbtest] == input$bbScore)
  
  pstest <- paste0(examType, ".PS")
  filterTableps <- MCAT_clean_data %>%
    filter(MCAT_clean_data[pstest] == input$psScore)
  
  tagList(
    median(filterTablecp$Real.CP) + median(filterTablecars$Real.CARS) + median(filterTablebb$Real.BB) + median(filterTableps$Real.PS))
  
}