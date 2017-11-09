practiceScorePredictor2 <- function(input, output, session, examType)
  
{
  ns <- NS(id)
  
  CPtest <- paste0(examType, ".CP")
  filterTablecp <- MCAT_clean_data %>%
    filter(MCAT_clean_data[CPtest] == ns(input$cpScore))
  # filtercol <- MCAT_clean_data[CPtest]
  #filterTablecp <- filter(filtercol == input$cpScore)
  
  carstest <- paste0(examType, ".CARS")
  filterTablecars <- MCAT_clean_data %>%
    filter(MCAT_clean_data[carstest] == ns(input$carsScore))
  
  bbtest <- paste0(examType, ".BB")
  filterTablebb <- MCAT_clean_data %>%
    filter(MCAT_clean_data[bbtest] == ns(input$bbScore))
  
  pstest <- paste0(examType, ".PS")
  filterTableps <- MCAT_clean_data %>%
    filter(MCAT_clean_data[pstest] == ns(input$psScore))
  
  tagList(
    median(filterTablecp$Real.CP) + median(filterTablecars$Real.CARS) + median(filterTablebb$Real.BB) + median(filterTableps$Real.PS))
  
}