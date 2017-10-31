
practiceScorePredictor <- function(examType)
  
{
  CPtest <- paste0(exam, ".CP")
  filterTablecp <- MCAT_clean_data %>%
    filter(CPtest == input$cpScore)
 
  carstest <- paste0(exam, ".CARS")
  filterTablecars<- MCAT_clean_data %>%
    filter(carstest == input$carsScore)
  
  bbtest <- paste0(exam, ".BB")
  filterTablebb <- MCAT_clean_data %>%
    filter(bbtest == input$bbScore)
  
  pstest <- paste0(exam, ".PS")
  filterTableps <- MCAT_clean_data %>%
    filter(pstest == input$psScore)
  median(filterTablecp$Real.CP) + median(filterTableCARS$Real.CARS) + median(filterTablebb$Real.BB) + median(filterTableps$Real.PS)
  
  }