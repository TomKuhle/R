featureEngineering <- function(dataIn) {
  
  # Marrital status
  single <- c("Master.", "Miss.")
  married <- c("Mr.", "Mrs.")
  
  splitCols <- data.frame(AgeFeat = dataIn$Age,
                       FareFeat = dataIn$Fare)
  
  ageRange <- range(splitCols$AgeFeat, na.rm = TRUE)
  fareRange <- range(splitCols$FareFeat, na.rm = TRUE)
  
  splitCols <- splitCols %>% mutate(AgeSteps = abs(diff(ageRange))/15,
                                    FareSteps = abs(diff(fareRange))/50)
  
  # Allocate ages to age intervals
  ageBreaks <- seq(from = ageRange[1], to = ageRange[2], by = splitCols$AgeSteps[1])
  fareBreaks <- seq(from = fareRange[1], to = fareRange[2], by = splitCols$FareSteps[1])
  
  # Contingency table
  vecCutAge <- cut(splitCols$AgeFeat, breaks = ageBreaks, include.lowest = TRUE)
  vecCutFare <- cut(splitCols$FareFeat, breaks = fareBreaks, include.lowest = TRUE)
  breakTable <- tibble::tibble(AgeBreak = vecCutAge,
                             FareBreak = vecCutFare)
  
  
  # Roughly 10 year Age intervals
  # ageVec <- dataIn$Age
  # ageRange <- range(dataIn$Age)
  # ageStep <- abs(diff(ageRange))/8
  # 
  # # Allocate ages to age intervals
  # ageBreaks <- seq(from = ageRange[1], to = ageRange[2], by = ageStep)
  # 
  # # Contingency table
  # vecCut <- cut(ageVec, breaks = ageBreaks, include.lowest = TRUE)
  # ageTable <- tibble::tibble(AgeBreak = vecCut)
  
  # Age in ~10 year steps
  # Ticket and fare
  featured <- dataIn %>% 
    cbind(breakTable) %>%
    mutate(Status = case_when(grepl(paste(single, collapse = "|"), Name) ~ "single",
                              grepl(paste(married, collapse = "|"), Name) ~ "married",
                              TRUE ~ "other")) %>%
    group_by(AgeBreak) %>% mutate(Age = cur_group_id()) %>% ungroup() %>%
    group_by(FareBreak) %>% mutate(Fare = cur_group_id()) %>% ungroup() %>%
    group_by(Ticket, Fare) %>% mutate(TicketFare = cur_group_id()) %>% ungroup()
  
  return(featured)
  
}