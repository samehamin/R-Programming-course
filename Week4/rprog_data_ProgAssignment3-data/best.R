best <- function(state, outcome) {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state and outcome are valid
  states <- unique(data[ , 7])
  outcomes <- c("heart attack", "heart failure", "pneumonia")
  
  if ((state %in% states) == FALSE) {
    stop(print("invalid state"))
  } else if ((outcome %in% outcomes) == FALSE) {
    stop(print("invalid outcome"))
  }
  
  ## Return hospital name in that state with lowest 30-day death
  
  #get the subset of the data with the desired state
  new_data <- subset(data, State == state)
  
  #get the desired outcome column from the data file
  if (outcome == "heart attack") {
    outcome_column <- 11
  }else if (outcome == "heart failure") {
    outcome_column <- 17
  }else {
    outcome_column <- 23
  }
  
  # clean NAs values
  required_columns <- as.numeric(new_data[,outcome_column])
  bad <- is.na(required_columns)
  desired_data <- new_data[!bad, ]
  
  #arrange the modified dataframe in ascending order of the outcome values
  columns_considered <- as.numeric(desired_data[, outcome_column])
  desired_rows <- which(columns_considered == min(columns_considered))
  desired_hospitals <- desired_data[desired_rows, 2]
  
  ## return the first hospital with duplicates
  if (length(desired_hospitals) > 1) {
    hospitals_sorted <- sort(desired_hospitals)
    hospitals_sorted[1]
  }else {
    desired_hospitals
  }
}
