rankhospital <- function(state, outcome, num = "best") {  

  #state = "TX"
  #outcome = "pneumonia"
  #num = 10
  
  ## read data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state and outcome are valid
  states <- unique(data[ , 7])
  outcomes <- c("heart attack", "heart failure", "pneumonia")
  
  ## Validate params
  if ((state %in% states) == FALSE) {
    stop(print("invalid state"))
  } else if ((outcome %in% outcomes) == FALSE) {
    stop(print("invalid outcome"))
  }
  
  ## Return hospital name in that state with lowest 30-day death
  
  #get the subset of the data with the desired state
  new_data <- subset(data, State == state)
  
  # return NA if exceed the number of hospitals in state
  if (is.numeric(num) == TRUE) {
    if (length(new_data[,2]) < num) {
      return(NA)
    }
  }
  
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
  
  ## arrange by outcome and hospital
  outcome_column_name <- names(desired_data)[outcome_column]
  hospital_column_name <- names(desired_data)[2]
  index <- with(desired_data, order(desired_data[outcome_column], 
                                    desired_data[hospital_column_name]))
  ordered_desired_data <- desired_data[index, ]
  
  #convert "best" or "worst", to numeric values
  if (is.character(num) == TRUE) {
    if (num == "best") {
      num = 1
    }else if (num == "worst") {
      num = length(ordered_desired_data[, outcome_column])
    }
  }
  
  ## rate
  ordered_desired_data[num, 2]
  
}

