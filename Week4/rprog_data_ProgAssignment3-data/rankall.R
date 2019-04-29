rankall <- function(outcome, num = "best") {
  ## read data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that outcome are valid
  outcomes <- c("heart attack", "heart failure", "pneumonia")
  
  ## For each state, find the hospital of the given rank
  state <- levels(factor(data[, 7]))
  hospital <- vector(mode="character") 
  
  for (i in seq(state)) {
    hospital[i] <- rankhospital(state[i], outcome, num)
  }
  
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  data.frame(hospital, state)

}
