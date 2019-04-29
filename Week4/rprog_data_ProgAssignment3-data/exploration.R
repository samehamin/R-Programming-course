# read the file
outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
head(outcome)

summary(outcome)

# hist the death rate from heart attack
outcome[, 11] <- as.numeric(outcome[, 11])
hist(outcome[,11])
