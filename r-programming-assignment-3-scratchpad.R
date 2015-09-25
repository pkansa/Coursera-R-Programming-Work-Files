## load the outcome file
outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")  ## colClasses is forcing the data to be read in as char
## check the data
head(outcome)

## create a numeric vector of the 30-day mortality rates
outcome[, 11] <- as.numeric(outcome[, 11])
## You may get a warning about NAs being introduced; that is okay
## then generate a histogram of the vector
hist(outcome[, 11])
