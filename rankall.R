rankall <- function(outcome, num = "best") {
        ## Read outcome data
        outcome.df <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        ## Check that state and outcome are valid
        ## check the outcome first
        validOutcome = c("heart attack","heart failure","pneumonia") ## concatenate the three outcomes we'll be checking for
        if (!outcome %in% validOutcome) { stop("Invalid outcome entered; check inputs")} ## if the input is not valid, stop the function
        
        ## then check the state
        validState = sort(unique(outcome.df[,7])) ## get a list of the unique state values, sorted alphabetically
        ##if (!state %in% validState) stop("Invalid state entered; check inputs") ## if the input is not valid, stop the function
        
        ## For each state, find the hospital of the given rank
        ## Build a vector that contains the three column names we would be searching through
        fullColName <- c("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure", "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")
        ## Then assign a vector value for the column name that mathes what we would be passing in
        ## match returns a vector of the positions of (first) matches of its first argument in its second.
        ## therefore, we pass in our initial input, find what position it is in the validOutcome vector,  and that
        ## corresponds to the full column name position
        colName <- fullColName[match(outcome,validOutcome)]
        
        ## Return a data frame with the hospital names and the
        ## (abbreviated) state name
        
        hospital <- character(0) ## Initialize an empty vector to hold the hospital name
        
        ## start by building a loop that goes through for each state
        for (i in seq_along(validState)) {
                outcome.df.state <- outcome.df[outcome.df$State==validState[i],]  ## pull the state data
                
        ## then order it by the outcome first, hospital name second
                outcome.df.state.sorted <- outcome.df.state[order(as.numeric(outcome.df.state[[colName]]),outcome.df.state[["Hospital.Name"]],decreasing=FALSE,na.last=NA), ]

        ## initialize the result index
                maxrank <- nrow(outcome.df.state.sorted)
                
                if (num=="best") {ranking = 1}
                else if (num=="worst") {ranking <- maxrank}  ## set to the number of rows for the state
                
                ##print(c("state",validState[i],"maxrank=",maxrank,"ranking = ",ranking))
                ## insert the name of the hospital into the hospital vector at the appropriate position
                hospital[i] <- outcome.df.state.sorted[ranking,"Hospital.Name"]
        }
        
        ## finally, return a dataframe with the list of hospitals and the state abbreviations
        ## the text of the problem did not ask for the rows to be named with the state abbreviation
        ## but the result output did; hence the row.names being set
         data.frame(hospital=hospital,state=validState, row.names=validState)
}