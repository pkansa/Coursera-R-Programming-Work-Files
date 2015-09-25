rankhospital <- function(state, outcome, num = "best") {
        ## Read outcome data
        outcome.df <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        ## Check that state and outcome are valid
        ## check the outcome first
        validOutcome = c("heart attack","heart failure","pneumonia") ## concatenate the three outcomes we'll be checking for
        if (!outcome %in% validOutcome) { stop("Invalid outcome entered; check inputs")} ## if the input is not valid, stop the function
        
        ## then check the state
        validState = unique(outcome.df[,7]) ## get a list of the unique state values
        if (!state %in% validState) stop("Invalid state entered; check inputs") ## if the input is not valid, stop the function
        
        ## Return hospital name in that state with the given rank 30-day death rate
        ## Build a vector that contains the three column names we would be searching through
        fullColName <- c("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure", "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")
        ## Then assign a vector value for the column name that mathes what we would be passing in
        ## match returns a vector of the positions of (first) matches of its first argument in its second.
        ## therefore, we pass in our initial input, find what position it is in the validOutcome vector,  and that
        ## corresponds to the full column name position
        colName <- fullColName[match(outcome,validOutcome)]
        
        ## create another dataframe that limits things to the state we are looking for
        outcome.df.state <- outcome.df[outcome.df$State==state,]
        
        ## next, order the data by outcome, eliminating the NA values
        ## decreasing=FALSE means it is sorted in ascending order
        ## na.last = NA means NA values are eliminated
        ## it is first ordered by the outcome, and then by the hospital name to break ties
        outcome.df.state.sorted <- outcome.df.state[order(as.numeric(outcome.df.state[[colName]]),outcome.df.state[["Hospital.Name"]],decreasing=FALSE,na.last=NA), ]
        
        ## now we need to check what ranking we are running things for
        maxrank <- nrow(outcome.df.state.sorted)
        
        if (num=="best") {num = 1}
        if (num=='worst') {num = maxrank}  ## set to the number of rows for the state
        
        ## check if an invalid ranking was entered; if so, return an NA, otherwise the result
        if (num > maxrank) {              
                print(NA)
       } else {
             outcome.df.state.sorted[num,"Hospital.Name"]
           }
}