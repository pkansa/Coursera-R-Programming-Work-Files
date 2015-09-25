complete <- function(directory, id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        files_full <- list.files(directory,full.names=TRUE)
        dat <- data.frame() ## create an empty data frame to read in to
        for (i in id){
                complete_cases <- sum(complete.cases(read.csv(files_full[i]))) ## read in all of the files necessary and merge into a single data frame
                x <- c(i,complete_cases)
                dat <- rbind(dat,x)
        }
        
        names(dat) <- c("id", "nobs")
        dat
        
        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used
        
        ## Return a data frame of the form:
        ## id nobs
        ## 1  117
        ## 2  1041
        ## ...
        ## where 'id' is the monitor ID number and 'nobs' is the
        ## number of complete cases
}