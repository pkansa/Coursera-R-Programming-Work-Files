corr <- function(directory, threshold = 0) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        
        ## figure out which ones meet our criteria
        complete_cases <-complete(directory)
        complete_cases_sub <- subset(complete_cases, nobs>threshold)
        
        ## Generate the list of ids
        complete_cases_id <- complete_cases_sub$id
        ## get the length of ids vector
        complete_cases_id_len <- length(complete_cases_id)
        ## initialize the vector to store the correlation
        ## initialize it with a value of zero, repeating for each of the vector entries above
        corr_vector <- rep(0, complete_cases_id_len)
        # find all files in the specdata folder
        files_full <- list.files(directory,full.names=TRUE)
        x <- 1
        for(i in complete_cases_id) {
                dat <- read.csv(files_full[i])
                ## set the appropriate value in the correlation vector
                corr_vector[x] <- cor(dat$sulfate, dat$nitrate, use="complete.obs")
                x <- x + 1
        }
        corr_vector
        
        
        ## 'threshold' is a numeric vector of length 1 indicating the
        ## number of completely observed observations (on all
        ## variables) required to compute the correlation between
        ## nitrate and sulfate; the default is 0
        
        ## Return a numeric vector of correlations
        ## NOTE: Do not round the result!
}