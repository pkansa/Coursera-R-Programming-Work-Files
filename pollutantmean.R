pollutantmean <- function(directory, pollutant, id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        ##print(directory)
        files_full <- list.files(directory,full.names=TRUE)
        dat <- data.frame() ## create an empty data frame to read in to
        ##print(files_full)
       for (i in id){
                       ## print(i)
                        dat <- rbind(dat, read.csv(files_full[i])) ## read in all of the files necessary and merge into a single data frame
        }

        mean(dat[,pollutant],na.rm=TRUE)
        ## 'pollutant' is a character vector of length 1 indicating
        ## the name of the pollutant for which we will calculate the
        ## mean; either "sulfate" or "nitrate".
        
        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used
        
        ## Return the mean of the pollutant across all monitors list
        ## in the 'id' vector (ignoring NA values)
        ## NOTE: Do not round the result!
}