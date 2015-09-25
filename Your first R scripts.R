add2 <- function(x,y){
        x + y
}

above10 <- function(x) {
        use <- x > 10  ## check for values greater than 10
        x[use] ## then return a vector of values evaluated
}

above <- function(x,n = 10) {  ## the = 10 is setting a default value
        use <- x > n
        x[use]
}

columnmean <- function(y, removeNA = TRUE) {
        nc <- ncol(y) ## calculate the number of columns
        means <- numeric(nc) ## empty vector with the length = number of columns
        for(i in 1:nc) {
                means[i] <- mean(y[,i], na.rm = removeNA)
        }
        means ## returns the vector named means
}