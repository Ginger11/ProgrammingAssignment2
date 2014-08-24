# The first function, makeVector creates a special "vector", which is really a list containing a function to:
set the value of the vector
get the value of the vector
set the value of the mean
get the value of the mean


makeVector <- function(x = numeric()) { # sets up function name, taking x
    m <- NULL # sets up empty m variable for cache - executed when run
    set <- function(y) { 
        print("I am Y:")
	  print(y)
        x <<- y
   	  m <<- NULL
    }


    get <- function() x # returns the value in x
    print("I am get")
    print(get)
    setmean <- function(mean) m <<- mean # places 
    getmean <- function() m

    list(set = set, get = get,
         setmean = setmean,
         getmean = getmean)
}















cachemean <- function(x, ...) {
    m <- x$getmean() # gets m 
    if(!is.null(m)) { # checks to see if any cached data
        message("getting cached data") 
        return(m) # prints cached data
    }
    data <- x$get()
    m <- mean(data, ...) # calculates mean and puts in m
    x$setmean(m) # puts m value into cached m
    m # prints m
}
