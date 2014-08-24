## There are two main functions - makeCacheMatrix takes a matrix, and sets up methods within it for use by
## the function cacheSolve. makeCacheMatrix puts a null value in variable m to start. It defines a list of 
## methods for use by external functions. These are in the comments in the code next to the relevant function (method).
## The second function (cacheSolve) takes the output from makeCacheMatrix as its input. If there's already a value in the 
## cache store (m) - i.e. not null - it prints this (i.e. it's used it as a cache). IF not, it gets the data with x$get(),
## uses solve (if it can) to invert it, which it then assigns to m. It then passes this to x$getinvert() method call, 
## which superassigns it to m, i.e. it is cached for future usage. 


## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
# takes a matrix as input to function. It also defines a series of methods (functions) for calling by cacheSolve later.
# Assuming matrix is invertible as per instructions, so no requirement to test for this. 
 
	m <- NULL

	set <- function(y) {
		x <<- y
		m <<- NULL
	}

	get <- function(){    # returns the matrix
		 x
      }
	invertmatrix <- function(solved) m <<- solved   # takes the inverted matrix and caches it with the superassignment operator.
	getinvert <- function() m  # returns the cached matrix.

	list(set = set, get = get, invertmatrix = invertmatrix, getinvert = getinvert)



}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x', using the methods supplied in the makeCacheMatrix object.
	  ## Applies solve() function to matrix data (if not already cached) to produce inverted matrix.

	  m <- x$getinvert()
        if(!is.null(m)) {   
           message("getting inverted matrix")
           return(m) 
        }
        data <- x$get()
        m <- solve(data, ...) 
	  x$invertmatrix(m) 
	  m
}
