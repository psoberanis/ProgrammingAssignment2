## This function is designed to allow a user to compute the inverse
## of a square matrix. In situations where the inverse needs to be computed
## over and over, the inverse is loaded from a cached location vs
## doing the computation again. This can potentially save the user
## computation time when computing the inverse is expensive.

## The makeCacheMatrix function creates a special list that contains the 
## square matrix along with the functions to retrieve the data.
## These functions are:
##      set - used to set the values of the matrix
##      setInverse - used the set the value of the inverse of the matrix
##      get - used to retrieve the matrix
##      getInverse - used to retrieve the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(n){
		x <<- n
		m <<- NULL
	}
	get <- function() x
	setInverse <- function(solve) m <<- solve
	getInverse <- function() m
	list(set=set, get=get, setInverse=setInverse, getInverse= getInverse)
}


## This function would be the function that would be used in a loop
## setting to compute the inverse of the matrix. The initial call 
## checks to see if the inverse of the matrix has already been computed.
## If not, it computes the inverse and caches the value.
## If the inverse is already computed, it notifies the user that cached 
## data will be used instead of computing the inverse of the matrix again.

cacheSolve <- function(x, ...) {
	m <- x$getInverse()
	if(!is.null(m)){
		message("getting cached data")
		return(m)
	}
	data <- x$get()
	m <- solve(data)
	x$setInverse(m)
	m
        ## Return a matrix that is the inverse of 'x'
}
