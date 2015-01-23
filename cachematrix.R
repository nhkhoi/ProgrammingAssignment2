## Put comments here that give an overall description of what your
## functions do

## This function is to get and set the matrix, as well as its inversion
## The function is form by four sub functions:
## set: change the value of the original matrix
## get: get the value of the original matrix
## setInverse: cache the value of the inversion
## getInverse: get the value of the inversion

makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL
	set <- function(y) {
		x <<- y
		changed.track <<- NULL
	}

	get <- function() x
	setInverse <- function(inverseMatrix) inverse <<- inverseMatrix
	getInverse <- function() inverse
	list(set = set, get = get,
		setInverse = setInverse, 
		getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve retrieve the inverse from the cache.
getInverseMatrix <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        if(!is.null(m)) {
        	message("getting cached data")
        	return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m
}
