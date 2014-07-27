## Programming Assignment -2
## Two functions:
## 1. makeCacheMatrix creates a special matrix object that can cache its inverse
## 2. cacheSolve checks if the inverse has already been computed and if so, returns
##    it from the cache. Otherwise, it computes the inverse and returns it (besides
##    caching it)

## Computes the inverse of the input matrix x.
## Provides the interface : get, getinv, set and setinv
## Uses 'solve' to compute the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function (y) {
		x <<- y
		inv <<- NULL
	}
	get <- function () x
	# solve is the R function to compute matrix inverse
	setinv <- function(solve) inv <<- solve
	getinv <- function () inv
	list (set = set, get = get, setinv = setinv, getinv = getinv)
}


## Caching the computed inverse so that it can be retrieved
## during future computations (if the input matrix hasn't changed)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inv <- x$getinv()
	if(!is.null(inv)) {
		message ("getting cached inverse...")
		return(inv)
	}
	data <- x$get()
	inv <- solve(data, ...)
	x$setinv(inv)
	inv
}
