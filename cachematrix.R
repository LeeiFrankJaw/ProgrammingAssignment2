## The first function creates a cached matrix, whereas the second
## function operates on the output of the first function, i.e. the
## cached matrix, to calculate the inverse of the matrix.

## matrix -> cacheMatrix
## return a cacheMatrix of matrix x. A cacheMatrix is represented by a
## list of four functions: set, get, setInverse, and getInverse. set
## updates the matrix, get returns the matrix, setInverse updates
## the inverse, and getInverse returns the inverse.

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setInverse <- function(inverse) inv <<- inverse
	getInverse <- function() inv
	list(set = set, get = get, setInverse = setInverse,
		getInverse = getInverse)
}


## cacheMatrix -> matrix
## return the inverse of cacheMatrix x. If the inverse is already in the
## cacheMatrix, then just return it. If not, calculate the inverse, update
## the cacheMatrix and return the result.

cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
	inv <- x$getInverse()
	if (!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}
	data <- x$get()
	inv <- solve(data, ...)
	x$setInverse(inv)
	inv
}
