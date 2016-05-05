## The following function is to set and get the matrix and setInverse and getInverse of the matrix given by user.

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setInverse <- function(inverse) inv <<- inverse
	getInverse <- function() inv
	list(set = set, get = get, setInverse = setInverse , getInverse = getInverse)

}


## This function calculates the inverse of the special matrix returned by the above function.
## If there is already cached inverse, then it will not calculate, else it will calculate and cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
	inv <- x$getInverse()
	if(!is.null(inv)) {
		message("getting cached matrix.")
		return(inv)
	}
	data <- x$get()
	inv <- solve(data)
	x$setInverse(inv)
	inv
}