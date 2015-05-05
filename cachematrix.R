# Assignment 2: Caching the Inverse of a Matrix

# Make a special list, which encapsulate a regular matrix, providing
# get/set/setinv/getinv function
makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y)
	{
		x <<- y
		# whenever setting a matrix, empty the cached inverse matrix
		inv <<- NULL
	}
	get <- function() x

	setinv <- function(i) inv <<- i
	getinv <- function() inv

	list(set = set, get = get, setinv = setinv, getinv = getinv)
}

# Encapsulate the solve function, make use of the cached matrix
cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv))
    {
    	# using the cached inverse matrix
    	return(inv)
    }
    # otherwise we have to calculate the inverse matrix
    mat <- x$get()
    inv <- solve(mat, ...)
    # and cached it
    x$setinv(inv)
    inv
}
