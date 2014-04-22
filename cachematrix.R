######################################################################
## Week 3 programming assignment 2

## The first function creates an object which includes several
## functions to cache the results of a (potentially) resource-
## hungry function, so it only has to be run once.

## makeCacheMatrix will store the inverse of a matrix for later use.
## The object returned will be used as input to cacheSolve

makeCacheMatrix <- function(x = matrix()) {
    # m is initialized to NULL in this environment
    m <- NULL
    # the set() function sets the value of x in the super-environment
    # to the input value y and sets m in the super-environment to NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    # the get() function returns the value of x
    get <- function() x
    # the setInverse() function solves a matrix for its inverse
    setInverse <- function(solve) m <<- solve
    # the getInverse() function retrieves the cached solution
    getInverse <- function() m
	# This builds the list of all of the pieces needed for cacheSolve
    list(set = set, get = get,
         setInverse = setInverse, getInverse = getInverse)
}

## cacheSolve returns the inverse of a matrix if the inverse is
## already cached, otherwise it computes the inverse and caches
## the result

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    # This line seeks a cached inverse. If it does not
	# exist, m will receive the default value of NULL
    m <- x$getInverse()
    # If an inverse was found (i.e. m not = NULL), return the cached value
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    # If m was returned as NULL
    message("did not use the cache")
    # get the input matrix
    data <- x$get()
    # solve the matrix for the inverse    
    m <- solve(data, ...)
    # set the inverse in the super-environment to the solution
    x$setInverse(m)
    # exit by evaluating the variable m    
    m
}
