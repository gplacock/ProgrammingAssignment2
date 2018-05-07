## Johns Hopkins / Coursera - Programming in R
## Week 3 assignment - Paul Lacock - 7 May 2018
##
## Two functions to demonstrate a technique for
## cacheing results of potentially resource intensive
## operations, making use of the scoping rules in R.

## Function 'makeCacheMatrix' creates a 'matrix' object which
## includes a list of functions which can be 
## used to cache results produced by the calling function, and to 
## return these results to the calling function (if already
## cached). Note that in spite of the use of the word 'inverse' 
## in the naming of the sub-functions, this function can be used 
## more generically by any calling function of a similar form to 
## 'cacheSolve' which operates on a matrix.

makeCacheMatrix <- function(x = matrix()) {
    ## 'x' is a matrix and must be invertible otherwise function
    ## 'cacheSolve' will return an error.
    
    m <- NULL
    
    ## Function 'set' allows user to set new matrix data
    ## (and set cached result 'm' to NULL)
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    ## Function 'get' returns current matrix data
    get <- function() x
    
    ## Function 'setInverse' caches the operation result
    ## ('inverse') to 'm'.
    setInverse <- function(inverse) m <<- inverse
    
    ## Function 'getInverse' returns the cached result in 'm'
    ## ('m' will be NULL if the inverse results for matrix x
    ## hasn't already been calculated and cached)
    getInverse <- function() m
    
    ## Return the list of functions for cacheing:
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## Function 'cacheSolve' returns a matrix which is the inverse
## of the matrix passed to function makeCacheMatrix. If the inverse
## for the current matrix has already been calculate and cached, the
## result is returned from the cache. Otherwise the result is
## calculated, and cached.

cacheSolve <- function(x) {
    ## Argument 'x' is a function of type makeCacheMatrix.
    m <- x$getInverse()     ## Get the current value in the cache.
    if(!is.null(m)) {       ## If this is not NULL ...
        message("getting cached data")  ## let user know
        return(m)                       ## and return this value, 
    }
    data <- x$get()         ## otherwise get current matrix,
    m <- solve(data)   ## and calculate its inverse
    x$setInverse(m)         ## and cache this using $setInverse.

    m
}
