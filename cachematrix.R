## Functions for speeding up repeated matrix inversions.
##
## 'makeCacheMatrix' creates a special matrix object that holds the
## original matrix and its cached inverse, plus a set of functions
## to access them. Use 'cacheSolve' to calculate the inverse of the
## special cache matrix object.
##
## Example: cm <- makeCacheMatrix(matrix(1:4, nrow=2, ncol=2))
##          cacheSolve(cm)
##
## The original matrix is assumed to be invertibe.


## Creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        ## Cached inverse initially empty
        inv <- NULL
        
        ## Accessor functions
        getmatrix <- function() x
        setmatrix <- function(m) {
                inv <<- NULL
                x <<- m
        }
        getinverse <- function() inv
        setinverse <- function(xi) inv <<- xi
        
        ## Return special object as a list consisting of the above
        ## functions that have access to the original matrix 'x'
        ## and its cached inverse 'inv' recorded in the current evironment
        list(getmatrix = getmatrix, setmatrix = setmatrix,
             getinverse = getinverse, setinverse = setinverse)
}


## Computes the inverse of the special matrix returned by 'makeCacheMatrix'.
## Any optional parameters following 'x' transmitted to R's 'solve' function
## as is.

cacheSolve <- function(x, ...) {
        ## Check if the inverse was previously calculated already and cached
        inv <- x$getinverse()
        if (!is.null(inv)) {
                message("getting cached inverse")
                return(inv)
        }
        
        ## Cache empty, so calculate the inverse and store the result in cache
        inv <- solve(x$getmatrix(), ...)
        x$setinverse(inv)
        inv
}
