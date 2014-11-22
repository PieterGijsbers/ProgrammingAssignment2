## This script contains functions that allow for the inverse of a matrix to be cached,
## so it does not have to be computed more than once, which might be computationally expensive.

## makeCacheMatrix creates a cacheMatrix which allows the inverse of the matrix to be cached
## parameters:
## x: The matrix to create a cacheMatrix for
## returns:
## A list containing the following functions to access data from the cacheMatrix
## set (matrix) : caches the given matrix
## get () : returns the cached matrix
## setInverse (matrix) : caches the given inverse
## getInverse () : returns the cachedInverseMatrix
makeCacheMatrix <- function(myMatrix = matrix()) {
    matrixInverse <- NULL
    set <- function(newMatrix) {
        myMatrix <<- newMatrix
        inverse <<- NULL
    }
    get <- function() myMatrix
    setInverse <- function(newInverse) matrixInverse <<- newInverse
    getInverse <- function() matrixInverse
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## Will return the inverse of cacheMatrix x.
## If the inverse has not yet been computed, it will do so, otherwise it will use the cached inverse.
cacheSolve <- function(x, ...) {
    inverse <- x$getInverse()
    
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    
    myMatrix <- x$get()
    inverse <- solve(myMatrix)
    x$setInverse(inverse)
    
    inverse
}