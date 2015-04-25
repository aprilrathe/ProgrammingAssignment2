## Functions to support a custom matrix object that maintains a cache of its 
## inverse. For use in calculations requiring performance optimization for 
## large matrices, where repeated access to the inverse is required.

## Creates a custom matrix object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}

## Calculates the inverse of the custom matrix returned by makeCacheMatrix. 
## If the inverse has already been calculated and the matrix has not changed, 
## retrieves the inverse from the cache. Else, calculates the inverse using the
## solve function.
cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("retrieving cached inverse value")
        return(inv)
    }
    
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}