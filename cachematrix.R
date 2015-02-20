## These functions create a special "matrix" object that 
## can cache its inverse (makeCacheMatrix) and either 
## compute the inverse of the matrix or return the cached
## value (cacheSolve)



## This function creates a special "matrix" object that 
## can cache its inverse. It is actually a list containing
## functions to set or get the value of the vector (a matrix)
## and to cache the inverse fo the stored matrix or return
## the cached value if one is present.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}


## This function computes the inverse of the special 
## "matrix" returned by makeCacheMatrix. If the 
## inverse has already been calculated (and the matrix 
## has not changed), then cacheSolve retrieves the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
