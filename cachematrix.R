## Usually, Matrix inversion can be a complex and costly computation.
## For this reason, most people would benefit from caching the inverse of
## a matrix rather than computing it repeatedly. The functions below are 
## used to create a special object that stores a matrix and caches its inverse.
## functions do

## This function creates an object (matrix) that can (and will) cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set,
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This next function computes the inverse of the matrix created above by
## the makeCacheMatrix function. If the inverse has already been calculated, 
## then it should retrieve the inverse from the cache. Otherwise, it will calculate it.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setinverse(inv)
        inv
}
