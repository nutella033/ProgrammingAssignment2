## These two functions are used to create a special object
## that stores a matrix and caches its inverse.

## This function creates the special matrix object as
## a list contatining functions to set the value of the matrix,
## get the value of the matrix, set the inverse of the matrix
## and get the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
    x <<- y
    i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function calculates the inverse of the special matrix
## created with makeCacheMatrix. If the inverse has already been
## calculated, it gets the inverse from the cache and skips
## the computation

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
