## The inverse of a matrix is cached which saves computation time when the inverse of a matrix
## is needed repeatedly (eg in a loop).
## the makeCacheMatrix function creates an object in which a matrix and its inverse can be hold.
## cacheSolve function returns the inverse of a matrix.

## The function makeCacheMatrix() creates a special "matrix" object which has the following functions:
## set : sets the value of the matrix
## get : gets the value of the matrix
## setinverse : sets the inverse of the matrix
## getinverse : gets the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) i <<- solve
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}


## the function cacheSolve() resturns the inverse matrix of a matrix created by the function
## makeCacheMatrix(). It calculates the inverse in case the inverse is not calculated before.
## If the inverse of the matrix is calculated before, it returns the inverse from cache.

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
