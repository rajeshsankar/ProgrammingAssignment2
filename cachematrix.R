## makeCacheMatrix and cacheSolve functions computes the inverse
## of a matrix object and caches the result. If the inverse of the 
## matrix object is found in the cache, the cached object is returned,
## instead of computing the inverse

## makeCacheMatrix function will take a matrix as a parameter.
## It contains methods to:
##     - set the value of the matrix
##     - get the value of the matrix
##     - set the inverse of the matrix
##     - get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {

    mtrx <- NULL
    set <- function(y) {
        x <<- y
        mtrx <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) mtrx <<- inverse
    getinv <- function() mtrx
    list(set = set, 
         get = get,
         setinv = setinv,
         getinv = getinv)

}


## cacheSolve function computes the inverse of the matrix created by
## makeCacheMatrix function. It first checks if the inverse has already been
## calculated. If calculated, it gets the value from the cache and returns 
## the value, thereby skipping the calculation. If the inverse is not found in
## cache, this method will compute the inverse of the matrix and puts the
## value in the cache

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    invmtrx <- x$getinv()
    if(!is.null(invmtrx)) {
        message("getting cached data")
        return(invmtrx)
    }
    data <- x$get()
    invmtrx <- solve(data)
    x$setinv(invmtrx)
    invmtrx
}
