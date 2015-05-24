## A set of functions to cache the inverse of a matrix

## This function creates a special "matrix" object that can cache 
## its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) { ## setter for the matrix object
        x <<- y
        inv <<- NULL
    }
    get <- function() x ## getter for the matrix object
    setinv <- function(solve) inv <<- solve ## setter for the inverse
    getinv <- function() inv ## getter for the inverse
    list(set = set, get = get, 
         setinv = setinv, getinv = getinv)
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has 
## not changed) the inverse will be retrieved from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()      ## trying to obtain the inverse from cache  
    if(!is.null(inv)) {    ## return the inverse matrix if exists
        message("getting cached data")
        return(inv)
    }
    ## if not cached
    data <- x$get() ## getting the matrix object
    inv <- solve(data) ## calculating the invers
    x$setinv(inv) ## put the value into the cache
    inv ## return the inverse matrix
}
