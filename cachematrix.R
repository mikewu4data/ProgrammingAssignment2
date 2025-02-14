## Put comments here that give an overall description of what your
## functions do

##  There are two functions:
##  1. makeCacheMatrix - function to create an object and cache the inverse
##  2. cacheSolve - function that will calculate the inverse on a matrix only 
##  if there isn't one already in the cache.

##  Function to create an inverse of a matrix and store it
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <-function() {
        x
    }
    setinv <- function(inv) {
        i <<- inv
    }
    getinv <- function() {
        inv
    }
    list(set=set, get=get,
         setinv=setinv,
         getinv=getinv)
}

##  Function to check for the cached inverted matrix and if not found then
##  creates the inverse of a matrix
cacheSolve  <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
