## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix caches a given matrix x in a new environment, 
## together with an inverse (initially set to NULL until cacheSolve is called). 
## The function returns a list of functions. "set" which sets the value of x;
## "get" which retrieves x; "setinverse" which caches the inverse of x;
## and "getinverse" which retrieves the cached inverse.


makeCacheMatrix <- function(x = matrix()) {
  makeCacheMatrix <- function(x=matrix()) {
    inverse <- NULL
    set <- function(y) {
      x <<- y
      inverse <<- NULL
    }
    
    get <- function() x
    setinverse<- function(inverse) inverse <<- solve(x)
    getinverse <- function() inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve returns the cached inverse of a given matrix if it is not NULL, and 
## otherwise returns the inverse of x while setting the cached inverse of x to the
## inverse of x.

cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached inverse")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data)
  x$setinverse(inverse)
  inverse
}

