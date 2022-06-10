## Put comments here that give an overall description of what your
## functions do


makeCacheMatrix <- function(x = matrix()) {
  ## Create a special matrix object with functions to set/get its data, and to
  ## cache/get its inverse
  inverse <- NULL
  
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  
  get <- function() x
  
  setInverse <- function(inv) {
    inverse <<- inv
  }
  
  getInverse <- function() inverse
  
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x', either by getting the cached
  ## inverse or calculating it
  inverse <- x$getInverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setInverse(inverse)
  inverse
}
