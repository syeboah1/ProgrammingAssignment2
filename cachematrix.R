## Put comments here that give an overall description of what your
## functions do

## This function makes a cache of a matrix and its inverse!
## It sets matrix value, gets matrix value, sets value of inverse, and gets value
## of inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
          x <<- y
          inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## this function gets the inverse of the matrix with the help of the above
## function. If we already have the inverse, it gets it from the cache, if not 
# it calculates it and sets it to the cache
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
  }

