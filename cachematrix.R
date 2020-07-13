## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates a matrix data structure with the capability to store the matrix's inverse, get and set various
## attributes of the matrix.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) i <<- inverse
  getInverse <- function() i
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve takes a makeCacheMatrix object, and checks for an existing inverse. If no inverse is cached, it creates a new one

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getInverse()
  if(!is.null(i)) {
    message("Getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setInverse(i)
  i
}