## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.


## makeCacheMatrix: caches the inverse for a matrix
## get/set the matrix
## get/set the inversem

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  
  setInverse <- function(inverse) inv <<- inverse
  
  getInverse <- function() inv
  
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## calculates the inverse of the matrix, if it is calculated before, it returns
## the cached value
## no error checking if the matrix is invertible

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(is.null(inv)) {
    message("calculating inverse")
    m <- x$get()
    inv <- solve(m)
    x$setInverse(inv)
  }
  inv
}
