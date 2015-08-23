## The functions makeCacheMatrix and cacheSolve create a square invertible 
## matrix as well as to create the inverse of the matrix in cache environment

## This function creates and returns a list containing functions to 
## set the matrix, get the matrix, set the inverse and get the inverse
## to use as the input to cacheSolve().

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## This function calculates inverse of the original matrix input to 
## makeCacheMatrix().

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}
