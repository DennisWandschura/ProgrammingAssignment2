## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(inv) m <<- inv
  getInverse <- function() m
  list(set = set, get = get, 
       setInverse = setInverse,
       getInverse = getInverse)
}


## Returns the inverse of the matrix

cacheSolve <- function(x, ...) {
    m <- x$getInverse()
    if(!is.null(m)) {
      print("getting cached data")
      return(m)
    }
    matrixData <- x$get()
    m <- solve(matrixData)
    x$setInverse(m)
    m
}
