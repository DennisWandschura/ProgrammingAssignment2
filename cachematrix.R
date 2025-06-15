# This function creates a special "matrix" object that can cache its inverse
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


# Returns the inverse of the matrix
cacheSolve <- function(x, ...) {
  
  # first check if the inverse is already computed
    m <- x$getInverse()
    if(!is.null(m)) {
      # return from cache
      print("getting cached data")
      return(m)
    }
    
    # if not compute inverse, store and return
    matrixData <- x$get()
    m <- solve(matrixData, ...)
    x$setInverse(m)
    m
}
