## The pair of functions below can be used to cache the inverse of a matrix

## The makeCacheMatrix function is used to create a "special" matrix that can cache its inverse. 
## The following inner functions are available:
##  set: set the value of the matrix
##  get: get the value of the matrix
##  setInverse: set the value of the inverse
##  getInverse: get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  
  storedInverse <- NULL
  
  set <- function(y) 
  {
    x <<- y
    storedInverse <<- NULL
  }
  
  get <- function() 
  {
    x
  }
  
  setInverse <- function(inverse)
  {
    storedInverse <<- inverse
  }
  
  getInverse <- function() 
  {
    storedInverse
  }
  
  list(set = set, 
       get = get,
       setInverse = setInverse,
       getInverse = getInverse
       )
}


## The cacheSolve function is used to compute the inverse of the "special" matrix. 
## If the inverse is available in cache it is returned instead of computing the inverse again.

cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  
  if(!is.null(m)) 
  {
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}
