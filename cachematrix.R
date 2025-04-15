## These functions work together to cache the inverse of a matrix.
## This can save computation time by avoiding redundant calculations
## if the inverse of the same matrix is needed multiple times.

## This function creates a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL  # Reset the cache when matrix is set again
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
}
## This function computes the inverse of the special "matrix" object.
## If the inverse is already cached, it retrieves it to save computation.
cacheSolve <- function(x, ...) {
  m <- x$getinverse()  # Retrieve the cached inverse if it exists
  if(!is.null(m)) {
    message("getting cached data")
    return(m)  # Return the cached inverse
  }
  data <- x$get()  # Get the matrix data
  m <- solve(data, ...)  # Compute the inverse of the matrix
  x$setinverse(m)  # Cache the computed inverse
  m  # Return the inverse
}

