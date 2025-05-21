## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse:

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  # Set the matrix and reset the cached inverse
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # Get the matrix
  get <- function() x
  
  # Set the cached inverse
  setinverse <- function(inverse) inv <<- inverse
  
  # Get the cached inverse
  getinverse <- function() inv
  
  # Return a list of the functions
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



## This function computes (or retrieves) the inverse of the special "matrix" returned by makeCacheMatrix:

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  
  # Check if the inverse is already cached
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # Compute the inverse and cache it
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setinverse(inv)
  inv
}
