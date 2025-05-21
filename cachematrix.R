## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse:

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # Initialize the cached inverse to NULL
  
  # Function to set a new matrix and reset the cached inverse
  set <- function(y) {
    x <<- y        # Store the new matrix in the parent environment
    inv <<- NULL   # Clear the previously cached inverse (if any)
  }
  
  # Function to get the current matrix
  get <- function() x
  
  # Function to set/cache the inverse of the matrix
  setinverse <- function(inverse) inv <<- inverse
  
  # Function to retrieve the cached inverse
  getinverse <- function() inv
  
  # Return a list of the four functions
  # This list acts like an object with methods to interact with the matrix and its cache
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



## This function computes (or retrieves) the inverse of the special "matrix" returned by makeCacheMatrix:

cacheSolve <- function(x, ...) {
  # Try to retrieve the cached inverse
  inv <- x$getinverse()
  
  # If the inverse is already cached, return it immediately
  if(!is.null(inv)) {
    message("getting cached data")  # Message to indicate cache is used
    return(inv)
  }
  
  # If no cached inverse, compute the inverse
  mat <- x$get()            # Get the matrix from the special object
  inv <- solve(mat, ...)    # Compute the inverse using R's solve() function
  
  # Cache the newly computed inverse for future use
  x$setinverse(inv)
  
  # Return the computed inverse
  inv
}

