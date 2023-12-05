## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  # Initialize the matrix
  mat <- x

  # Initialize a cached value for the inverse
  cache_inverse <- NULL

  # Define a function to set the matrix
  set <- function(matrix) {
    mat <<- matrix
    # When the matrix is set, invalidate the cached inverse
    cache_inverse <<- NULL
  }

  # Define a function to get the matrix
  get <- function() mat

  # Define a function to get the cached inverse or calculate it if not present
  getInverse <- function() {
    if (!is.null(cache_inverse)) {
      message("Getting cached inverse.")
      return(cache_inverse)
    } else {
      message("Calculating inverse.")
      cache_inverse <<- solve(mat)
      return(cache_inverse)
    }
  }

  # Return a list of functions
  list(set = set, get = get, getInverse = getInverse)
}



## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  # Check if the input is a valid cacheable matrix
  if (!inherits(x, "makeCacheMatrix")) {
    stop("Input must be a cacheable matrix created by makeCacheMatrix.")
  }

  # Get the cached inverse or calculate it
  inverse <- x$getInverse()

  # Return the inverse matrix
  return(inverse)
}

