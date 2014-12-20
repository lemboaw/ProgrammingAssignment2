## These functions are meant to be used in combination with each other

## makeCacheMatrix creates a special matrix object which can cache its inverse
## It is meant to be used in conjunction with cacheSolve
makeCacheMatrix <- function(x = matrix()) {
  # If inverse is NULL, the inverse must be computed
  i <- NULL
  
  # Function to manually change the matrix
  # the inverse must be recomputed once changed using set
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  # Get function returns the matrix
  get <- function() x
  
  # Function to set the inverse matrix x
  setinverse <- function(inverse) i <<- inverse
  
  # Function to return the inverse matrix of x
  getinverse <- function() i
  
  # Return the list of functions that can be called on this object
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve takes only a square invertable matrix 
## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve will retrieve 
## the inverse from the cache.
cacheSolve <- function(x, ...) {
  
  # Pull the inverse value from the CacheMatrix object that is passed
  i <- x$getinverse()
  
  # If the value is not NULL, return the cached value
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
    
  } else { # If the Value is NULL, recompute the inverse and cache it
    
    # Get the matrix from the passed object
    data <- x$get()
    
    # Compute, set, and return the inverse
    i <- solve(data, ...)
    x$setinverse(i)
    return(i)
  }
  
}
