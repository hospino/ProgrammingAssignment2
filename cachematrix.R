
# makeCacheMatrix: return a list of functions
makeCacheMatrix <- function(x = matrix()) {
  
  # inv will store the cached inverse matrix
  inv <- NULL
  
  # Set the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # Get the matrix
  get <- function() x
  
  # Set the inverse
  setinv <- function(inverse) inv <<- inverse
  
  # Get the inverse
  getinv <- function() inv
  
  # Return  
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}

# cacheSolve: Compute the inverse of the matrix. If the inverse is cached before, it returns the cached inverse.
cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  
  # If the inverse is cached, return cached inverse
  if (!is.null(inv)) {
    message("USE CACHED DATA")
    return(inv)
  }
  
  # The inverse is not cached, calculate it
  data <- x$get()
  inv <- solve(data, ...)
  
  # Cache the inverse
  x$setinv(inv)
  
  # Return the inverse
  inv
}


# Example usage:
# > m <- matrix(rnorm(36), nrow = 6)          // Create a matrix m

# > cm <- makeCacheMatrix(m)                   
# > cacheSolve(cm)                            
# > cacheSolve(cm)                            