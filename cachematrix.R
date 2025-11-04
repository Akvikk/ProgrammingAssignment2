## makeCacheMatrix: creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL   # this will store the inverse later

  # Function to set a new matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL   # reset inverse when a new matrix is set
  }

  # Function to get the matrix
  get <- function() x

  # Function to store the inverse
  setinverse <- function(inverse) inv <<- inverse

  # Function to get the cached inverse
  getinverse <- function() inv

  # Return all 4 functions as a list
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve: computes the inverse of the matrix or retrieves it from cache
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }

  mat <- x$get()
  inv <- solve(mat, ...)
  x$setinverse(inv)
  inv
}
