## Caching the Inverse of a Matrix:
## Some computations in programming languages such as R are time consuming and use
## a lot of computing power.  Caching makes it so once a chunk of code
## is run once, it doesn't have to be run again.  This saves time and makes
## the code more efficient.
## This code creates a matrix and stores it as an object
## then caches its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Since the code above created the matrix object.  We need
## to find its inverse.  The following code does this, but
## if the inverse has already been calculated, the value
## is retrieved instead of recalculated.

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting data from cache")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
