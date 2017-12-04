# Overall description of what the functions do.
# This pair of functions will first create a matrix object that can cache its inverse
# and then compute the inverse of the matrix. If the matrix has not been changed and
# the inverse matrix already calculated the second function will retreive the
# inverse from the cache.

# Below is an example of a square invertible matrix to test.
# mtx <- matrix(rnorm(100), 10, 10)

# An example of how to run:
# mx <- makecachematrix(mtx)
# cacheSolve(mx)

# A short comment describing this function:
# This function will return of list funtions to: set the matrix, get the matrix,
# set the inverse matrix, and the get the inverse matrix.
# Note: calling makecachematrix() will set the matrix the first time but to replace/rest with
# another matrix use set()

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function()
    x
  setmat <- function(inverse)
    m <<- inverse
  getmat <- function()
    m
  list(
    set = set,
    get = get,
    setmat = setmat,
    getmat = getmat
  )
}


# A short comment describing this function:
# This function will compute the inverse of the matrix provided from makecachematrix().
# In the event that the inverse matrix has already been computed, it will return
# the cached inverse matrix.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getmat()
  if (!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setmat(m)
  m
}
