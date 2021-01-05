
## Matrix inversion is usually a costly computation and there may be 
# some benefit to caching the inverse of a matrix rather than compute it repeatedly

## makeCacheMatrix:This function creates a special "matrix" object 
# that can cache its inverse


## makeCacheMatrix is a first class function (passes functions as arguments to
# other functions).  The output of makeCacheMatrix is a list of functions to 
# perform the following:
#   1. set the value of the matrix
#   2. get the value of the matrix
#   3. set the inverse of the matrix
#   4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
# If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache .

cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
  
  ## Returns a matrix that is the inverse of 'x'
}
