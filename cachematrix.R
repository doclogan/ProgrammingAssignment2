## This pair of functions computes the inverse of an arbitrary square matrix.
## Importantly it caches the results of the computation so that recomputation is not
## necessary if the results are in the cache.


## The makeCacheMatrix function provide a list of 4 helper functions:
## set -- stores the matrix and sets the solution to null
## get -- returns the matrix
## setsolve_m -- stores the solution i.e. the inverse of the matrix
## getsolve_m -- returns the solution

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve_m <- function(solve_m) m <<- solve_m
  getsolve_m <- function() m
  list(set = set, get = get,
       setsolve_m = setsolve_m,
       getsolve_m = getsolve_m)
}


## The cacheSolve function claculates the inverse of the matrix.
## It first checks if the inverse was previously calculated and only does the
## calculation if that condition is not satisfied.
## The function makes extensive use of the helper functions in makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve_m()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve_m(m)
  m
}