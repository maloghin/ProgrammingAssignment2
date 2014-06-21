## This function creates a special "matrix" object that can cache its inverse
## The function returns a liste containing
## set the matrix
## get the matrix
## set the inverse of the matrix
## get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  setmx <- function(y) {
    x <<- y
    m <<- NULL
  }
  getmx <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(setmx = setmx, getmx = getmx,
       setsolve = setsolve,
       getsolve = getsolve)
  
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$getmx()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
