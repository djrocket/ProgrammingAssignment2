## the following two functions invert an arbitrary (invertible) matrix
## and store the results for subsequent use to avoid re-calculation

## this function initializes a matrix object that can be used for
## caching and retrieving the results on the inverse operation

makeCacheMatrix <- function(x = matrix()) {
  # initialize variable m, the cache, to a null value
  m <- NULL
  # this function sets variable x in the parent environment to
  # a specific matrix passed on as a parameter, and variable m to null value
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  #this function retrieves matrix stored in variable x
  get <- function() x
  #this function caches the inverse in variable m in the parent environment
  setinverse <- function(inverse) m <<- inverse
  #this function retrieves the value stored in cache
  getinverse <- function() m
  # return a list of functions that can be referenced by name as subfunctions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## This function retrieves cached data, and if the cache is emptry,
## it calculates inverse, returns the result, and stores it in cache

cacheSolve <- function(x, ...) {
  # check cache for data and return the value, if present
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  # calculate inverse ofa given matrix, store result in cache and return the value 
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
  
}
