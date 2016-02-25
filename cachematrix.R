## The following are a pair of functions that cache and compute the 
## inverse of a matrix.

## The first function creates a special "matrix" object
## and also it can cache its inverse.

makeCacheMatrix <- function(mt = matrix()) {
  inverse <- NULL
  set <- function(x) {
  mt <<- x;
  inverse <<- NULL;
  }
  get <- function() return(mt);
  setinv <- function(inv) inverse <<- inv;
  getinv <- function() return(inverse);
  return(list(set = set, get = get, setinv = setinv, getinv = getinv))
}


## The below function computes the inverse of the special
## "matrix" object returned by `makeCacheMatrix` above. If the inverse had
## been already calculated , then 'cacheSolve` function will return the inverse 
## from the cache.


cacheSolve <- function(mt, ...) {
  inverse <- mt$getinv()
  if(!is.null(inverse)) {
    message("Getting cached data...")
    return(inverse)
  }
  data <- mt$get()
  inverse <- solve(data, ...)
  mt$setinv(inverse)
  return(inverse)
}
