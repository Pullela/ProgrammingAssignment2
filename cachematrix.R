## This function calculates the inverse of a matrix. If the same matrix is 
## is already used to compute the inverse previously, it get the inverse value
## directly from the cache without computing again this time.


## This function creates a special "matrix" object that can cache its inverse.


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get,setinv = setinv, getinv = getinv)
}

## This function computes the inverse of the special "matrix" returned by 
##makeCacheMatrix above. If the inverse has already been calculated (and the 
##matrix has not changed), then the cachesolve should retrieve the inverse from 
##the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  
  if(!is.null(m)) {
    
    message("getting cached data")
    return(m)
  }  
  
  y <- x$get()
  m <- solve(y, ...)
  x$setinv(m)
  m
  
  