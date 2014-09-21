## Functions makeCacheMatrix and cacheSolve(x) allow to save computational
## resources when taking an inverse of a matrix by:
## 1. making a cached inverse and
## 2. using the cached inverse instead of computing it again.

## The command for using the functions: cacheSolve(makeCacheMatrix(x)) 
## where 'x' is the matrix to be inversed.


## makeCacheMatrix is a function that allows to create, store
## and recall a cached matrix inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL   ## set variable 'm' equal to NULL
  set <- function(y){
    x <<- y   ## set x to y
    m <<- NULL ## set m to NULL
  }
  get <- function() x   ## get returns value of 'x'
  setsolve <- function(solve) m <<- solve   ## set 'm' equal to solve
  getsolve <- function() m   ## return value of 'm'
  list(set = set, get = get, ## return a labled matrix of functions set, get ...
       setsolve =setsolve,
       getsolve = getsolve)
}


## CacheSolve is a function that returns matrix inverse of 'x' from the cache.

cacheSolve <- function(x, ...) {
  ## return a matrix that is the inverse of 'x'
  m <- x$getsolve()   ## attempt to assign 'm' the value of cached inverse if 
  ## it exists
  if(!is.null(m)){    ## check if cashed inverse exists
    message("getting cached data")   ##message when cached inverse exists
    return (m)   ##return cached inverse
  }
  ## if no cached inverse exists:
  data <- x$get()   ## get data for 'x' from makeCacheMatrix
  m <- solve(data, ...)   ## calculate the inverse of 'x'; assign the value to 'm'
  x$setsolve(m)   ## set cached value in makeCacheMatrix equal to calculated 'm'
  m    ## returns a matrix inverse
}