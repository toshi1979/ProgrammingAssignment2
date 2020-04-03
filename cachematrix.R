## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a special "matrix" object that can cache its inverse.
## How it works:
## 1. initialize inv as NULL
## 2. $set defines matrix to be computed and revert inv to NULL 
## 3. $get returns matrix to be computed
## 4. $setSolve puts inverse value to inv
## 5. $getSolve returns inverse value

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setSolve <- function(inverse) inv <<- inverse
  getSolve <- function() inv
  list(set = set, get = get,
       setSolve = setSolve,
       getSolve = getSolve)
}

## Write a short comment describing this function
## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then cacheSolve should retrieve
## the inverse from the cache.
## How it works:
## 1. Get inverse value to identify if it was already computed.
## 2. Return inverse if it exists. If not, compute inverse of given matrix.
## 3. Set inverse value to the object for cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getSolve()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setSolve(inv)
  inv
}
