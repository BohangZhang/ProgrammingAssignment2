## Matrix inversion is a costly computation and there may be some benefit to 
## caching the inverse of a matrix rather than compute it repeatedly. 
## The following two functions can create a special "matrix" and cache the inverse of it.

## makeCacheMatrix: This function creates a special "matrix" object (a list of four functions) that can cache its inverse.
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  invers <- NULL
  set <- function(y) {
    x <<- y
    invers <<- NULL
  }
  get <- function() x
  setinverse <- function(i) {
    invers <<- i
  }
  getinverse <- function() invers
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  invers <- x$getinverse()
  if (!is.null(invers)) {
    message("getting cached data")
    return(invers)
  }
  data <- x$get()
  invers <- solve(data)
  x$setinverse(invers)
  invers
}
