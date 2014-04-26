## The two functions below create an object which stores a matrix and caches its inverse.


## the first function "makeCacheMatrix" 
## 1. it takes an argument x of type matrix
## 2. it returns a list  of 4 functions: set, get, setinverse, and getinverse

makeCacheMatrix <- function(x = matrix()) {

  inverse <- NULL
  set <- function(y){
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inverse <<- solve
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The second function "cacheMean" uses "makeVector" function in its implementation.
## The input is matrix made from makeCacheMatrix.
## The output is the inverse coming either from the special vector's  cache or computation.

cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()            # query the x matrix's cache
  if(!is.null(inverse)) {              # if there is a cache
    message("getting cached data")     # just return the cache, no computation needed
    return(inverse)
  }
  data <- x$get()                      # if there's no cache
  inverse <- solve(data, ...)          # we actually compute them here
  x$setinverse(inverse)                # save the result back to x's cache
#  message("display the inverse")
  inverse                              # return the inverse
  
}



cachemean <- function(x, ...) {
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  message("display the mean")
  m
}


## For testing the functions: 
## x <- matrix(1:4,2,2)
## x
## cacheSolve(makeCacheMatrix(x)) %*% x # is the identity matrix

