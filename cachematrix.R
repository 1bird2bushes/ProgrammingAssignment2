## Author: 1bird2bushes
## Date: 06/21/2015
## 
## The functions within this file will take in an invertible matrix (square, nxn)
## and will cache it's inverse. Everytime the inverse is called it will pull it from
## the cache rather than recalculating

## makeCacheMatrix will take in a matrix and stores it as well as the inverse (initialized as NULL)
## in a global environment. The first time the function is fun will assign NULL to xInverse, since
## the inverse has not yet been calculated.

makeCacheMatrix <- function(x = matrix()) {
  xInverse <- NULL
  set <- function(y) {
    x <<- y
    xInverse <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) xInverse <<- inverse
  getInverse <- function() xInverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve will calculate and cache the inverse of a matrix. If the inverse has already been
## calculated, it would have been cached and therefore a call of the function will simply get the
## cached inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  xInverse <- x$getInverse()
  if(!is.null(xInverse)){
    message("Getting cached data")
    return(xInverse)
  }
  data <- x$get()
  xInverse <- solve(data)
  x$setInverse(xInverse)
  xInverse
}

## The following block of comments is a sample run of the functions in this file.
## I've also confirmed that initially there is not retrieval from cache, since NULL.


# > source("cachematrix.R")
# > m <- matrix(c(-1, -2, 1, 1), 2,2)
# > x <- makeCacheMatrix(m)
# > x$get()
# [,1] [,2]
# [1,]   -1    1
# [2,]   -2    1
# > inv <- cacheSolve(x)
# > inv
# [,1] [,2]
# [1,]    1   -1
# [2,]    2   -1
# > inv <- cacheSolve(x)
# Getting cached data
# > source("cachematrix.R")
# > m <- matrix(c(-1, -2, 1, 1), 2,2)
# > x <- makeCacheMatrix(m)
# > x$get()
# [,1] [,2]
# [1,]   -1    1
# [2,]   -2    1
# > inv <- cacheSolve(x)
# > inv
# [,1] [,2]
# [1,]    1   -1
# [2,]    2   -1
# > inv <- cacheSolve(x)
# Getting cached data
# > inv
# [,1] [,2]
# [1,]    1   -1
# [2,]    2   -1