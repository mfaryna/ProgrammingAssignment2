## Following pair of functions compute and cache the inverse of a matrix
## Assumption was made, that matrix passed to those functions is invertible

## Function makeCacheMatrix creates a list containing a set of functions to the following actions (1-4 below):
makeCacheMatrix <- function(X = matrix()) {
  M <- NULL                                                             ## Cleans object M
  set <- function(Y) {                                                  ## 1) Sets the value of the matrix. Function taking one argument (matrix)
    X <<- Y                                                                 ## Assigns values of matrix 'Y' to matrix 'X' (in an environment different from the current environment)
    M <<- NULL                                                              ## Assigns NULL to matrix 'M' (in an environment different from the current environment)
  }

  get <- function() X                                                   ## 2) Gets the value of the matrix. Function without arguments, returning matrix 'X'
  setInverse <- function(solve) M <<- solve                             ## 3) Sets given matrix to matrix 'M'. Function that assigns its argument to 'M'
                                                                        ##    (in an environment different from the current environment)
  getInverse <- function() M                                            ## 4) Gets the value of the inverse matrix. Function without arguments, returning matrix 'M'
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)  ## Definition of the list of functions returned by the function makeCacheMatrix.
}

## Function that either takes an inverse matrix from cache or (if there's nothing in cache) computes one
cacheSolve <- function(X, ...) {
  M <- X$getInverse()                 ## Calling function getInverse on matrix X and assigning a result to an object M.
  if(!is.null(M)) {                   ## If there's an inverse in cache, 
    message("getting cached data")    ## printing a message that values were taken from cache
    return(M)                         ## and returning inverse matrix M from cache.
  }
  message("computing inverse matrix") ## Printing a message that values are being computed.
  data <- X$get()                     ## Calling function 'get' on matrix 'X' and assigning a result to matrix 'data'.
  M <- solve(data, ...)               ## Computing inverse matrix of 'data' and assigning a result to matrix 'M'.
  X$setInverse(M)                     ## calling function 'setInverse' on matrix 'X' and assigning a result to matrix 'M'                                     
                                      ## <-> caches computed inverse matrix M, next time we call cacheSolve(makeCacheMatrix(X)) we'll get value from cache, assuming 'X' didn't change
  M
}

## How to use those functions:
## 1) source("cachematrix.R"). File cachematrix.R should be in your working directory.
## 2) define matrix X, eg. x<-matrix(c(2,2,3,2),2,2)
## 3) create a list of functions on matrix X, eg. y<-makeCacheMatrix(x)
## 4) compute inverse or take one from cache, eg. cacheSolve(y)
## Writing it all in one line, eg. cacheSolve(makeCacheMatrix(matrix(c(2,2,3,2),2,2))) will always result in computing inverse matrix (not taking from cache).