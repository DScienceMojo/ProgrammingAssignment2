## These functions are written to  be able to cache the inverse of a matrix
## which is computationally time-consuming operation. Once the inverse is computed
## it will be stored and read from the cache instead of recomputing the inverse.

## This function creates a list containing a function to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  matrixInverse <- NULL
  set <- function(y) {
    x <<- y
    matrixInverse <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) matrixInverse <<- solve
  getinverse <- function() matrixInverse
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## This function returns the inverse of a matrix 'x'.
## It checks to see if the inverse of the matrix has previously been computed. If not,
## it will compute the inverse and store it in the cache. If yes, then it will
## return the cached inverse matrix instead of recomputing it.
cacheSolve <- function(x, ...) {
  matrixInverse <- x$getinverse()
  if(!is.null(matrixInverse)){
    message("getting cached data")
    return(matrixInverse)
  }
  data <- x$get()
  matrixInverse <- solve(data)
  x$setinverse(matrixInverse)
  matrixInverse
}
