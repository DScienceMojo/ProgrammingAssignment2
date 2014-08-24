## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
