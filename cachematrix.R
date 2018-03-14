## Put comments here that give an overall description of what your
## functions do

## Create a special "matrix" which is really a list containing a function to: 
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the inverse of the matrix
## 4. get the inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
  invX <- NULL
  set <- function(y) {
    x <<- y
    invX <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) invX <<- inverse
  getinverse <- function() invX
  list(set = set, get = get,
       setinverse = setinverse, getinverse = getinverse)
}


## This function checks to see if the inverse matrix has already been calculated
## If so, it gets the inverse from the cache and skips the computation. 
## If not, it computes the inverse and sets the value in the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invX <- x$getinverse()
  if(!is.null(invX)) {
    message("getting cached data")
    return(invX)
  }
  data <- x$get()
  invX <- solve(data, ...)
  x$setinverse(invX)
  invX
}
