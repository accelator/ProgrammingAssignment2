## Put comments here that give an overall description of what your
## The construction of makeCacheMatrix function just simulates the use of makeVector.
## The main responsibility of this function is setting and storing both the input
## matrix and the inverse matrix.
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Write a short comment describing this function
## The use of cacheSolve is quite similar to the cacheMean. 
## First, it judges whether the input matrix has been calculated before
## If the input has never been calculated, the function will calculate
## the inverse matrix and print the result.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m        ## Return a matrix that is the inverse of 'x'
}
