## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  ## initialize the inverse
  x_inverse <- NULL
  set <- function(y) {
    x <<- y
    x_inverse <<- NULL
  }
  ##get the matrix
  get <- function() x
  ## set the inverse matrix by calling solve
  setinverse <- function(solve) x_inverse <<- solve
  ## get the inverse variable
  getinverse <- function() x_inverse
  ## list to be returned, containing the special matrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then cacheSolve should retrieve the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ## first get the inverse matrix
  x_inverse <- x$getinverse()
  ## if the matrix inverse is not NULL, get it from the cache
  if(!is.null(x_inverse)) {
    message("getting cached inverse matrix")
    return(x_inverse)
  }
  data <- x$get()
  ## calculate the inverse of the matrix 
  x_inverse <- solve(data, ...)
  ## set the special matrix inverse
  x$setinverse(x_inverse)
  ## return the inverse
  x_inverse
}
