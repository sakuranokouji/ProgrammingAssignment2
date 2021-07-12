## Put comments here that give an overall description of what your
## functions do
## Coursera: R Programming
## Week 3 Assignment
## Write a short comment describing this function
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {    ## defines the argument, default mode set to "matrix" 
  inv <- NULL                                  ## inv will hold the inverse matrix value, inv is initialised as NULL
  set <- function(y) {                         ## define the set function to assign new variables  
      x <<- y                                  ## the value of matrix in the parent environment
      inv <<- NULL                             ## inv is reset to NULL if there is a new matrix
  }     
  get <- function() {x}                        ## defines the get function: returns the value of the matrix argument  
    setinverse <- function(inverse) {inv <<- inverse}     ## assign a value to inv in the parent environment
    getinverse <- function () {inv}                       ## gets the value of inv when called
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)    ##reference list for use with the $ operator
}

## Write a short comment describing this function
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
      message("getting cached data")
      return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
