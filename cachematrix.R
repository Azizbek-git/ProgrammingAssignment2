## Put comments here that give an overall description of what your
## functions do

##The main task of the functions is to cache the inverse of a matrix.

## Write a short comment describing this function
##This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  InverseValue <- NULL
  set <- function(y){
      x <<- y
      InverseValue <<- NULL
  }
  get <- function()x
  setInverse <- function(solve) InverseValue <<- solve
  getInverse <- function() InverseValue
  list(set = set, get = get, setInverse = setInverse, 
       getInverse = getInverse)
}


## Write a short comment describing this function

##This function computes the inverse of the special "matrix" 
##returned by makeCacheMatrix above. If the inverse has already 
##been calculated (and the matrix has not changed), then cacheSolve 
##should retrieve the inverse from the cache
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  InverseValue <- x$getInverse()
  if(!is.null(InverseValue)) {
    message("getting cached data")
    return(InverseValue)
  }
  data <- x$get()
  InverseValue <- solve(data, ...)
  x$setInverse(InverseValue)
  InverseValue
}

