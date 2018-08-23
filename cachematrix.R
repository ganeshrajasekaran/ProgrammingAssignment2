## Scope : Caching the heavily computational matrix inversion result
## Reason : Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than compute it repeatedly
## The following functions addresses the computational issue by caching the inverse result for same input.

##
## This function creates the Cachable Matrix object. It is kind of wrapper for "matrix" object, it holds the input as well the inversion of Input.
## The responsiblity of this function is to create Cachable Matrix Object but it does not perform the actual Inverse of Matrix.
##
## Note : The variables like x & inv are in local enviroment of this cachable Matrix Object. 
## It cant be accessed directly but with through set/setinverse or get/getinverse function in the Cachable Matrix Object.
##
makeCacheMatrix <- function(x = matrix()) {
  
  ## Initial the variable as NULL
  inv <- NULL
  
  ## Reset or Set the current input value x 
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  ## return the current Input value x
  get <- function() x
  
  ## set the Inverse Result into the local enviroment
  setinverse <- function(inverse) inv <<- inverse
  
  ## Get the Inverse Result from the local enviroment
  getinverse <- function() inv
  
  ## Create Object of type list 
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


##
## A wrapper for the R solve function. It basically takes the CachableMatrixObject as input and performs the Inverse operation and cache the result in the MatrixObject.
## If the processed cachableMatrixObject is passed again, it returns the result from the cache.
##
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
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
