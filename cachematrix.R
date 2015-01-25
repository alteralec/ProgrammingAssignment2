## This script contains two functions to cache the inverse of a matrix 
## It is especially useful in the case of large matrices
## It avoids performing long computations for no reason

## This function creates a special matrix object with a cache for its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL # Initialize the cache
  
  ## This function sets the matrix
  set<-function(y){
    x<<-y
    inv<<-NULL
  }
  
  get <- function() x ## This function gets the matrix
  setinv <- function(inverse) inv <<- inverse ## This function sets the inverse of the matrix
  getinv <- function() inv ## This function gets the inverse of the matrix
  
  ## This returns the list of all of the above functions
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## This function returns the inverse of the special matrix object
## Computation will be avoided if the inverse has been computed previously
## In this case the inverse is retrieved from the cache

cacheSolve <- function(x, ...) {
  
  inv <- x$getinv() ## Retrieve cache
  
  ## In the case the cache is not empty
  if(!is.null(inv)) {
    message("Retrieving info from cache")
    return(inv)
  }
  
  ## In the case the cache is empty
  
  mat <- x$get() ## Get the matrix
  inv <- solve(mat, ...) ## Perform inversion computation
  x$setinv(inv) ## Cache the result of the computation
  inv ## Return the result
}
