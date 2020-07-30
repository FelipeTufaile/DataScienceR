## "MakeCacheMatrix" is a function for creating matrices in a way that the 
## information can be cached for fast usage in future procedures.
## The function receives / defines a matrix and creates the following objects:
## "get", "set", "getinv" and "setinv".

makeCacheMatrix <- function(x = matrix()) {
  
  ## Here, "inv" is defined as NULL so when it is called in "cacheSolve"
  ## for the first time, it won't generate an
  ## error.
  inv <- NULL
  
  ## "get" is a function that gets a matrix "x", stored in the main function.
  ## The function "get" does not require any input.
  get <- function() x
  
  ## "set" is a function that sets a matrix "y" to an existing matrix "x".
  ## In order to use this function, it is necessary to pass matrix, "y",
  ## as argument for the function.
  set <- function(y){
    x <<- y ## The double arrow symbol is required in order to apply the
            ## statement in the main function and not only locally.
    inv <<- NULL ## Setting "inverse" equal to NULL will erase the previous
                     ## value of "inverse" which corresponded to the inverse of 
                     ## the old "x" matrix (i.e. before setting "x" to "y").  
  }
  
  ## The objects "getinv" and "setinv" below are similiar to the functions
  ## "get" and "set", mentioned above. However, they are meant to get and set
  ## the inverse of "x".
  getinv <- function() inv
  setinv <- function(inverse) {inv <<- inverse}
  
  
  ## Assigning the 4 functions above as objects of the function makeCacheMatrix
  list(set = set, get = get, getinv = getinv, setinv = setinv)
}

## "cacheSolve is a alternative for the "solve" function.
## For "cacheSolve, if the inverse of a matrix is already defined, it returns 
## the inverse stored in cache. Otherwise, it executes "solve".

cacheSolve <- function(x, ...) {
    ## First, "cacheSolve" gets the value of "inv" stored as objetc of the 
    ## function "makeCacheMatrix" using "getinv" and checks if the inverse 
    ## is already defined. That is, checks whether or not the variable "inv" 
    ## is NULL.
    inv <- x$getinv()
    if(!is.null(inv)){
      message('Getting cached data...')
      return(inv)}
    ## If the variable "inv" is NULL, it executes "solve" and stores the
    ## the inverse of "x" as an object of "makeCacheMatrix" using "setinv".
    else {
      inv <- solve(x$get())
      x$setinv(inv)
      return(inv)
    }
}
