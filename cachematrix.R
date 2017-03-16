## The idea was to modify a little bit the functions that were given as an example
## mainly by changing from vector to matrix.

## What this function does is generating a matrix "object", i.e. 
## a matrix that has functions to get/set its value, and to get/set its inverse.
## We can call this function with its argument equal to some matrix, but if we leave it
## empty, the default is an empty matrix and regardless of the choice, when we change the 
## value of the matrix, the inverse resets to NULL.

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set <- function(y) {
    x <<- y
    inv <<-NULL
  }
  get <- function() x
  setInv <- function(I)   inv <<-I
  getInv <- function() inv
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
  
}


## This function receives a matrix "object" and if it already has an inverse, it just
## prints it, if it doesn't have one, it gets it and updates the value of the inverse of
## the matrix "object" to the inverse.


cacheSolve <- function(x, ...) {
  i <- x$getInv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data,...)
  x$setInv(i)
  i
}
