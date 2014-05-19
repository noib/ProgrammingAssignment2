## The two functions return the inverse of an input matrix using cache storage in order to perform the computation only once

## This function takes a matrix as input
## and returns a list of methods allowing the caching & retrieval of the matrix and its inverse within its environment
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  # This 1st method returns the input matrix itself
  get <- function() x
  # This 2nd method caches a matrix (the inverse of the input matrix) in the variable "inv".
  # Note the use of assignment operator "<<-" which looks for "inv" in the parent environment.
  setinv <- function(inverse) inv <<- inverse
  # This 3rd method returns the matrix stored in "inv"
  getinv <- function() inv
  # The 3 methods get returned as a list
  list(get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function takes a "makeCacheMatrix" object as input
## and returns the cached inverse if it has already been computed or else computes the inverse
cacheSolve <- function(x) {
  # Gets the matrix stored in "inv"
  inv <- x$getinv()
  # If this matrix is not null, it means it's the inverse, which can be returned directly
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  # Otherwise the inverse must be calculated by retrieving the input matrix...
  data <- x$get()
  # and computing the inverse using "solve"...
  inv <- solve(data)
  # which must be cached for later retrieval...
  x$setinv(inv)
  # and then returned
  inv
}
