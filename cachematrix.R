           ##  Assignment: Caching the Inverse of a Matrix  ##

# Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse
# of a matrix rather than computing it repeatedly (there are also alternatives to matrix inversion that 
# we will not discuss here). Your assignment is to write a pair of functions that cache the inverse of 
# a matrix.


  
# The following function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  setmatrix <- function(y) {
    x <<- y
    inv <<- NULL
  }
  getmatrix <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(setmatrix = setmatrix, getmatrix = getmatrix,
       setinverse = setinverse,
       getinverse = getinverse)

}



# The following function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
# If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should 
# retrieve the inverse from the cache.
# 
# Computing the inverse of a square matrix is done with the solve function in R. This function assumes 
# that the matrix supplied is always invertible.


cacheSolve <- function(x, ...) {
 ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$getmatrix()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
  
}




