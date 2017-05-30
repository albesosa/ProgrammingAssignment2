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



# Example
# x=matrix(runif(20, 0, 1),nrow=4,ncol=4)
# cc=makeCacheMatrix(x)

# cc$getmatrix()
# [,1]      [,2]       [,3]      [,4]
# [1,] 0.5916053 0.7559402 0.79218359 0.7278767
# [2,] 0.0315016 0.1456996 0.07608882 0.7065250
# [3,] 0.9144490 0.5837768 0.17641658 0.5153764
# [4,] 0.8680757 0.6647327 0.23538558 0.7172623


#first run. The inverse matrix data was not in cache
# cacheSolve(cc)
# [,1]      [,2]       [,3]       [,4]
# [1,]  0.48200491  2.184841  10.109251  -9.905100
# [2,] -1.47633506 -6.124033 -18.818716  21.052403
# [3,]  2.27643019  2.043009   8.053398 -10.109173
# [4,]  0.03780006  2.360841   2.562753  -2.811091


#Second run. Retrieve data from the cache
# cacheSolve(cc)
# getting cached data
# [,1]      [,2]       [,3]       [,4]
# [1,]  0.48200491  2.184841  10.109251  -9.905100
# [2,] -1.47633506 -6.124033 -18.818716  21.052403
# [3,]  2.27643019  2.043009   8.053398 -10.109173
# [4,]  0.03780006  2.360841   2.562753  -2.811091


