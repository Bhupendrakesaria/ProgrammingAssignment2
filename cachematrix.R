## Caching the Inverse of a Matrix.
## Obtaining Inverse of a Matrix is a costly computation. Caching the Inverse will save computation time of 
## obtaining Inverse repeatedly. 

## This Function creates a special "Matrix" function object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL        # Assigining NULL to inv
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }   ## End of function y
  
  get <- function() x
  setInverse <- function(solveinverseofMatrix) inv <<- solveInverseofMatrix
  getInverse <- function() inv
  list(set=set, get = get, setInverse = setInverse, getInverse = getInverse)

}   # End of makecacheMatrix function


## This function computes the Inverse of the special "matrix" createded by above function makecacheMatrix.
## If the Inverse has already been calculated  (and if the matrix is unchanged) , then it it must retrieve from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("Getting cached Data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setInverse(inv)
  inv
}
