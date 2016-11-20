## Matrix inversion is usually a costly computation and there may be some benefit to 
## caching the inverse of a matrix rather than compute it repeatedly 
## (there are also alternatives to matrix inversion that we will not discuss here). 
## Your assignment is to write a pair of functions that cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(z = matrix()) {
    InvM <- NULL
    
    BuildMatrix <- function(matrix = matrix()){
      z <<- matrix
    }
    
    LoadMatrix <- function() z
    
    InvMatrix <- function(inverseMatrix = matrix()){ 
      InvM <<- inverseMatrix
    } 
    LoadInvert <- function() inverse
    list(get = LoadMatrix, set = BuildMatrix, getI = LoadInvert, setI = InvMatrix)
  }



## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has 
## already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x' 
    
    if(is.null(x$getI())){
      print("Not found, recompute...") 
      x$setI(solve(x$get()))
    }else {print("Found, loading...")}
    
    x$getI()
  }

