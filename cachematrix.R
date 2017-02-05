##
## R Source Code File: cachematrix.R
##
## Manifest:
##
## function, makeCacheMatrix.  Caches (in memory) a matrix.
## 
## function, cacheSolve: Computes the inverse of a cahce matrix, if the computation
## is needed (not computed already before).
##

##
## function, makeCacheMatrix: This function creates a special "matrix" object 
## that can cache its inverse.
##
makeCacheMatrix <- function(x = matrix()) {
  ## function argument x is an assumed invertible matrix
  
  ## the '<<-' operator assigns a value to an object in 
  ## another (e.g. global) environment
  
  ## clear the cached inverted matrix
  inv <<- NULL
  
  ## define the matrix set function
  set = function(y) {
    x <<- y
    inv <<- NULL
  }
  
  ## define the matrix get function
  get = function() x
  
  ## define the inverse matrix set function
  setinv = function(inverse) inv <<- inverse
  
  ## define the inverse matrix get function
  getinv = function() inv
  
  ## return the matrix and inverse matrix object functions
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}

## function, cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix function. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve the inverse 
## from the cache.
##
cacheSolve <- function(x, ...) {
  ## function argument x is an assumed invertible matrix
  ## x and inv are already cached if previously solved by this function
  
  ## get the inverse matrix
  inv = x$getinv()
  
  ## check if the (solved) inverse matrix is defined
  if (!is.null(inv)){
    ## already solved, so use the inverse matrix from the cache 
    message("Cached Data Utilized")
    
    ## return the inverse matrix
    return(inv)
  }
  
  ## no prior (solved) inverse matrix, so get the matrix
  mat.data = x$get()
  
  ## solve the matrix, defining the inverse matrix
  inv = solve(mat.data, ...)
  
  ## set the inverse matrix into cache (in memory)
  x$setinv(inv)
  
  ## return the inverse matrix
  return(inv)
}

