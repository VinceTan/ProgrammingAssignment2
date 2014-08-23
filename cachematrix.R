## Programming Assignment 2
## Assignment: Caching the Inverse of a Matrix
## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  ## Set matrix
  
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  ## Get matrix  
  
  get <- function() x
  
  ## Set and get matrix inverse
  
  setmatrixinv<- function(solve) m <<- solve
  getmatrixinv <- function() m
  
  list(set = set, get = get,
       setmatrixinv = setmatrixinv,
       getmatrixinv = getmatrixinv)
}

## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##             If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {

  ## Get matrix inverse
    m <- x$getmatrixinv()
    
  ## Check if matrix inverse is available in cache
  ## Getinverse if available
  
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
  
  ## Solve inverse if not available
    matrix <- x$get()
    m <- solve(matrix, ...)
    x$setmatrixinv(m)
    m
}
  
