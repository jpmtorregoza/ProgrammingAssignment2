## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse 
##of a matrix rather than compute it repeatedly (there are also alternatives to matrix inversion that we 
##will not discuss here). Your assignment is to write a pair of functions that cache the inverse of a matrix.


## This function creates a special "matrix" that caches the inverse of an input matrix. 

makeCacheMatrix <- function(x = matrix()) {
 m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInvMat <- function(inv) m <<- inv
  getInvMat <- function() m
  list(set = set, get = get,
       setInvMat = setInvMat,
       getInvMat = getInvMat)

}


## This function calculates the inverse of the cached matrix created by the makeCacheMatrix function if the inverse has not yet been calculated by a previous operation.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		m <- x$getInvMat()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInvMat(m)
  m
}
