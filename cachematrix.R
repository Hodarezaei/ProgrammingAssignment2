##I am going to write a pair of functions to create a matrix (makeCacheMatrix) and 
##cache its inverse (cacheSolve),if the inverse has already been calculated 
##then cacheSolve should retrieve the inverse from the cache. 

## This function creates a special matrix that can caches its inverse

makeCacheMatrix <- function(x = matrix()) {
  invMat <- NULL
  set <- function(y) {
    x <<- y
    invMat <<- NULL
  }
  get <- function() x
  setinvMat <- function(inverse) invMat <<- inverse
  getinvMat <- function() invMat
  list(set = set, get = get,
       setinvMat = setinvMat,
       getinvMat = getinvMat)
}

## This function computes the inverse of the special matrix created by makeCacheMatrix

cacheSolve <- function(x, ...) {
  invMat <- x$getinvMat()
  if(!is.null(invMat)) {
    message("getting cached data")
    return(invMat)
  }
  data <- x$get()
  invMat <- solve(data, ...)
  x$setinvMat(invMat)
  invMat
}

