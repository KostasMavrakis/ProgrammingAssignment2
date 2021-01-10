#the following function makeCacheMatrix creates a special "matrix", which is really a list containing a function to set and get the values of the matrix and the inverse

makeCacheMatrix <- function(x = matrix()) {
  
#makeCacheMatrix creates a special "matrix" object that can cache its inverse.
  
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}
  
#The following function cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above.
  
cacheSolve <- function(x, ...) {
  
#cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above.
#If the inverse has already been calculated, then the cachesolve should retrieve the inverse from the cache.

  i <- x$getinverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
#Computing the inverse of a square matrix can be done with the solve function in R.
  
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}

#We assume that the matrix supplied is always invertible.
