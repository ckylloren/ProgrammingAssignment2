##Cache the inverse of a matrix so that it can be retrieved without recalculating repeatedly

## makeCacheMatrix creates a list of functions that can get the data, set the inverse, and retrieve the inverse.

makeCacheMatrix <- function(x = matrix()) {
  #print(class(x))
  inverse <- NULL
  get <- function() x #returns x
  setinverse <- function(solve) inverse <<- solve
  getinverse <- function() inverse
  list(get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve computes the inverse of the matrix returned by makeCacheMatrix. Will retrieve the inverse instead of calculating it if already calculated.

cacheSolve <- function(matx, ...) {
  s <- matx$getinverse() #assigns 'getinverse' item of matx to s.
  if(!is.null(s)) {
    message("getting cached data")
    return(s) #returns s and exits function
  }
  #next part only happens if 'if' stmt is false and doesn't return the inverse
  data <- matx$get() #assigns 'get' item of x to data
  b <- solve(data, ...) #calculates the inverse of data
  matx$setinverse(b) #assigns 'b' to inverse via function for setinverse
  b
}
