## Define two functions makeCacheMatrix and cacheSolve
## makeCacheMatrix creates a matrix that can cache its inverse
## cacheSolve computes the inverse of the matrix.
## if the inverse is already calculated it will return the cache
## value instead of calculating it again

## makeCacheMatrix takes matrix as an argument, and returns a list
## which contains the functions to set the matrix, get cached matrix
## get cached inverse and set the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<-y
    inv <<- NULL
  }
  get = function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve computes the inverse of the matrix using solve function
## if the value is already computed it will return a message
## and print the inverse of the matrix stored in cache

## if the value is not computed, it will apply the solve function
## on the matrix

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
        ## Return a matrix that is the inverse of 'x'
}
