## Put comments here that give an overall description of what your
## functions do

## As in the example given, makeCacheMatrix is a function that creates a special matrix which is really a list containing a function to
##set the value of the matrix
##get the value of the matrix
##set the value of the matrix
##get the value of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() {
    x
  }
  setinverse <- function(inverse) {
    inv <<- inverse
  }
  getinverse <- function() {
    inv
  }
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve is a function to calculate the inverse of the special matrix created by makeCacheMatrix. First, it checks if
## the inverse has already been calculated and in this case it takes that result from the cache. On the contrary, when the result 
## to the cheking is negative, it performs the calculation of the inverse

cacheSolve <- function(x, ...) {
  inv <- x$getinverse() ## Return a matrix that is the inverse of 'x'
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
        
