## The makecacheMatrix creates a list that contains function to set
## the value of the matrix, get the value of the matrix, set the value of the
## inverse and get the value of the inverse. The cacheSolve function utilizes the first function
## to check if there is already a calculated value in the cache for the inverse of the matrix, if there is
## it gets that value otherwise it calculates the inverse.

## Makes a list of functions to set, get the value of a matrix, and set and get the inverse of that same matrix.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  

}


## Checks to see if there's already a calculated value for a matrix or a set of matrice in the cache.
## if there is it gets that value, otherwise it calculates the inverse. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
  
}
