## The functions below allow to cache the inverse of a matrix so that if the inverse of a given matrix has already been 
## calculated it is not calculated again.

## Function that caches the inverse of a matrix (the matrix needs to have an inverse otherwise an error is triggered)

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function returns the inverse of a matrix that has been created with makeCacheMatrix. If the inverse has already been
## calculated then it is not calculated again

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
