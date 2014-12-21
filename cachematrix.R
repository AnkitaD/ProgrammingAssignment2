# Caching the Inverse of a Matrix.
# input to program provided as follows
# newmatrix <- matrix(data=c(some numbers),nrow,ncol)
# new <- makeCacheMatrix(newmatrix)
# cacheinverse(new)




makeCacheMatrix <- function(x = matrix()) {  # to indicate input x is in matrix format
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse    # set inverse matrix called by cacheSolve
  getinverse <- function() m       #get inverse matrix 
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)   #inbulit function to find inverse
  x$setinverse(m)
  m
  
}
