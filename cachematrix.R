#Transforms a standard numberic matrix to a matrix that can cache its inverse.

#Transforms a standard numberic matrix to a matrix that can cache its inverse.
# Params: x a standard numeric matrix
# Returns: a cached matrix
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


#Calculates the inverse of a cached matrix.
#If the inverse has already been calculated, 
#returns the cached inverse
# Params: x cached amatrix
# Returns: the inverse of a cached matrix
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
