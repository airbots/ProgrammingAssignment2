## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  setInverMatrix <- function(inverseMatrix) m <<- inverseMatrix
  getInverMatrix <- function() m

  list(set = set, get = get,
       setInverMatrix = setInverMatrix,
       getInverMatrix = getInverMatrix)
}

## Write a short comment describing this function
cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverMatrix()
  if(!is.null(m)) {
    message("getting cached Matrix")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverMatrix(m)
  m
}

