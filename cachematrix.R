##Pair of functions that cache the inverse of a matrix.

##Returns a list containig the functions needed to
##create/get the matrix 'x' and its inverse
makeCacheMatrix <- function(x = matrix()) {

  inverseMatrix <- NULL
  set <- function(y) {
    x <<- y
    inverseMatrix <<- NULL
  }
  get <- function() x
  setInverseMatrix <- function(inverse) inverseMatrix <<- inverse
  getInverseMatrix <- function() inverseMatrix
  list(set = set, get = get,
       setInverseMatrix = setInverseMatrix,
       getInverseMatrix = getInverseMatrix)
}

## Compute the inverse of a matrix and store the result in the object 'x'.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverseMatrix <- x$getInverseMatrix()
    if(!is.null(inverseMatrix)) {
      message("getting cached data")
      return(inverseMatrix)
    }
    data <- x$get()
    inverseMatrix <- solve(data, ...)
    x$setInverseMatrix(inverseMatrix)
    inverseMatrix
}
