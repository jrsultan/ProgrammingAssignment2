
## This function creates the special matrix object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## This function computes the inverse of the special matrix returned by makeCacheMatrix above.
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
}

## This creates a 2x2 matrix with values 1:4, as a sample to use when using cacheSolve.
InverseMatrix<-makeCacheMatrix(matrix(1:4, 2, 2))
InverseMatrix$get()
InverseMatrix$getinv()

## This applies the cacheSolve matrix to the InverseMatrix made above, or making the inverse of the InverseMatrix.
cacheSolve(InverseMatrix)
