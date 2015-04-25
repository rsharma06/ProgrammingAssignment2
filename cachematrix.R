##Caching the Inverse of a Matrix


##This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    MatrixInverse <- NULL
    set <- function(y) {
      x <<- y
      MatrixInverse <<- NULL
    }
    get <- function() x
    setMatrixInverse <- function(Inverse) MatrixInverse <<- Inverse
    getMatrixInverse <- function() MatrixInverse
    list(set = set, get = get,
         setMatrixInverse = setMatrixInverse,
         getMatrixInverse = getMatrixInverse)
    

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    Inverse <- x$getMatrixInverse()
    if(!is.null(Inverse)) {
      message("getting cached data")
      return(Inverse)
    }
    data <- x$get()
    Inverse <- solve(data, ...)
    x$setMatrixInverse(Inverse)
    Inverse
  }
