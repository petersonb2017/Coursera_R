## These funtions are mean to take in a matrix and cache its inverse so as to
## avoid costly computations.

## This functions takes a matrix and returns a list of functions making it
## capable to cache the data of the inverted form of a matrix.

makeCacheMatrix <- function(x = matrix()) {
m <- NULL
set <- function(y){
  x <<- y
  m <<- NULL
}
get <- function() x
setInvertMatrix <- function(invMatrix) m <<- invMatrix
getInvertMatrix <- function() m
list(set = set, get = get, setInvertMatrix = setInvertMatrix, 
     getInvertMatrix = getInvertMatrix)
}


## This function checks to see if a matrix has already had its invertion
## created, and returns it if it does, or computes and sets it if it hasn't.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInvertMatrix()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setInvertMatrix(m)
  m
}
