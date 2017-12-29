##Author: Sara Regina Ferreira de Faria
##Date: December, 29th, 2017
##Data Science - R Programming - Johns Hopkings University

#This program will hande Matrix inversion, a costly computation calculation,
#using cache memory to reduce computational consuming


#creates a "matrix" object that can cache its inverse
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


##computes the inverse of a "matrix" using makeCacheMatrix
cacheSolve <- function(x, ...) {
  data <- x$get()
  if (nrow(data) == ncol(data))
  {
    inv <- x$getinv()
    if(!is.null(inv)) {
      message("getting cached data")
      return(inv)
    }
    inv <- solve(data) #assuming that data is invertible
    x$setinv(inv)
    return(inv)
  }
  message("Can not calculate the inverse. The matrix must be a square matrix")
}
