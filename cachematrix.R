## Put comments here that give an overall description of what your
## functions do

## This function is used for creating a matrix which can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
 inverse <- NULL
  set <-function(y) {
    x <<- y
    inverse <- NULL
  }
  get <- function() x
  setinv <- function(inv) inverse <<- inv
  getinv <- function() inverse
  list(set=set,get=get, setinv=setinv,getinv=getinv)
}


## This function gives the inverse of the matrix created by the function above ("MakeCacheMatrix").
##If the inverse has already been calculated the cached matrix is returned otherwise the inverse is calculated and returned.

cacheSolve <- function(x, ...) {
  inverse <- x$getinv()
  if(!is.null(inverse)) {
    message("Getting cached matrix")
    return(inverse)
  }
  matrix <- x$get()
  inverse <- solve(matrix, ...)
  x$setinv(inverse)
  inverse
       
}
