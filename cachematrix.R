## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  #inv <- matrix(NA,nrow(x),ncol(x))
  invMatrix <- NULL
  set <- function(y = matrix()) {
    x <<- y
    #inv <<- matrix(NA,nrow(x),ncol(x))
    invMatrix <- NULL
  }
  get <- function() x
  setInv <- function(i) invMatrix <<- i
  getInv <- function() invMatrix
  list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invMatrix <- x$getInv()
  if(!is.null(invMatrix)) {
    message ("getting cached Invertible matrix")
    return(invMatrix)
  }
  Matrixdata <- x$get()
  invMatrix <- solve(Matrixdata, ...)
  x$setInv(invMatrix)
  invMatrix
}
