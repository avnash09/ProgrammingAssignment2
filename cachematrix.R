## Write a short comment describing this function

## The first function, makeCacheMatrix creates a special "vector"
## which is really a list containing a function to

## 1.set the value of the vector
## 2.get the value of the vector
## 3.set the value of the mean
## 4.get the value of the mean


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

## calculates the Inverse of the special "vector" created with the above function. 
## However, it first checks to see if the Inverse has already been calculated. 
## If so, it gets the Inverse from the cache and skips the computation. 
## Otherwise, it calculates the Inverse of the matrix 
## and sets the value of the Inverse in the cache via the setInv function.

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
