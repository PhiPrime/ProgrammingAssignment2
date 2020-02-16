## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix returns a list containing a function to:
    #1 set the value of the matrix
    #2 get the value of the matrix
    #3 set the value of the inverse
    #4 get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function () x
  setInv <- function (inv) i <<- inv
  getInv <- function() i
  list (set = set, get = get,
        setInv = setInv,
        getInv = getInv)
}


## cacheSolve calculates the inverse of the "matrix"
## First it checks if the inv. has already been calculated.
## If it has, it will skip additional computations

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
  i <- x$getInv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)#Since Inv was already found return value of `i`
  }
  data <- x$get()
  i <- solve(data, ...)#getInv() was null, so it is solved now
  x$setInv(i)#value is saved to "matrix"
  i
}
