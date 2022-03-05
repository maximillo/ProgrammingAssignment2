## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# 
# The first function, makeVector creates a special "vector", which is really a list containing a function to
# 
# set the value of the matrix
# 
# get the value of the matrix
# 
# set the value of the invert
# 
# get the value of the invert

makeCacheMatrix <- function(x = matrix()) {
  invert <- NULL
  set <- function(y){
    x <<- y
    invert <<- NULL
  }
  
  get <- function() x
  setinvert <- function(inv) invert <<- inv
  getinvert <- function() invert
  list(set = set, get = get, setinvert = setinvert, getmean = getmean)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invert <- x$getinvert()
  if(!is.null(invert)) {
    message("getting cached inverted matrix")
    return(invert)
  }
  data <- x$get()
  invert <- solve(data, ...)
  x$setinvert(invert)
  invert
}
