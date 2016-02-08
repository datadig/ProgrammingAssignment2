## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  ## Assumptions:
  ##  1. The "x" is a square matrix and invertible 
  ## Function Description:
  ##              1. set the value of the matrix
  ##              2. get the value of the matrix
  ##              3. set the value of the inverse matrix
  ##              4. get the value of the inverse
  ##  
  
  inv = NULL
  set = function(y) {
    # use `<<-` to assign a value to an object in an environment 
    # different from the current environment. 
    x <<- y
    inv <<- NULL
  }
  get = function() x
  setinv = function(inverse) inv <<- inverse 
  getinv = function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

  inv = x$getinv()
  
  # is inverse calculated already?
  if (!is.null(inv)){
    # get it from the cache and skips the computation. 
    message("using cached data....")
    return(inv)
  }
  
  # Inverse not calculated.. calculating the inverse below 
  mat.data = x$get()
  inv = solve(mat.data, ...)
  
  # seting inverse in the cache below.
  x$setinv(inv)
  
  return(inv)
  
}
