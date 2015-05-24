## These codes help cache potentially time consuming computations
## These codes are split into two steps: cache function and 
## the inverse of a matrix computation function

## Step one: Cache Function. This function creates an object of a matrix that
## cashes the inverse which is a form of a special "matrix"
makeCacheMatrix <- function(x = matrix()) {
  makeCacheMatrix <- function(mtx = matrix()) {
    inverse <- NULL
    set <- function(x) {
      mtx <<- x;
      inverse <<- NULL;
    }
    get <- function() return(mtx);
    setinv <- function(inv) inverse <<- inv;
    getinv <- function() return(inverse);
    return(list(set = set, get = get, setinv = setinv, getinv = getinv))

}


## Step two: Inverse Matrix Computation Function. The inverse of the special "matrix" 
## is then computed obtained in step one as returned by the makeCacheMatrix function. cacheSolve 
## will return the inverse of the matrix in the event that the matrix is
## unchanged

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- mtx$getinv()
  if(!is.null(inverse)) {
    message("Getting cached data...")
    return(inverse)
  }
  data <- mtx$get()
  invserse <- solve(data, ...)
  mtx$setinv(inverse)
  return(inverse)
}
