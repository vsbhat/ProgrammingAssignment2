
## this function returns a list with 4 elements of mode function to 
## set the matrix, get the matrix,set the inverse of the matrix, get the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(val) inv <<- val
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## this function returns a inverse of a matrix created using makeCacheMatrix function.
## if the inverse has been previously derived & cached it returns cached results, else this function also caches the 
## inverse value it computes for the given matrix for future retrival. 
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data,)
  x$setinv(i)
  i
}
