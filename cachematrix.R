## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolved <- function(solvedmatrix) m <<- solvedmatrix
  getsolved <- function() m
  list(set = set, get = get, setsolved = setsolved, getsolved = getsolved)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        m <- x$getsolved()
        if(!is.null(m)){
          message("getting cached data")
          return(m)
        }          
        data <- x$get()
        m <- solve(data, ...)
        x$setsolved(m)
        m
}
