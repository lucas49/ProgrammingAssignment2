## This function creates a special "matrix" object 
## that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  minvrs <- NULL
  set <- function(y) {
    x <<- y
    minvrs <<- NULL
  }
  get <- function() x
  setminvrs <- function(inverse) minvrs <<- inverse
  getminvrs <- function() minvrs
  list(set = set, get = get,
       setminvrs = setminvrs,
       getminvrs = getminvrs)
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already 
## been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  minvrs <- x$getminvrs()
  if(!is.null(minvrs) && 
     (is.matrix(minvrs) && is.matrix(solve(x$get())) && dim(minvrs) == dim(solve(x$get())) && all(minvrs == solve(x$get())))){
    message("getting cached data")
    return(minvrs)
  }
  
  data <- x$get()
  minvrs <- solve(data,...)
  x$setminvrs(minvrs)
  minvrs
}
