##This function creates a matrix that caches the inverse.  
##It is done so that it does not have to be computed over and over

makeCacheMatrix <- function(x = matrix()) {
  abc <- NULL
  set <- function(y) {
    x <<- y
    abc <<- NULL
  }
  get <-function() x
  setInverse <- function(catch) abc <<-catch
  getInverse <- function() abc
  list(set = set, 
       get = get,
       setInverse = setInverse, 
       getInverse = getInverse)
}


## This function computes the inverse of the matrix. 
##If it has already been computed it does not compute again
##All it does is pulls it out of the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  abc <- x$getInverse()
  if(!is.null(abc)){
    message("getting the cached datas")
    return(abc)
  }
  cba <- x$get()
  abc <- solve(cba, ...)
  x$setInverse(abc)
  abc
}
