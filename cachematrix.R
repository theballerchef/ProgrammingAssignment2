

## This function creates a matrix that can store the inverse

makeCacheMatrix <- function(x = matrix()) {
    c <- NULL
    ## set matrix if needed
    store <- function(y=x){
        c <<- NULL
        x <<- y
        x
    }
    ## store the inverse of the matrix
    storeinverse <- function(inverse){
        c <<- inverse
        c
    }
list(store = store, storeinverse = storeinverse)
}


## This function checks if the inverse of the matrix has been calculated and cached. If it has, it prints the inverse, if it hasn't it calculates the inverse and caches it.

cacheSolve <- function(x, ...) {
        c <- x$storeinverse()
        ## Has the inverse been calculated?
        if(is.null(c))
        {
            stored <- x$store()
            c <- solve(stored)
            x$storeinverse(c)
        }
        else
        {
            c
        }
        
}
