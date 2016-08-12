

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

}


## This function checks if the inverse of the matrix has been calculated then prints the calculated inverse if it has or calculates and stores the inverse if it hasn't

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
