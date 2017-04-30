## Together these functions create a matrix and a list of functions that allow the inverse of the matrix
## to be calculated once and then cached to avoid having to recalculate the inverse every time it is needed.

## makeCacheMatrix creates a special matrix object that includes a list of functions
## used by cacheSolve to get or set the cached inverse stored in the variable matinv.

makeCacheMatrix <- function(x = matrix()) {
        matinv <- NULL
        set <- function(y) {
                x <<- y
                matinv <<- NULL
        }
        get <- function() x
        setinv <- function(solve) matinv <<- solve
        getinv <- function() matinv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)

}


## When a makeCacheMatrix object is entered as an argument to cacheSolve, it checks to see 
## if the inverse is already cached, and if so, returns it. If not, it calculates the inverse, 
## puts it into the variable matinv, caches it, and returns it.

cacheSolve <- function(x, ...) {
        ## Checks to see if inverse of matrix is cached
        matinv <- x$getinv()
        if(!is.null(matinv)) {
                message("Getting cached data")
                return(matinv)
        }
        ##Calculates the inverse if it is not cached
        data <- x$get()
        matinv <- solve(data, ...)
        x$setinv(matinv)
        matinv
}
