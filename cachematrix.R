## A pair of functions that cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## This function retrieves the inverse of the special "matrix" returned by 
## makeCacheMatrix above from the cache, or computes it if not cached.

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        ## Return a matrix that is the inverse of 'x'
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
