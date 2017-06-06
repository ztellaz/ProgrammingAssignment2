
## makeCacheMatrix creates a matrix that can cache its inverse.
##it contains 4 functions

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function (y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(solve) 
                m <<- solve
        getInverse <- function() m
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
        
}


## cacheSolve computes the inverse of the matrix
##it should retrieve the inverse from cache (if matrix the same)
##otherwise calculates the inverse and sets the value of the inverse

cacheSolve <- function(x, ...) {
        m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cahced data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        return(m)
}
