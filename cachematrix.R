## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverted <- function(inverted) m <<- inverted
        getinverted <- function() m
        list(set = set, get = get,
             setinverted = setinverted,
             getinverted = getinverted)
}

## Write a short comment describing this function
cacheSolve <- function(x, ...) {
        m <- x$getinverted()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverted(m)
        m
}