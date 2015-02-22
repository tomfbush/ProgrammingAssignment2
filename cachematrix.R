## These functions enable the caching of a matrix and its inversion for 
## efficient retrieval, in the event that the inversion of the same input matrix
## is requested a second time. The functions exploit the use of R's lexical
## scoping and the '<<-' operator to enable the passing of values that would
## normally be out of scope for the environments of the functions.

## makeCacheMatrix is a special function that contains a list of other functions
## that enable the setting and retrieving of the source matrix, as well as the
## setting and retrieving of the inverted. The actual inverting is done in the
## cacheSolve function.

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

## cacheSolve checks if m is null; if it is not, it retrieves the cached,
## inverted matrix. If it is, it retrieves the uninverted matrix from its
## argument, passes it to data, uses the solve() function to invert it and sets
## it back with the special function x$setinverted(). Finally it returns it.
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