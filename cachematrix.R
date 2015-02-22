## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
makeCacheMatrix <- function(x = matrix()) { # pass argument in as x
        m <- NULL # set m as NULL
        set <- function(y) { # makeVector$set becomes a function that passes an argument as y
                x <<- y # y is assigned to x using <<- since x is out of scope (in the parent environment)
                m <<- NULL # m is set to NULL again?
        }
        get <- function() x # makeVector$get is a function that returns x
        setinverted <- function(inverted) m <<- inverted # simple function that sets value of m to whatever is passed to makeVector$setmean
        getinverted <- function() m # makeVector$getmean just returns m, set above
        list(set = set, get = get, # returns the functions as a list of functions or values if set, but only cachemean can set these values! always returns code without
             setinverted = setinverted,
             getinverted = getinverted)
}

## Write a short comment describing this function
cacheSolve <- function(x, ...) { # ... means operate on each of the arguments passed I think???
        m <- x$getinverted() # m returns m from getmean? so NULL?
        if(!is.null(m)) {
                message("getting cached data")
                return(m) # checks if m isn't null (i.e. if it exists in the environment) and returns m if it isn't, skips out of function
        }
        data <- x$get() # data gets value of x
        m <- solve(data, ...) # m gets the mean of data (which is x from the original makeVector function call?)
        x$setinverted(m) # m gets m from outside of makeVector scope
        m # m is returned
}

## Testing
x <- makeCacheMatrix(matrix(rnorm(81),nrow=9,ncol=9))
cacheSolve(x)
