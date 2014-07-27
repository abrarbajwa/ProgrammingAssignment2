## These two functions basically initiate a few variables and store (cache) a matrix and its inverse in case it is turns up in the future so that inverse computation can be done expeditiously

## This function defines a class with functions and variables on which the data will be stored.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y = matrix()) {
                x <<- y
                i <<- NULL
        }
        get <- function() return(x)
        setInverse <- function(inverse) i <<- inverse
        getInverse <- function() i
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
            
}

## This function returns the inverse of the matrix through solve function

cacheSolve <- function(x, ...) {
        i <- x$getInverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data)
        x$setInverse(i)
        i
}