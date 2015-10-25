## The functions within this function are essentially a list. First the matrix values
## are set and the next function gets the values of the matrix. Then the value
## of the inverse is set. And the last function gets the inverse of the matrix.

## This function creates a matrix in which it is possible to cache the inverse of a
## matrix. This enables you to use the cached inverse in subsequent functions or
## computations without needing to compute the inverse everytime anew.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
            x <<- y
            m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get, 
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve calculates the inverse of the matrix created in the previous function.
## It checks if the inverse is already calculated. If that's true it will get the
## mean from the cache and skips futher computation. If it is not yet calculated,
## the function will calculate the inverse and sets the inverse in the cache through
## the setinverse function.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
            message("getting cached data")
            return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
