# Assignment 2
## Your assignment is to write a pair of functions that cache the inverse of a matrix.

# Write the following functions:
# 1. makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
# 2. cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), 
# then the cachesolve should retrieve the inverse from the cache.

## 1. makeChacheMatrix
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getsolve <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getsolve = getsolve)
}

## 2. cacheSolve
cacheSolve <- function(x, ...) {
    m <- x$getsolve()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}

a <- matrix(c(5, 4, 1, 2), nrow = 2, ncol = 2)
a2 <- makeCacheMatrix(a)
cacheSolve(a2)

a2$get()

