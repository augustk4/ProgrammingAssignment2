## These functions work together to create a special "matrix" and find and 
## cache its inverse

## The makeCacheMatrix function creates a special "matrix" stored in a list 
## along with commands to set and retrieve the "matrix" and its inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL                            
        set <- function(y) {                      # subset to create and cache the matrix
                x <<- y
                m <<- NULL
        }
        get <- function() x                       # subset to retrieve the "matrix"
        setInverse <- function(solve) m <<- solve # subset to cache the inverse
        getInverse <- function() m                # subset to retrieve the cached inverse
        list(set = set, get = get,                # stores the functions in a list
             setInverse = setInverse,
             getInverse = getInverse)
}

## The cacheSolve function finds and caches the inverse of the "matrix" 
## created by the makeCacheMatrix function

cacheSolve <- function(x, ...) {
        m <- x$getInverse()                     # loads any previously cached inverse
        if(!is.null(m)) {                       # returns previously cached inverse
                message("getting cached data")
                return(m)
        }
        data <- x$get()                         # loads the "matrix"
        m <- solve(data, ...)                   # solves for the inverse
        x$setInverse(m)                         # caches inverse
        m
}
