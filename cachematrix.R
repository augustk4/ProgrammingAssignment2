## These functions work together to create a special "matrix" and find and 
## cache its inverse

## This function creates a special "matrix" stored in a list along with 
## commands to set and retrieve the "matrix" and its inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL                            
        set <- function(y) {                      # creates and caches the matrix
                x <<- y
                m <<- NULL
        }
        get <- function() x                       # subset to retrieves the matrix
        setInverse <- function(solve) m <<- solve # caches the inverse
        getInverse <- function() m                # subset to retrieve the cached inverse
        list(set = set, get = get,                # stores the functions in a list
             setInverse = setInverse,
             getInverse = getInverse)
}


## This function finds and caches the inverse of the "matrix" created by the
## first function

cacheSolve <- function(x, ...) {
        m <- x$getInverse()                     #loads any cached inverse
        if(!is.null(m)) {                       #returns previously cached inverse
                message("getting cached data")
                return(m)
        }
        data <- x$get()                         #loads the matrix
        m <- solve(data, ...)                   #solves for the inverse
        x$setInverse(m)                         #caches inverse
        m
}
