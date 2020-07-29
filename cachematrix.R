## The following functions calculate the inverse of a square, 
## invertible matrix by pulling the inverse from a cache or 
## by calculate the inverse from scratch.

## The first function creates the special matrix object with
## a cached inverse.

## The second function either retrieves the cached inverse 
## or calculates it. 

## Function that creates a special matrix, 
## whose inverse is cached within the object

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL # overwrite any existing inverse 
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


## A function that returns the cached inverse when available,
## or calculates the inverse 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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