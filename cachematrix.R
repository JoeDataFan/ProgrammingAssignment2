## R script for R Programming Assignment 2 from week 3 - Lexcial Scoping
## The two functions below "makeCacheMatrix" and "cacheSolve" are designed
## to cache the inverse of a matrix "x".

## The function "makeCacheMatrix" creates a special matrix with functions 
## to set and get the value of the matrix and functions to set and get the
## inverse of the matrix "x". the enviroment within this functions is where
## "x" is defined.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## The function "cacheSolve" calculates the inverse of the special matrix 
## from the "makeCacheMatrix" function. This function also determines if 
## the inverse of the matrix has already been determined. If so, the function 
## will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}