## R Programming Assignment 2: 
## Author: Colleen Davies
## Function and variable names provided by rdpeng

## makeCacheMatrix() creates an object of type makeCacheMatrix() which is then
## used by cacheSolve() to either retrieve the inverse of the matrix passed to
## makeCacheMatrix() from the cache, or compute it if necessary.

## makeCacheMatrix() takes a matrix as an input and returns an object that 
## contains 4 functions (set, get, setinv, getinv), as well as x and inv.

makeCacheMatrix <- function(x = matrix()) { # x initialized here
    inv <- NULL # initializes inv as an object
    set <- function(z) {
        x <<- z # initializes input in parent environment
        inv <<- NULL # clears cache in parent environment
    }
    get <- function() x # retrieves x from makeCacheMatrix(x)
    setinv <- function(inverse) inv <<- inverse # sets inverse in parent environ
    getinv <- function() inv # retrieves inverse from makeCacheMatrix(x)
    list(set = set,
         get = get,
         setinv = setinv,
         getinv = getinv) # returns to parent environment
}

## cacheSolve takes an object x of the type makeCacheMatrix(), and either 
## retrieves the inverse of x$get() from the cache if it has already been 
## computed, or calculates the inverse and stores it in the cache.

cacheSolve <- function(x, ...) {
    inv <- x$getinv() # initializes inv
    if(!is.null(inv)) { # checks if inverse is already in the cache
        message("retrieving from cache")
        return(inv) # returns inverse if already in cache
    }
    # if inverse not already in cache:
    mymat <- x$get() # retrieves original matrix from x
    inv <- solve(mymat) # computes inverse of original matrix
    x$setinv(inv) # stores inverse in cache
    inv # returns inverse
}

# Useful reference 
# https://github.com/lgreski/datasciencectacontent/blob/master/markdown/
#   rprog-breakingDownMakeVector.md

