###############################################################
## This pair of functions, makeCacheMatrix and cacheSolve,    #
## calculates the inverse of a square matrix and caches the   #
## the result to prevent unneccessary calculations later on   #
##                                                            #
## Example of usage:                                          #
##    x <- makeCacheMatrix(matrix(1:4,2,2))                   #
##    x$get()                                                 #
##    cacheSolve(x)                                           #
##    m <- x$get()                                            #
##    inv <- cacheSolve(x)                                    #
##    m                                                       #
##    inv                                                     #
##    m%*%inv                                                 #
##                                                            #
## May 24th. 2015                                             #
## Ola Lie                                                    #
###############################################################


## makeCacheMatrix takes a matrix as input
## and returns a list of setter and getter functions
## it also stores the matrix and caches the inverse if it is set 

makeCacheMatrix <- function(x = matrix()) {
    
    m <- NULL      # caches the inverse matrix
    
    ## setters and getters
    set <- function(y) {
        x <<- y    # change the matrix
        m <<- NULL # reset the inversed matrix, when the matrix is changed
    }
    
    get <- function() x
    setinv <- function(inv) m <<- inv
    getinv <- function() m
    
    ## return a list
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve returns the inverse of a matrix.
## The input must be the output from makeCacheMatrix
## (a list of functions) which stores a matrix, caches
## its inverse, and include setters and getters.
## If the inverse is cached,this function retrieves it.
## If it's not, this function calculates the inverse
## and stores the results in the closure of the setter.

cacheSolve <- function(x, ...) {
    
    ## try to get the cached inverse matrix
    inv  <- x$getinv()
    
    ## if it's cached, return it (and exit)
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    ## if the inverse is not cached
    data <- x$get()                 # get the matrix
    inv <- solve(data, ...)         # calculate its inverse
    x$setinv(inv)                   # cache it for later usage
    
    ## return the inverse of the matrix
    inv    
}

## "An object is data with functions. A closure is a function with data." - John D. Cook
## So when the function makeCacheMatrix returns the functions  set, get, setinv and getinv,
## these functions capture and store the execution environment of the particular call to 
## the function makeCacheMatrix where we find x (the matrix) and m (its inverse).
## New calls to makeCacheMatrix create different x's and m's.
