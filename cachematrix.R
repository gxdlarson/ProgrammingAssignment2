## the purpose of the following two functions is to create a
## special object that stores a numeric matrix and cache's its
## inverse matrix.  the inverse matrix is cached in the sense
## that it can be computed once and then stored for further use.

## the makeCacheMatrix() function will create
## a list object with four (4) functions. these
## functions consist of setters and getters for
## the matrix 'x' and its inverse 'm'.
## note: both 'x' and 'm' are defined in
## the makeCacheMatrix() environment and are
## not local to any of the setters and/or getters.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    
    ## setter function to initialize or change
    ## the matrix 'x' and set the inverse matrix
    ## 'm' to null.
    set <- function(y) {
        ## perhaps there should be a check to see
        ## whether x and y are equal before doing
        ## the following assignments. oh well ...
        x <<- y
        m <<- NULL
    }
    
    ## getter function to retrieve the matrix 'x'
    get <- function() x
    
    ## setter function to set inverse matrix 'm' to
    ## value supplied with inverse parameter.
    ## this function should only be called from the
    ## cacheSolve() function.  otherwise, there is
    ## no guarantee the inverse matrix here is valid.
    setinverse <- function(inverse) m <<- inverse
    
    ## getter function to get the inverse matrix 'm'
    getinverse <- function() m
    
    ## function declarations for list items.
    ## enables the use of x$<item> syntax
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## the cacheSolve() function  returns the inverse matrix 'm'
## of a matrix 'x' created with the makeCacheMatrix() function.
## if the inverse matrix 'm' is null, then this function will
## compute the inverse matrix by calling the solve() function
## with matrix 'x'.

cacheSolve <- function(x, ...) {
    ## get the inverse matrix 'm' stored in 'x'
    m <- x$getinverse()
    
    ## if inverse exists,
    ## just return it.
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    ## if inverse does not exist,
    ## compute it, store it,
    ## return it.
    m <- solve(x$get(), ...)
    x$setinverse(m)
    m
}
