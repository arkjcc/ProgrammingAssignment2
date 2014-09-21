## Matrix multiplication is a computationally intesive 
## operation especially when dealing with large matrices
## hence it is highly desirable to come up with ways to 
## cache these operations whenever possible.


## makeCacheMatrix
## - 
## Creates a special "matrix" that can cache its inverse
## This contains a list of functions to 
##    - set the value of the matrix
##    - get the value of the matrix
##    - set the inverse matrix
##    - get the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        cm <- NULL
        ## set function
        set <- function(y) {
        	    ## assigning to the parent frame
                x <<- y
                ## initial cached value is set to NULL
                cm <<- NULL
        }
        ## get function
        get <- function() x
        ## set inverse function
        setinv <- function(inverse) cm <<- inverse
        ## get inverse function
        getinv <- function() cm
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve
## -
## Computes the inverse of teh special "matrix" returned by
## makeCacheMatrix. If the inverse has already been calculated
## then this function retrieves teh inverse from the cache

cacheSolve <- function(x, ...) {
	    ## get the cached inverse
        inv <- x$getinv()
        if (!is.null(inv)) {
                message("getting from cached inverse")
                return (inv)
        }
        ## get the matrix from object
        matrix <- x$get()
        ## calculate the inverse using solve function
        inv <- solve(matrix, ...)
        ## cache the inverse
        x$setinv(inv)
        ## Return the inverse of 'x'
        inv
}
