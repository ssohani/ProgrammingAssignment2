## This function creates a cache with the inverse of a square matrix stored in the global environment. This 
## function is useful in avoiding repeated inverse calculations of lengthy matrices which can be time consuming.
## The cache can be accessed if the matrix whose inverse needs to be calculated is equal to the one that is 
## stored in the global environment.

## makeCacheMatrix function solves for the inverse of the matrix and stores the cache in the global 
## environment as variable ‘m’.

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


## cacheSolve accesses the inverse of matrix m from the global environment and returns the inverse 
## matrix without performing time consuming calculations. 

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

        ## Return a matrix that is the inverse of 'x'
}
