# Caching the Inverse of a Matrix:

# This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        
        inve <- NULL
        set <- function(y) {
                x <<- y
                inve <<- NULL
        }
        
        get <- function() x
        setInverse <- function(inverse) inve <<- inverse
        getInverse <- function() inve
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


# This function computes the inverse matrix of makeCacheMatrix function

cacheSolve <- function(x, ...) {
 
        inve <- x$getInverse()
        if (!is.null(inve)) {
                message("retrieving cached data")
                return(inve)
        }
        mat <- x$get()
        inve <- solve(mat, ...)
        x$setInverse(inve)
        inve
}        
