## Caching the Inverse of a Matrix:

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        temp <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) temp <<- inverse
        getInverse <- function() temp
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}
}


## This function computes the inverse by the makeCacheMatrix above.
## If the inverse has already been calculated , then it should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        temp <- x$getInverse()
        if (!is.null(temp)) {
                message("getting the cached data")
                return(temp)
        }
        mat <- x$get()
        temp <- solve(mat, ...)
        x$setInverse(temp)
        temp
}
