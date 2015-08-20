## These functions catche the inverse of a matrix

## The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to
        # 1 - set the value of the matrix
        # 2 - get the value of the matrix
        # 3 - set the value of the inverse of the matrix
        # 4 - get the value of the inverse of the matrix


makeCacheMatrix <- function( x = matrix()) {
        slv <- NULL
        set <- function(y) {
                x <<- y
                slv <<- NULL
        }
        get <- function() x
        setsolve <- function(solve_x) slv <<- solve_x
        getsolve <- function() slv
        list( set = set,
              get = get,
              setsolve = setsolve,
              getsolve = getsolve)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        slv <- x$getsolve()
        if(!is.null(slv)) {
                message("getting cached data")
                return(slv)
        }
        data <- x$get()
        slv <- solve(data, ...)
        x$setsolve(slv)
        slv
}
