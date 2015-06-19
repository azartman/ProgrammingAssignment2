## The purpose of the following functions is to cache the inverse of a matrix

## makeCacheMatrix creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmatrix <- function(solve) m <<- solve
        getmatrix <- function() m
        list(set = set, get = get, setmatrix = setmatrix, getmatrix = getmatrix)
}


## cacheSolve computes the inverse of the special "matrix" object returned by makeCacheMatrix above. If the inverse
## has already been calculated, then the cacheSolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        m <- x$getmatrix()
        if(!is.null(m)) {
              message("Getting cached data...")
              return(m)
        }
        matrix <- x$get()
        m <- solve(matrix,...)
        x$setmatrix(m)
        m
}
