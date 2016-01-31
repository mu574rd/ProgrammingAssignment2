## This function creates a "matrix" that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setmatrix <- function(solve) m <<- solve
    getmatrixn <- function() m
    list(set=set, get=get,
         setmatrix=setmatrix,
         getmatrix=getmatrix)

}


## This function solves the inverse of the "matrix" returned by makeCacheMatrix. 
## If the inverse has already been calculated, then the 
## cachesolve function should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    
        ## Return a matrix that is the inverse of 'x'
        m <- x$getmatrix()
        if(!is.null(m)) {
            message("getting cached matrix")
            return(m)
        }
        matrix <- x$get()
        m <- solve(matrix, ...)
        x$setmatrix(m)
        m

}
