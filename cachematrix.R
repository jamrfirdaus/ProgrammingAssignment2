## Create a cache matrix object that can be used to solve
## the inverse of the matrix


## Create a cacheMatrix object

makeCacheMatrix <- function(x = matrix()) {
    cachedInverse <- NULL
    set <- function(y) {
        x <<- y
        cachedInverse <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) cachedInverse <<- inverse
    getInverse <- function() cachedInverse
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Return the inverse of an cacheMatrix object

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    invFunc <- x$getInverse()
    if(!is.null(invFunc)) {
        message("getting cached data")
        return(invFunc)
    }
    data <- x$get()
    invFunc <- solve(data, ...)
    x$setInverse(invFunc)
    invFunc
}
