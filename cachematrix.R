##
## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
##
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##

## makeCacheMatrix(x) creates special object which contains original matrix and - if was set - it's inverse.
## Cached inverse matrix is managed trough encapsulation of getter/setter functions.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get,
         setinverse = setinv,
         getinverse = getinv)
    
}


## cacheSolve() manipulates special matrix object created by makeCacheMatrix() and finds it's inverse.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve retrieve the inverse from the cache.
## Otherwise  solve(...) is used to calculate inverse and the result is cached.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    im <- x$getinverse()
    if(!is.null(im)) {
        message("getting cached data")
        return(im)
    }
    mtrx <- x$get()
    im <- solve(mtrx, ...)
    x$setinverse(im)
    im
}
