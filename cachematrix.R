## cachematrix.R - cousera rprog007
## v0.1 daisy14 20140919
## provides functions to create a matrix object which 
## can cache its inverse matrix. 

## makeCacheMatrix 
## create matrix and function to get/set the matrix and its inverse.
makeCacheMatrix <- function(x = matrix()) {
    x_inv <- NULL
    set <- function(b) {
        x <<- b
        x_inv <<- NULL      # the inverse is invalid after a set operation
    }
    get <- function() x
    setinverse <- function(inverse) x_inv <<- inverse
    getinverse <- function() x_inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve
## returns the inverse matrix.
## if the inverse is cached, return the cached inverse, otherwise
## calculate the inverse and store it in the cache.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    x_inv <- x$getinverse()
    if(!is.null(x_inv)) {
        message("using cached data")
        x_inv
    }
    x_inv <- solve(x$get())
    x$setinverse(x_inv)
    x_inv
}
