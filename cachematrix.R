## These two methods work together to create and use a special extended version 
## of matrix that caches the calculated value of the inverse and returns the 
## cached value on successive calls to the method, without having to go through 
## the sometimes costly routine of re-calculating the matrix.

## Define a special "object" that is a matrix with the special ability to 
##  store, or cache, the computed inverse
## CacheMatrix is created by cm <- makeCacheMatrix()
## CacheMatrix is set by e.g. cm$set(matrix(c(1,2,0,-1), 2, 2))
## CacheMatrix is retrieved by e.g. cm$get()
## CacheMatrix is solved for it's inverse by cm$getinverse()
## Optionally can pass in created matrix() with values instead of using the 
##  <obj>$set(...) method
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Retrieves cached value for matrix inverse, 
## or solves and caches value before returning inverse
## x must be "object" created by makeCacheMatrix
## Optional further parameters passed straight through to base R solve(matrix, ...) method
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    ## First, do input error checking (verify that is valid CM)
    if(class(x) != "list" || length(x) != 4 
       || class(x[[1]]) != "function" || class(x[[2]]) != "function"  
       || class(x[[3]]) != "function"  || class(x[[4]]) != "function"
       || names(x)[1] != "set" || names(x)[2] != "get" 
       || names(x)[3] != "setinverse" || names(x)[4] != "getinverse")
        stop("Invalid use of cacheSolve - must pass in cacheMatrix object created by makeCacheMatrix(...)")
    
    ## Do calculation
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}