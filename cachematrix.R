##The following two functions creates a matrix
##and finds it's inverse (given that it has one)
##either by computing it or retreiving the cached
##inverse matrix

## The makeCacheMatrix function sets a matrix that
## is prepared in a cachable way

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(solve) inv <<- solve
        getinv <- function() inv
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}




## The cacheSolve function retreives the inverse of the matrix 'x' that
## was the input from the function above (either by retreiving the already
## computed inverse matrix or finding the inverse matrix from scratch)



cacheSolve <- function(x = matrix(),...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("Working tirelessly to find that cached inverse matrix of yours...")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data,...)
        x$setinv(inv)
        inv
        
}