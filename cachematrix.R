## These functions create a cacheable matrix inverse

## makeCacheMatrix takes a matrix as input and returns
## the special cacheable matrix

makeCacheMatrix <- function(x = matrix()) {
    # Initialize a null inverse matrix
    inv <- NULL

    # set matrix into local data
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }

    # return actual matrix
    get <- function() x

    # set inverse into cache
    setInv <- function(new_inv) inv <<- new_inv

    # get inverse from cache
    getInv <- function() inv

    list(set = set, get = get,
     setInv = setInv,
     getInv = getInv)

}


## cacheSolve computes inverse of the special matrix

cacheSolve <- function(x, ...) {
    # get the cached version and check if it is valid
    inv <- x$getInv()

    if (!is.null(inv)) {
        message("getting cached inverse")
        return(inv)
    }
    
    # cached inverse is null
    # we calculate and update cache
    data <- x$get()
    inv <- solve(data)
    x$setInv(inv)
    inv
}
