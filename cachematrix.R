# My first function caches the inverse of a matrix and the second one
# computes the inverse of the matrix returned above. If the inverse has
# already been calculated then the second function retrieves the inverse
# from the cache.

# This function caches the inverse of a matrix:
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
        
}

# This function computes the inverse of the matrix returned by 
# the previous function. If the inverse has already been 
# calculated then this function retrieves the inverse 
# from the cache:
cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setInverse(inv)
        inv
}
