## "Data structure" which is really a set of functions to hold a matrix 
## along with its cache matrix inversion. Also, functions to support the 
## matrix creatation and matrix inversion uitlizing the "data structure".

## Makes a special vector which consists of 4 operations (functions) to set,
## get the data matrix and to set, get the inversion of that data matrix.
## The inverted matrix will be cached once calculated and cleared when the
## data matrix changes.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverted) inv <<- inverted
    getInverse <- function() inv
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Calculates matrix inversion for a given matrix by first looking at the
## cached inverted matrix. Calculate the inversion and cache it if it was 
## not cached before.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    if (!is.null(inv)) {
        message("Getting cached inverted matrix.")
        return(inv)
    }
    message("Inverted matrix is not cached. Calculate and cache.")
    m <- x$get()
    ## Assumption that the matrix is always invertible
    inv <- solve(m)
    x$setInverse(inv)
    inv
}
