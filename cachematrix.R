
## makeCacheMatrix: This function creates a special 
## "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        Inv <- NULL
        set <- function(y) {
                x <<- y
                Inv <<- NULL
        }
        get <- function() x
        setinvmat <- function(solve) Inv <<- solve
        getinvmat <- function() Inv
        list(set = set, get = get,
             setinvmat = setinvmat,
             getinvmat = getinvmat)
}

## cacheSolve: This function computes the inverse of 
## the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix 
## has not changed), then the cachesolve should retrieve the 
##inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        Inv <- x$getinvmat()
        if(!is.null(Inv)) {
                message("getting cached data")
                return(Inv)
        }
        data <- x$get()
        Inv <- solve(data, ...)
        x$setinvmat(Inv)
        Inv
}
