## The functions cache the inverse of a matrix.
## By using the "<<--" operator, the values of variables "x" and "inv"
## can be re-assigned in the environment that contains the environments
## from which the expressions for re-assignment have been called. 

## This function creates a list object that has as its elements the four
## functions used for caching and storing the matrix object.

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
             setinv = setinv,
             getinv = getinv)

}


## This function computes the inverse of a matrix returned by 
## makeCacheMatrix function above. If the inverse has already been calculated,
## then the cacheSolve will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    
        ## Return a matrix that is the inverse of 'x'

        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv



}
