## This pair of functions calculate the inverse of a matrix (if it has not 
## already been computed) and cache the inverse matrix. If the same matrix is
## passed to the function again, it returns the inverse from the cache instead
## of computing it again.


## This function creates a "matrix" object containing functions that allow you
## to set and retrieve a matrix and it's inverse. The matrix inverse is cached
## for later use.
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL 
        ## Change the matrix and reset the inverse to NULL
        set <- function(y) {
                x <<- y
                m <<- NULL  
        }
        ## Return the matrix
        get <- function() x
        ## Set the inverse matrix and cache the object 
        setInverse <- function(inverse) m <<- inverse 
        ## Return the inverse matrix
        getInverse <- function() m
        ## Store the functions defined above
        list(set = set, get = get,
                setInverse = setInverse,
                getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then cacheSolve retrieves the inverse from the 
## cache.
cacheSolve <- function(x, ...) {
        ## Determine if the inverse has already been computed and return the
        ## cached matrix if it has
        m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        ## Compute the inverse matrix
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m
}
