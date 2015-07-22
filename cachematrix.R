## These functions create the inverse of a matrix,
## caching the inverted matrix in order to reduce
## access time, if the inverted matrix has already
## been created.

## makeCacheMatrix creates a list object
## that can set the value of a matrix, get the
## value of the matrix, set the inverse of that matrix
## and get the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
        cachedInverse <- NULL
        set <- function(y) {
                x <<- y
                cachedInverse <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) cachedInverse <<- inverse
        getInverse <- function() cachedInverse
        list(set = set, 
             get = get, 
             setInverse = setInverse, 
             getInverse = getInverse)
}


## cacheSolve computes the inverse of a
## matrix returned by makeCacheMatrix.  If the inverse
## has already been calculated, and the matrix has not
## changed, then cacheSolve simply returns the previously
## cached inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverseMatrix <- x$getInverse()
        if(!is.null(inverseMatrix)) {
                message("getting cached data")
                return(inverseMatrix)
        }
        originalMatrix <- x$get()
        inverseMatrix <- solve(originalMatrix, ...)
        x$setInverse(inverseMatrix)
        x$getInverse()
}
