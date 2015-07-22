## These functions create the inverse of a matrix,
## caching the inverted matrix in order to reduce
## access time on subsequent accesses.
##
## makeCacheMatrix takes a matrix, assumed to be square
## and invertible, and creates an object which stores
## the original matrix, the inverse of that matrix, and
## creates accessor and settor functions to get and set
## both the original and the inverse.
##
## cacheSolve takes as an argument the object created
## by makeCacheMatrix, checks to see if the inverse
## has already been calculated, returning the cached
## value if it has and calculating and returning the inverse
## if it hasn't already been computed.

## makeCacheMatrix creates a list object
## that can set the value of a matrix, get the
## value of the matrix, set the inverse of that matrix
## and get the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
        
        ## This flag indicates whether the inverse has been computed.
        cachedInverse <- NULL
        
        ## set will store the original matrix and set the cached flag
        ## to indicate that the inverse has not been calculated yet
        set <- function(y) {
                x <<- y
                cachedInverse <<- NULL
        }
        
        ## get simply returns the original matrix
        get <- function() x
        
        ## setInverse will use the 
        setInverse <- function(inverse) cachedInverse <<- inverse
        
        ## getInverse calls cachedInverse to check whether the
        ## inverse has already been computed or if it needs to 
        ## be calculated, and then returns the inverse accordingly
        getInverse <- function() cachedInverse
        
        ## return a list object that encapsulates the original matrix
        ## and provides accessor and settor methods
        list(set = set, 
             get = get, 
             setInverse = setInverse, 
             getInverse = getInverse)
}


## cacheSolve computes the inverse of a
## matrix, using the object returned by makeCacheMatrix.  If the inverse
## has already been calculated, and the matrix has not
## changed, then cacheSolve simply returns the previously
## cached inverse.

cacheSolve <- function(x, ...) {

        inverseMatrix <- x$getInverse()
        if(!is.null(inverseMatrix)) {
                message("getting cached data")
                return(inverseMatrix)
        }
        originalMatrix <- x$get()
        
        ## I keep entering matrices that are not invertible.
        ## According to some research, det(<matrix>) = 0 indicates
        ## that the matrix is not invertible, so I'm adding
        ## that as a defensive check.  Return an empty matrix if
        ## this matrix is not invertible.
        if (det(originalMatrix) == 0) {
                message("matrix is not invertible")
                return(matrix())
        }
        
        inverseMatrix <- solve(originalMatrix, ...)
        x$setInverse(inverseMatrix)
        
        ## Return a matrix that is the inverse of 'x'
        ## Could simply return the value of the local variable
        ## "inverseMatrix", but this is a nice test of the accessor
        ## function.
        x$getInverse()
}
