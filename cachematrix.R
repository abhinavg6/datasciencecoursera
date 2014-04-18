## The pair of functions makeCacheMatrix and cacheSolve
## cache the inverse of a matrix, so that once inverse of
## a matrix is calculated, it's retrieved from the cache
## subsequently, but only until the main matrix is changed

## makeCacheMatrix function creates a special "matrix" object 
## that can cache its inverse.

makeCacheMatrix <- function(m = matrix()) {
    # Initialize symbol for cached inverse of matrix
    inv <- NULL
    # Initialize symbol to indicate matrix has changed
    matChanged <- TRUE
    
    # Set a matrix in the data structure
    setMatrix <- function(x) {
        m <<- x
        inv <<- NULL
        matChanged <<- TRUE
    }
    
    # Get the currently set matrix
    getMatrix <- function() m
    
    # Set the calculated inverse of a matrix, 
    # and indicate that the matrix is not changed
    setMatInverse <- function(inverse) {
        matChanged <<- FALSE
        inv <<- inverse
    }
    
    # Get the inverse of the matrix
    getMatInverse <- function() inv
    
    # Returns whether the matrix has changed or not
    isMatChanged <- function() matChanged
    
    # Return the list of functions that can be executed
    # on this data structure
    list(setMatrix = setMatrix, 
         getMatrix = getMatrix,
         setMatInverse = setMatInverse,
         getMatInverse = getMatInverse,
         isMatChanged = isMatChanged)
}


## cacheSolve function computes the inverse of the 
## special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the 
## matrix has not changed), then it retrieves the inverse 
## from the cache.

cacheSolve <- function(specialMatrix, ...) {
    # Get the cached inverse of matrix
    inv <- specialMatrix$getMatInverse()
    # Ask whether the matrix has changed or not
    isMChanged <- specialMatrix$isMatChanged()
    
    # If the cached inverse is not NULL and
    # if the matrix has not changed
    if(!is.null(inv) && !isMChanged) {
        message("Getting cached inverse of matrix")
        return(inv)
    }
    
    # Get the current matrix for which inverse 
    # has been requested
    m <- specialMatrix$getMatrix()
    
    # Calculate the inverse of matrix
    inv <- solve(m, ...)
    # Set the calculated inverse in data structure
    specialMatrix$setMatInverse(inv)
    
    message("Getting fresh calculated inverse of matrix")
    inv
}
