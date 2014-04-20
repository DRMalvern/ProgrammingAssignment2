## Combination of functions makeCacheMatrix and cacheSolve allows calculate
## inverse matrix only one times for each matrix. Second and next requests 
## for inverse matrix are answered from cache.This solution save time.


## Function makeCacheMatrix creates object that can cache inverse matrix
## after calculating for next use.
makeCacheMatrix <- function(x = matrix()) {
    ## Variable inverseMatrix have to be initialized by value NULL 
    ## (for first ask for result of inverting matrix is necessary). 
    inverseMatrix <- NULL
    
    ## Function setMatrix for reinitialize matrix and 
    ## set flag about not calculated inverse matrix (inverseMatrix <- NULL). 
    setMatrix <- function(y) {
        x <<- y
        inverseMatrix <<- NULL
    }
    
    ## Name of next functions describe what they do.
    getMatrix <- function() x
    setInverseMatrix <- function(im) inverseMatrix <<- im
    getInverseMatrix <- function() inverseMatrix
    list(setMatrix = setMatrix, getMatrix = getMatrix,
         setInverseMatrix = setInverseMatrix,
         getInverseMatrix = getInverseMatrix)
}


## Function cacheSolve first time calculate inverse matrix 
## and next time will use calculated value again. 
## Remember value is much faster than calculate again.
cacheSolve <- function(x, ...) {
    ##Receiving inverse matrix or NULL value. 
    im <- x$getInverseMatrix()
    
    ## Test whether it has been inverse matrix calculated
    ## (NULL==not calculated). 
    if(!is.null(im)) {
        message("getting cached data")
        return(im)
    }
    
    ##First calculation of inverse matrix and save result for next use.
    data <- x$getMatrix()
    im <- solve(data, ...)
    x$setInverseMatrix(im)
    
    ## Return a matrix that is the inverse of 'x'
    im
}
