## The following pair of functions work in tandem to create 
## a system for solving the inverse of a matrix. It only  
## performs the actual computation the first time a given 
## matrix is presented to the solver. The first function 
## returns an object(list) capable of storing a matrix and 
## its inverse. The second function given an object of 
## this type is responsible for either calculating the 
## contained matrix's inverse or if present returning the 
## previously stored (cached) version. If this function 
## calculates the inverse it caches the result to return on 
## subsequent calls  

## Function returns a list of functions for storing and
## retrieving a matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
    matrixInverse <- NULL
    
    set <- function(y){
        x <<- y
        # Erase previous cached matrix when
        # setting new matrix
        matrixInverse <<- NULL
    }
    
    get <- function() x
    
    setInverse <- function(matrixInverse) {
        matrixInverse <<- matrixInverse
    }
    
    getInverse <- function() matrixInverse
    
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Given a list of the type returned by makeCacheMatrix,
## return the contained matrix's inverse if stored, otherwise
## compute and return the inverse.
cacheSolve <- function(x, ...) {
        matrixInverse <- x$getInverse()
        
        if(!is.null(matrixInverse)){
            message("Getting cached matrix inverse")
            return(matrixInverse)
        }
        
        matrix <- x$get()
        matrixInverse <- solve(matrix, ...)
        x$setInverse(matrixInverse)
        
        matrixInverse
}
