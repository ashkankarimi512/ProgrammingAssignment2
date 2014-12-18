## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(xmat = matrix()) {
        matInv <- NULL
        set <- function(y) {
                xmat <<- y
                matInv <<- NULL
        }
        
        get <- function() xmat
        
        setInvMat <- function(y) matInv <<- y
        
        getInvMat <- function() matInv
        
        list(set = set, get = get, setInvMat = setInvMat, getInvMat = getInvMat)
}


## function: cacheSolve
##
## This function calculates the invese of the xmat or returns the cached Inverse
## it it has not been changed.
#
cacheSolve <- function(xmat, ...) {
        ## Return a matrix that is the inverse of 'xmat'
        matInv <- xmat$getInvMat()
        if ( !is.null(matInv)) {
                message("getting cached data")
                return(matInv)
        }
        
        data <- xmat$get()
        ## Assume the martrix has an inverse.
        ## TBD : check if the matrix has an inverst!
        
        matInv <- solve(data)
        xmat$setInvMat( matInv )
        matInv
}
